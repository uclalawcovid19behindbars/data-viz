library(tidyverse)
library(behindbarstools)

## read facility-level data
raw_dat <- behindbarstools::read_scrape_data(all_dates = TRUE, state = "Georgia")
ga_state <- raw_dat %>%
    filter(Jurisdiction == "state",
           Age != "Juvenile") %>%
    group_by(Facility.ID) %>%
    mutate(res_deaths_lag = dplyr::lag(Residents.Deaths, order_by = Date),
           res_active_df = diff_roll_sum(Residents.Confirmed, Date)) %>%
    ungroup() %>%
    mutate(res_active_dfr = res_active_df / Residents.Population,
           res_new_deaths = Residents.Deaths - res_deaths_lag,
           res_cfr =  res_new_deaths / res_active_df,
           ## on days when the death count dropped, make NA
           res_cfr = ifelse(res_new_deaths < 0, NA, res_cfr),
           res_cfr = ifelse(is.infinite(res_cfr), NA, res_cfr),
           res_cfr = ifelse(is.nan(res_cfr), NA, res_cfr)) 
write_csv(ga_state, "~/Desktop/ga_state.csv")

latest_ga <- ga_state %>%
    filter(Date == max(Date))
latest_ga$Residents.Initiated / latest_ga$Residents.Population

## read statewide data
historical_statewide <- read_csv("https://raw.githubusercontent.com/uclalawcovid19behindbars/data/master/historical-data/historical_state_counts.csv")
ga_prison_pop <- behindbarstools::read_mpap_pop_data() %>%
    filter(State == "Georgia") %>%
    pull(Population.Feb20)
ga_statewide <- historical_statewide %>%
    filter(State == "Georgia") %>%
    ## NB: denominator is feb 2020 prison population!
    mutate(res_active_df = diff_roll_sum(Residents.Confirmed, Date),
           res_deaths_lag = dplyr::lag(Residents.Deaths, order_by = Date),
           res_new_deaths = Residents.Deaths - res_deaths_lag,
           res_active_dfr = res_active_df / ga_prison_pop,
           res_cumulative_rate = Residents.Confirmed / ga_prison_pop,
           res_cfr = res_new_deaths / res_active_df ,
           res_cfr = ifelse(is.infinite(res_cfr), NA, res_cfr),
           # staff_cfr = diff_roll_sum(Staff.Confirmed, Date) / diff_roll_sum(Staff.Deaths, Date),
           # staff_cfr = ifelse(is.infinite(staff_cfr), NA, staff_cfr)
           ) %>%
    ## diff_roll_sum() still makes estimate from NA data
    filter(Date <= as.Date("2021-07-11"))
write_csv(ga_statewide, "~/Desktop/ga_statewide.csv")

## get general population data
ga_general <- behindbarstools::get_genstate_covid() %>%
    filter(State == "Georgia") %>%
    mutate(gen_active_df = diff_roll_sum(General.Confirmed),
           gen_active_dfr = gen_active_df / General.Population,
           gen_cumulative_rate = General.Confirmed / General.Population)
ga_general <- ga_general %>%
    mutate( gen_deaths_lag = dplyr::lag(General.Deaths, order_by = Date),
            gen_new_deaths = General.Deaths - gen_deaths_lag,
            gen_cfr = gen_new_deaths / gen_active_df ,
            ## on days when the death count dropped, make NA
            gen_cfr = ifelse(gen_new_deaths < 0, NA, gen_cfr),
            gen_cfr = ifelse(is.infinite(gen_cfr), NA, gen_cfr),
            gen_cfr = ifelse(is.nan(gen_cfr), NA, gen_cfr)) 
#write_csv(ga_general, "~/Desktop/ga_general.csv")

ga_statewide_df <- ga_general %>%
    left_join(ga_statewide, by = "Date")
write_csv(ga_statewide_df, "~/Desktop/ga_statewide_df.csv")

# PLOT: Statewide prison vs GA state pop - active  ------------------------------
ga_genpop_compare_active <- ga_statewide_df %>% 
    ggplot() + 
    ## blue = state pop
    geom_line(aes(x = Date, y = gen_active_dfr), size = 1.0, color = "#4C6788") +
    ## orange = prison 
    geom_line(
        data = ga_statewide %>% filter(!is.na(res_active_dfr)),
        aes(x = Date, y = res_active_dfr), size = 1.0, color = "#D7790F"
        ) + 
    theme_behindbars() + 
    theme(
        axis.ticks.y = element_line(color = "#555526"), 
        axis.title.y = element_blank(), 
        axis.line.y = element_line(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_blank()) + 
    geom_vline(xintercept = as.Date("2021-07-11"), size = 1.5, color = "#000000") + 
    scale_x_date(date_breaks = "2 month", date_labels =  "%b %y") + 
    scale_y_continuous(labels = scales::percent) 
ggsave("~/Desktop/ga_viz/ga_genpop_compare_active.png", ga_genpop_compare_active, width = 10, height = 8)
ggsave("~/Desktop/ga_viz/ga_genpop_compare_active.svg", ga_genpop_compare_active, width = 7, height = 5)

# PLOT: Statewide prison - CFR  ------------------------------
ga_cfr <- ga_statewide_df %>% 
    ggplot() + 
    ## blue = state pop
    # geom_line(aes(x = Date, y = gen_cfr), size = 1.0, color = "#4C6788") +
    ## orange = prison 
    geom_line(
        data = ga_statewide %>% filter(!is.na(res_cfr)),
        aes(x = Date, y = res_cfr), size = 1.0, color = "#D7790F"
    ) +
    geom_line(
        data = ga_statewide %>% filter(!is.na(res_active_dfr)),
        aes(x = Date, y = res_active_dfr), size = 1.0, color = "red"
    ) +     theme_behindbars() + 
    theme(
        axis.ticks.y = element_line(color = "#555526"), 
        axis.title.y = element_blank(), 
        axis.line.y = element_line(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_blank()) + 
    ## NO NEW COVID DEATHS REPORTED AFTER THIS POINT -- sus
    geom_vline(xintercept = as.Date("2021-03-21"), size = 1.5, color = "#000000") + 
    scale_x_date(date_breaks = "1 month", date_labels =  "%b %y") + 
    scale_y_continuous(labels = scales::percent) 
ggsave("~/Desktop/ga_viz/ga_cfr.png", ga_cfr, width = 10, height = 8)
ggsave("~/Desktop/ga_viz/ga_cfr.svg", ga_cfr, width = 7, height = 5)

# PLOT: Cumulative staff and res cases in GA prisons ----------------------

ga_cumulative <- ga_state %>% 
    # filter(Date <= "2021-06-13") %>% 
    group_by(Date) %>% 
    summarise(total_staff = sum_na_rm(Staff.Confirmed), 
              total_res = sum_na_rm(Residents.Confirmed)) %>% 
    ggplot(aes(x = Date)) + 
    geom_line(aes(y = total_staff), color = "#D7790F", size = 1.0) + 
    geom_line(aes(y = total_res), color = "#4C6788", size = 1.0) + 
    # scale_y_continuous(limits = c(0, 120)) +
    # scale_x_date(limits = c(as.Date("2021-04-15"), as.Date("2021-06-16"))) + 
    theme_behindbars(base_size = 18) + 
    # geom_vline(xintercept = as.Date("2021-06-13"), linetype = "dashed") + 
    labs(y = "Cumulative COVID-19 Cases in Georgia State Prisons")
ggsave("~/Desktop/ga_viz/ga_cumulative.png", ga_cumulative, width = 10, height = 8)
ggsave("~/Desktop/ga_viz/ga_cumulative.svg", ga_cumulative, width = 7, height = 5)

# Case Fatality Rate analysis ---------------------------------------------

## statewide

## facility-level
ga_state %>%
    select(Name, Date, Residents.Confirmed, res_active_df, res_active_dfr, 
           Residents.Deaths, res_new_deaths, res_cfr) %>%
    View()

# Outbreaks analysis ------------------------------------------------------
outbreaks_tab <- ga_state

# Facility-level outbreaks
## grab facilities with top 5 highest active case rates
top_dfr <- ga_state %>%
    group_by(Facility.ID) %>%
    arrange(-res_active_dfr) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    arrange(-res_active_dfr) %>%
    slice_head(n = 5) %>%
    pull(Name)
top_dfr %>% select(Name, Date, res_active_dfr ) %>% View()
    
peak_dfr <- c("GWINNETT COUNTY CORRECTIONAL INSTITUTION",
              "COWETA COUNTY CORRECTIONAL INSTITUTION",
              "WALKER STATE PRISON",
              )
ga_state %>%
    filter(Name == "COWETA COUNTY CORRECTIONAL INSTITUTION") %>%
    ggplot(aes(
        x = Date, y = res_active_dfr, color = Name, fill = Name)) +
    geom_line(size = 1.5) +
    geom_area(alpha = .5) +
    theme_behindbars() +
    scale_color_bbdiscrete() +
    scale_fill_bbdiscrete() +
    theme(legend.position = "none") +
    labs(y = "Estimated Active Case Rate") +
    ggtitle("Monitoring Facility Outbreaks")

# Deaths analysis ---------------------------------------------------------




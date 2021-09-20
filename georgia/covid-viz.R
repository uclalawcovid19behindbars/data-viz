library(tidyverse)
library(lubridate)
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
# write_csv(ga_state, "~/Desktop/ga_state.csv")

latest_ga <- ga_state %>%
    filter(Date == max(Date))
latest_ga$Residents.Initiated / latest_ga$Residents.Population

## read statewide prison data and create metrics for analysis 
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
           res_deathrate = Residents.Deaths / ga_prison_pop,
           res_newdeath_rate = res_new_deaths / ga_prison_pop,
           # staff_cfr = diff_roll_sum(Staff.Confirmed, Date) / diff_roll_sum(Staff.Deaths, Date),
           # staff_cfr = ifelse(is.infinite(staff_cfr), NA, staff_cfr)
           ) %>%
    ## diff_roll_sum() still makes estimate from NA data
    filter(Date <= as.Date("2021-07-12"))
# write_csv(ga_statewide, "~/Desktop/ga_statewide.csv")

## get general population data and create metrics for analysis 
ga_general <- behindbarstools::get_genstate_covid() %>%
    filter(State == "Georgia") 
ga_general <- ga_general %>%
    mutate( gen_active_df = diff_roll_sum(General.Confirmed),
            gen_active_dfr = gen_active_df / General.Population,
            gen_cumulative_rate = General.Confirmed / General.Population,
            gen_deaths_lag = dplyr::lag(General.Deaths, order_by = Date),
            gen_new_deaths = General.Deaths - gen_deaths_lag,
            gen_newdeath_rate = gen_new_deaths / General.Population,
            gen_cfr = gen_new_deaths / gen_active_df ,
            ## on days when the death count dropped, make NA
            gen_cfr = ifelse(gen_new_deaths < 0, NA, gen_cfr),
            gen_cfr = ifelse(is.infinite(gen_cfr), NA, gen_cfr),
            gen_cfr = ifelse(is.nan(gen_cfr), NA, gen_cfr),
            gen_deathrate = General.Deaths / General.Population) 
#write_csv(ga_general, "~/Desktop/ga_general.csv")

ga_statewide_df <- ga_general %>%
    left_join(ga_statewide, by = "Date") %>%
    mutate(week = lubridate::week(Date),
           year = lubridate::year(Date),
           month = lubridate::month(Date)) 
write_csv(ga_statewide_df, "~/Desktop/ga_statewide_df.csv")

## create monthly aggregate counts -- new cases, new deaths
monthly_df <- ga_statewide_df %>%
    group_by(month, year) %>%
    # arrange()
    summarise(gen_monthly_deaths = sum_na_rm(gen_new_deaths),
              res_monthly_deaths = sum_na_rm(res_new_deaths),
              gen_monthly_cases = sum_na_rm(gen_active_df),
              res_monthly_cases = sum_na_rm(res_active_df),
              Date = first(Date)) %>%
    ungroup() %>% 
    mutate(Date = floor_date(Date, "months"),
           General.Population = first(ga_general$General.Population),
           Residents.Population = ga_prison_pop,
           gen_monthly_deathrate = gen_monthly_deaths / General.Population,
           gen_monthly_caserate = gen_monthly_cases / General.Population,
           res_monthly_deathrate = res_monthly_deaths / Residents.Population,
           res_monthly_caserate = res_monthly_cases / Residents.Population)

# A: Statewide prison vs GA state pop - active  ------------------------------
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
    geom_vline(xintercept = as.Date("2021-07-12"), size = 1.5, color = "#000000") + 
    scale_x_date(date_breaks = "2 month", date_labels =  "%b %y") + 
    scale_y_continuous(labels = scales::percent) +
    labs(y = "Active COVID-19 Case Rate (estimated)",
         title = "Active COVID-19 case rate",
         subtitle = "GA statewide (blue) and state prison population (orange)",
         tag = "A")
ggsave("~/Desktop/ga_viz/ga_genpop_compare_active.png", ga_genpop_compare_active, width = 10, height = 8)
ggsave("~/Desktop/ga_viz/ga_genpop_compare_active.svg", ga_genpop_compare_active, width = 7, height = 5)

# A2: A state pop - active  ------------------------------
ga_genpop_active <- ga_statewide_df %>% 
    ggplot() + 
    ## blue = state pop
    geom_line(aes(x = Date, y = gen_active_dfr), size = 1.0, color = "#4C6788") +
    theme_behindbars() +
    theme(
        axis.ticks.y = element_line(color = "#555526"), 
        axis.title.y = element_blank(), 
        axis.line.y = element_line(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_blank()) + 
    geom_vline(xintercept = as.Date("2021-07-12"), size = 1.5, color = "#000000") + 
    scale_x_date(date_breaks = "2 month", date_labels =  "%b %y") + 
    scale_y_continuous(labels = scales::percent) +
    labs(y = "Active COVID-19 Case Rate (estimated)",
         title = "Active COVID-19 case rate",
         subtitle = "Georgia statewide",
         tag = "A2")
ggsave("~/Desktop/ga_viz/ga_genpop_active.png", ga_genpop_active, width = 10, height = 8)
ggsave("~/Desktop/ga_viz/ga_genpop_active.svg", ga_genpop_active, width = 7, height = 5)


# B: Statewide prison vs GA state pop - cumulative death rate  ------------------------------
ga_genpop_compare_deaths <- ga_statewide_df %>% 
    ggplot() + 
    ## blue = state pop
    geom_line(aes(x = Date, y = gen_deathrate), size = 1.0, color = "#4C6788") +
    ## orange = prison 
    geom_line(
        data = ga_statewide %>% filter(!is.na(res_deathrate)),
        aes(x = Date, y = res_deathrate), size = 1.0, color = "#D7790F"
    ) + 
    theme_behindbars() +
    theme(
        axis.ticks.y = element_line(color = "#555526"), 
        axis.title.y = element_blank(), 
        axis.line.y = element_line(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_blank()) + 
    geom_vline(xintercept = as.Date("2021-07-12"), size = 1.5, color = "#000000") + 
    scale_x_date(date_breaks = "2 month", date_labels =  "%b %y") + 
    scale_y_continuous(labels = scales::percent) +
    labs(y = "COVID-19 Cumulative Death Rate (estimated)",
         title = "COVID-19 cumulative death rate",
         subtitle = "GA statewide (blue) and state prison population (orange)",
         tag = "B")
ggsave("~/Desktop/ga_viz/ga_genpop_compare_deaths.png", ga_genpop_compare_deaths, width = 10, height = 8)
ggsave("~/Desktop/ga_viz/ga_genpop_compare_deaths.svg", ga_genpop_compare_deaths, width = 7, height = 5)

# C: Statewide prison vs GA state pop - monthly new death rate  ------------------------------
ga_genpop_compare_deaths_monthly <- monthly_df %>% 
    ggplot() + 
    ## blue = state pop
    geom_line(aes(x = Date, y = gen_monthly_deathrate), size = 1.0, color = "#4C6788") +
    ## orange = prison 
    geom_line(
        aes(x = Date, y = res_monthly_deathrate), size = 1.0, color = "#D7790F"
    ) + 
    theme_behindbars() +
    theme(
        axis.ticks.y = element_line(color = "#555526"), 
        axis.title.y = element_blank(), 
        axis.line.y = element_line(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_blank()) + 
    geom_vline(xintercept = as.Date("2021-07-12"), size = 1.5, color = "#000000") +
    geom_vline(xintercept = as.Date("2021-03-14"), size = 1.5, color = "#000000") +
    scale_x_date(date_breaks = "2 month", date_labels =  "%b %y") + 
    scale_y_continuous(labels = scales::percent) +
    labs(y = "COVID-19 Monthly Death Rate (estimated)",
         title = "COVID-19 monthly death rate",
         subtitle = "GA statewide (blue) and state prison population (orange)",
         tag = "C")
ggsave("~/Desktop/ga_viz/ga_genpop_compare_monthly_deaths.png", ga_genpop_compare_deaths_monthly, width = 10, height = 8)
ggsave("~/Desktop/ga_viz/ga_genpop_compare_monthly_deaths.svg", ga_genpop_compare_deaths_monthly, width = 7, height = 5)

# D: Statewide prison - CFR  ------------------------------
ga_cfr <- ga_statewide_df %>% 
    ggplot() + 
    ## blue = state pop
    geom_line(aes(x = Date, y = gen_cfr), size = 1.0, color = "#4C6788") +
    ## orange = prison 
    geom_line(
        data = ga_statewide %>% filter(!is.na(res_cfr)),
        aes(x = Date, y = res_cfr), size = 1.0, color = "#D7790F"
    ) +
    # geom_line(
    #     data = ga_statewide %>% filter(!is.na(res_active_dfr)),
    #     aes(x = Date, y = res_active_dfr), size = 1.0, color = "red"
    # ) +     
    theme_behindbars() + 
    theme(
        axis.ticks.y = element_line(color = "#555526"), 
        axis.title.y = element_blank(), 
        axis.line.y = element_line(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_blank()) + 
    ## NO NEW COVID DEATHS REPORTED AFTER THIS POINT -- sus
    geom_vline(xintercept = as.Date("2021-07-12"), size = 1.5, color = "#000000") +
    ## date dashboard was taken down 
    geom_vline(xintercept = as.Date("2021-03-14"), size = 1.5, color = "#000000") +
    scale_x_date(date_breaks = "1 month", date_labels =  "%b %y") + 
    scale_y_continuous(labels = scales::percent) + 
    labs(y = "COVID-19 Case Fatality Rate",
         title = "COVID-19 case fatality rate",
         subtitle = "GA statewide (blue) and state prison population (orange)",
         tag = "D")
ggsave("~/Desktop/ga_viz/ga_cfr.png", ga_cfr, width = 10, height = 8)
ggsave("~/Desktop/ga_viz/ga_cfr.svg", ga_cfr, width = 7, height = 5)

# Case Fatality Rate analysis ---------------------------------------------

## statewide

## facility-level
ga_state %>%
    select(Name, Date, Residents.Confirmed, res_active_df, res_active_dfr, 
           Residents.Deaths, res_new_deaths, res_cfr) %>%
    View()

# Outbreaks analysis ------------------------------------------------------
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
later_outbreaks <- ga_state %>%
    filter(Date > as.Date("2021-03-01")) %>%
    group_by(Facility.ID) %>%
    arrange(-res_active_dfr) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    arrange(-res_active_dfr) %>%
    slice_head(n = 2) %>%
    pull(Name)
facs_to_highlight <- c(top_dfr, later_outbreaks)

facility_active_caserate <- ga_state %>%
    filter(Name %in% facs_to_highlight,
           Name != "EFFINGHAM COUNTY CORRECTIONAL INSTITUTION",
           Name != "CARROLL COUNTY CORRECTIONAL INSTITUTION") %>%
    ggplot(aes(
        x = Date, y = res_active_dfr, color = Name, fill = Name)) +
    geom_line(size = 1.5) +
    # geom_area(alpha = .5) +
    theme_behindbars() +
    scale_color_bbdiscrete() +
    scale_fill_bbdiscrete() +
    theme(
        axis.ticks.y = element_line(color = "#555526"), 
        axis.title.y = element_blank(), 
        axis.line.y = element_line(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_blank()) + 
    ## NO NEW COVID DEATHS REPORTED AFTER THIS POINT -- sus
    geom_vline(xintercept = as.Date("2021-07-16"), size = 1.5, color = "#000000") +
    scale_x_date(date_breaks = "2 months", date_labels =  "%b %y") + 
    scale_y_continuous(labels = scales::percent) + 
    labs(y = "Estimated COVID-19 Active Case Rate",
         title = "Facility Outbreaks in Georgia State Prisons",
         subtitle = "Estimated COVID-19 Active Case Rate",
         tag = "E") + 
    theme(legend.position = "bottom")
ggsave("~/Desktop/ga_viz/facility_active_caserate.png", facility_active_caserate, width = 10, height = 8)
ggsave("~/Desktop/ga_viz/facility_active_caserate.svg", facility_active_caserate, width = 7, height = 5)

# Deaths analysis ---------------------------------------------------------
top_deaths <- ga_state %>%
    group_by(Facility.ID) %>%
    arrange(-Residents.Deaths) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    arrange(-Residents.Deaths) %>%
    slice_head(n = 5) %>%
    pull(Name)

facility_deaths <- ga_state %>%
    filter(Name %in% top_deaths) %>%
    ggplot(aes(
        x = Date, y = res_new_deaths, color = Name, fill = Name)) +
    geom_line(size = 1.5) +
    # geom_area(alpha = .5) +
    theme_behindbars() +
    scale_color_bbdiscrete() +
    scale_fill_bbdiscrete() +
    theme(
        axis.ticks.y = element_line(color = "#555526"), 
        axis.title.y = element_blank(), 
        axis.line.y = element_line(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_blank()) + 
    ## NO NEW COVID DEATHS REPORTED AFTER THIS POINT -- sus
    geom_vline(xintercept = as.Date("2021-07-16"), size = 1.5, color = "#000000") +
    scale_x_date(date_breaks = "2 months", date_labels =  "%b %y") + 
    scale_y_continuous(limits = c(0, 4)) + 
    labs(title = "Deaths in Georgia State Prisons",
         subtitle = "COVID-19 Deaths Among Incarcerated People",
         tag = "F") + 
    theme(legend.position = "none")
ggsave("~/Desktop/ga_viz/facility_deaths.png", facility_deaths, width = 10, height = 8)
ggsave("~/Desktop/ga_viz/facility_deaths.svg", facility_deaths, width = 7, height = 5)


# Case fatality rate analysis ---------------------------------------------------------

## note to self: this is not super helpful because what is jumping to the top 
#                are facilities with 100% case fatality rate (tho that could be important to highlight)
# Facility-level outbreaks
## grab facilities with top 5 highest active case rates
top_cfr <- ga_state %>%
    group_by(Facility.ID) %>%
    arrange(-res_cfr) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    arrange(-res_cfr) %>%
    slice_head(n = 5) %>%
    pull(Name)

facility_casefatalityrate <- ga_state %>%
    filter(Name %in% top_cfr) %>%
    ggplot(aes(
        x = Date, y = res_cfr, color = Name, fill = Name)) +
    geom_line(size = 1.5) +
    # geom_area(alpha = .5) +
    theme_behindbars() +
    # scale_color_bbdiscrete() +
    # scale_fill_bbdiscrete() +
    theme(
        axis.ticks.y = element_line(color = "#555526"), 
        axis.title.y = element_blank(), 
        axis.line.y = element_line(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_blank()) + 
    ## NO NEW COVID DEATHS REPORTED AFTER THIS POINT -- sus
    geom_vline(xintercept = as.Date("2021-07-16"), size = 1.5, color = "#000000") +
    scale_x_date(date_breaks = "2 months", date_labels =  "%b %y") + 
    scale_y_continuous(labels = scales::percent) + 
    labs(y = "Estimated COVID-19 Case Fatality Rate",
         title = "Facility Outbreaks in Georgia State Prisons",
         subtitle = "Estimated COVID-19 Case Fatality Rate",
         tag = "") + 
    theme(legend.position = "none")
ggsave("~/Desktop/ga_viz/facility_casefatalityrate.png", facility_casefatalityrate, width = 10, height = 8)
ggsave("~/Desktop/ga_viz/facility_casefatalityrate.svg", facility_casefatalityrate, width = 7, height = 5)


#########################
# unused ---------------------------------------------------------
#########################
# PLOT: Statewide prison vs GA state pop - daily death rate  ------------------------------
ga_genpop_compare_deaths_daily <- ga_statewide_df %>% 
    ggplot() + 
    ## blue = state pop
    geom_line(aes(x = Date, y = gen_newdeath_rate), size = 1.0, color = "#4C6788") +
    ## orange = prison 
    geom_line(
        data = ga_statewide %>% filter(!is.na(res_newdeath_rate)),
        aes(x = Date, y = res_newdeath_rate), size = 1.0, color = "#D7790F"
    ) + 
    theme_behindbars() +
    theme(
        axis.ticks.y = element_line(color = "#555526"), 
        axis.title.y = element_blank(), 
        axis.line.y = element_line(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_blank()) + 
    geom_vline(xintercept = as.Date("2021-07-12"), size = 1.5, color = "#000000") + 
    scale_x_date(date_breaks = "2 month", date_labels =  "%b %y") + 
    scale_y_continuous(labels = scales::percent) +
    labs(y = "COVID-19 Daily Death Rate (estimated)",
         title = "COVID-19 daily death rate",
         subtitle = "GA statewide (blue) and state prison population (orange)",
         tag = "")

# PLOT: statewide prison vs GA state pop - monthly new case rate  ------------------------------
ga_genpop_compare_deaths_monthly <- monthly_df %>% 
    ggplot() + 
    ## blue = state pop
    geom_line(aes(x = Date, y = gen_monthly_caserate), size = 1.0, color = "#4C6788") +
    ## orange = prison 
    geom_line(
        aes(x = Date, y = res_monthly_caserate), size = 1.0, color = "#D7790F"
    ) + 
    theme_behindbars() +
    theme(
        axis.ticks.y = element_line(color = "#555526"), 
        axis.title.y = element_blank(), 
        axis.line.y = element_line(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_blank()) + 
    geom_vline(xintercept = as.Date("2021-07-12"), size = 1.5, color = "#000000") +
    scale_x_date(date_breaks = "2 month", date_labels =  "%b %y") + 
    scale_y_continuous(labels = scales::percent) +
    labs(y = "COVID-19 Monthly Case Rate (estimated)",
         title = "COVID-19 monthly case rate",
         subtitle = "GA statewide (blue) and state prison population (orange)")

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

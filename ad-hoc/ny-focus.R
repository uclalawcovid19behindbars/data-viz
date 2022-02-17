library(tidyverse)
library(behindbarstools)
library(skimr)

scrape_df <- read_scrape_data(all_dates = TRUE, state = "New York")
scrape_df <- scrape_df %>%
    filter(Jurisdiction == "state")
num_fac = 10
date <- Sys.Date()
state_df <- read_csv("https://raw.githubusercontent.com/uclalawcovid19behindbars/data/master/historical-data/historical_state_counts.csv")
latest_scrape_date <-  max(scrape_df$Date)
# delta_start_date <- latest_scrape_date - lubridate::days(delta_days)
delta_start_date <- "2021-12-01"

run_ny_analysis <- function(scrape_df, metric) {
    fac_data <- scrape_df %>%
        filter(!(stringr::str_detect(Name, "(?i)state") & stringr::str_detect(Name, "(?i)wide"))) %>%
        filter(Date >= delta_start_date) %>%
        group_by(Name, State) %>%
        mutate(start_val = first(!!sym(metric)),
               last_val = last(!!sym(metric)),
               raw_change = last_val - start_val,
               pct_increase = (raw_change / start_val)*100) %>%
        filter(Date == max(Date)) %>%
        distinct(Facility.ID, Name, State, start_val, last_val, raw_change, pct_increase, Population.Feb20, Capacity) %>% 
        filter(raw_change > 0) 
    keep_facs_pct_increase <- fac_data %>%
        arrange(desc(pct_increase), Name) %>% 
        mutate(metric_arrange = "pct_increase") %>% 
        head(num_fac) 
    keep_facs_raw_increase <- fac_data %>%
        arrange(desc(raw_change), Name) %>% 
        mutate(metric_arrange = "raw_increase") %>% 
        head(num_fac) 
    keep_facs <- keep_facs_pct_increase %>%
        bind_rows(keep_facs_raw_increase) %>%
        distinct(Name, State, .keep_all = TRUE) %>%
        mutate(Name = str_to_title(Name),
               perc_full = (Population.Feb20 / Capacity)*100)
    # ## do the same for state
    state_data <- state_df %>%
        filter(State == "New York") %>% 
        filter(Date >= delta_start_date) %>%
        group_by(State) %>%
        mutate(start_val = first(!!sym(metric)),
               last_val = last(!!sym(metric)),
               raw_change = last_val - start_val,
               pct_increase = (raw_change / start_val)*100) %>%
        distinct(State, start_val, last_val, raw_change, pct_increase) %>%
        filter(raw_change > 0)
    out <- list(var = metric,
                facs = keep_facs,
                state = state_data)
    return(out)
}

### examine results

run_ny_analysis(scrape_df, metric = "Residents.Confirmed" )

metrics <- c("Residents.Confirmed", "Staff.Confirmed", "Residents.Deaths", "Staff.Deaths")
out <- metrics %>%
    map(~ run_ny_analysis(metric = .x, 
                          scrape_df = scrape_df))

### check greene CC

greene <- scrape_df %>%
    filter(Name == "GREENE CORRECTIONAL FACILITY") %>%
    mutate(active_df = diff_roll_sum(Residents.Confirmed))
greene %>% select(Date, active_df) %>% View()
plot(greene$Date, greene$active_df)

## overcrowding? 
## NB: though saved locally, this data file was generated via 
## https://github.com/uclalawcovid19behindbars/covid19_behind_bars_scrapers/blob/master/production/historical_scrape/historical_scrapers/historical_new_york_population.R
pop_df <- read_csv("~/UCLA/misc-data/NY_historical_pop_20210801.csv")

ny_pop_out <- scrape_df %>% 
    group_by(Name) %>%
    filter(Date == max(Date)) %>%
    ungroup() %>%
    select(Date, Facility.ID, Capacity) %>%
    left_join(pop_df, by = "Facility.ID") %>%
    # select(Date, Name, Population.Feb20, Residents.Population, Capacity) %>%
    mutate(perc_full = Residents.Population / Capacity,
           over_80 = ifelse(perc_full > .79, 1, 0)) %>%
    filter(!is.na(perc_full))

table(ny_pop_out$over_80)

## data viz ----------------------------------------------------------------
## cases in prisons
plotting_df <- scrape_df %>%
    filter(Date > "2021-01-01") %>%
    filter(Facility.ID %in% c(941,937, 914, 906, 908)) %>%
    group_by(Facility.ID) %>%
    mutate(Res.Act.Est = diff_roll_sum(Residents.Confirmed, Date)) %>%
    mutate(rollavg = zoo::rollmean(Res.Act.Est, k = 3, fill = NA)) %>% 
    mutate(rollavg = ifelse(rollavg < 0, 0, rollavg)) %>%
    ungroup() %>%
    mutate(Name = str_replace(Name, "CORRECTIONAL FACILITY", "CF"),
           my_label = glue("{Name}-{Facility.ID}"))

plotting_df %>%
    filter(Date > "2021-11-15") %>% 
    ggplot() + 
    geom_line(aes(x = Date, y = rollavg, color = Name), size = 1) + 
    theme_behindbars(base_size = 14,
                     base_color = "black") + 
    theme(legend.title = element_blank(),
          legend.position = "top") +
    scale_color_bbdiscrete() +
    labs(title = "Omicron in New York State Prisons",
         y = "Estimated Active COVID cases\namong incarcerated people",
         x = "")
ggsave("ny_prisons_omicron.svg", width = 9, height = 7)
ggsave("ny_prisons_omicron.png", width = 9, height = 7)

## state-wide staff cases
scrape_df %>%
    filter(Name == "STATEWIDE") %>%
    mutate(Staff.Act.Est = diff_roll_sum(Staff.Confirmed, Date)) %>%
    mutate(rollavg = zoo::rollmean(Staff.Act.Est, k = 5, fill = NA)) %>% 
    mutate(rollavg = ifelse(rollavg < 0, 0, rollavg)) %>%
    ggplot(aes(
        x = Date, y = rollavg, color = Name, fill = Name)) +
    geom_line(size = 1.5) +
    geom_area(alpha = .5) +
    theme_behindbars(base_size = 14, 
                     base_color = "black") + 
    scale_color_bbdiscrete() +
    scale_fill_bbdiscrete() +
    theme(legend.position = "none") +
    labs(y = "Staff Estimated\nCOVID Cases") +    
    ggtitle("Over 4,400 new cases among DOCCS staff since December 1, 2021")
ggsave("ny_staff_omicron.svg", width = 9, height = 7)
ggsave("ny_staff_omicron.png", width = 9, height = 7)
    

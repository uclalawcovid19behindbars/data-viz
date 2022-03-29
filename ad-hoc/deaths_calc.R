library(behindbarstools)
library(tidyverse)
library(plotly)
Sys.Date() #[1] "2022-03-01"

## TO DO - look into florida, april 11 2021 n_deaths = 430 then drops down to ~200

## read data
root <- "https://raw.githubusercontent.com/uclalawcovid19behindbars/data/master/historical-data/"
ntl_historical <- read_csv(str_c(root, "historical_national_counts.csv"))
state_jur_historical <- read_csv(str_c(root, "historical_state_jurisdiction_counts.csv"))
state_historical <- read_csv(str_c(root, "historical_state_counts.csv"))

## FIRST LOOK AT THE ISSUE
state_jur_death_sum <- state_jur_historical %>%
    filter(Measure == "Residents.Deaths",
           Web.Group == "Prison" ) %>%
    group_by(Date, Web.Group) %>%
    summarise(sum_deaths = sum_na_rm(Val))
plot_ly(data = state_jur_death_sum, x = ~Date, y = ~sum_deaths, color = ~Web.Group)

## 0 - JUST LOOK AT THE THE TWO DATES THAT CHANGED
## drop: between june 20 and june 27 2021
## the states impacted are TX, IL, and AR
state_jur_drop <- state_jur_historical %>%
    filter(Measure == "Residents.Deaths",
           Web.Group == "Prison",
           Date %in% c(as.Date("2021-06-20"), as.Date("2021-06-27"))) %>%
    select(-Rate) %>%
    pivot_wider(names_from = Date, values_from = Val) %>%
    mutate(alt_june_27 = ifelse(is.na(`2021-06-27`), 0, `2021-06-27`),
            value_change = `2021-06-27` - `2021-06-20`,
           value_change_with_na = alt_june_27 - `2021-06-20`,
           big_change = value_change > 10)

## 1 - LOOK AT STATES THAT DROPPED in June '21
# AR: no more data after 6/20/21
# IL: data reporting gap between 6/20/21 and 8/15/21
# TX: data reporting gap between 6/20/21 and 9/5/21
states_dropped_hist <- state_jur_historical %>%
    filter(Measure == "Residents.Deaths",
           Web.Group == "Prison",
           State %in% c("Texas", "Illinois", "Arkansas")) %>%
    select(-Rate)
plot_ly(data = states_dropped_hist, x = ~Date, y = ~Val, color = ~State)

## 2 - LOOK AT STATES THAT GREW in September '21
## TX: 100% of the jump comes from TX, where count rose from NA to 292
## notable that the number of deaths in NJ fell from 105 to 52 (why?)
state_jur_rise <- state_jur_historical %>%
    filter(Measure == "Residents.Deaths",
           Web.Group == "Prison",
           Date %in% c(as.Date("2021-08-29"), as.Date("2021-09-05"))) %>%
    select(-Rate) %>%
    pivot_wider(names_from = Date, values_from = Val) %>%
    mutate(alt_aug_count = ifelse(is.na(`2021-08-29`), 0, `2021-08-29`),
           value_change = `2021-09-05` - `2021-08-29`,
           value_change_with_na = `2021-09-05` - alt_aug_count,
           big_change = value_change > 10)

library(behindbarstools)
library(tidyverse)
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

## 1 - using stat jur: calc the number of new deaths nationally since summer 2021
## NB: noticed a big drop in deaths (~400) among state prisons between 6/20/21 and 6/27/21
state_jur_deaths <- state_jur_historical %>%
    filter(Measure == "Residents.Deaths") %>%
    select(-Rate) %>% 
    group_by(State, Web.Group) %>%
    mutate(prev_deaths = lag(Val, order_by = Date)) %>%
    ungroup() %>%
    mutate(change_deaths = Val - prev_deaths,
           neg_change = change_deaths < 0,
           change_gr_10 = abs(change_deaths) > 9,
           neg_change_10 = neg_change & change_gr_10) #%>% 
    # summarise(sum_deaths = sum_na_rm(Val))

## Isolate to a certain date range: 
## between June 20th and June 27th, 2021, the number of deaths among incarcerated  people

states_dropped_june_21 <- state_jur_deaths %>%
    # filter(neg_change_10) %>%
    filter(neg_change) %>%
    filter(Date > as.Date("2021-06-15"),
           Date < as.Date("2021-07-01"))

## was the web grouping changed in this date range?
## this seems to have gotten "fixed" on september 5th the web grouping seems to have gotten fine again

View(states_dropped_june_21)

states_dropped_june_21 %>%
    select(State) %>%
    unique() %>%
    pull(State)

plot_ly(data = state_jur_deaths, x = ~Date, y = ~sum_deaths, color = ~Web.Group)

## 2- using nlt historical: calc the number of new deaths nationally since summer 2021
deaths <- ntl_historical %>%
    filter(Measure == "Residents.Deaths")
plot_ly(data = deaths, x = ~Date, y = ~Count)
deaths %>%
    filter(Date == as.Date("2021-05-30") | 
               Date == max(Date)) 

## count 47 new deaths nationally, but that def can't be right nvm 

#    Date       Measure             Count       Reporting      Missing                                                      
# 2021-05-30  Residents.Deaths       2712        52 "Maine"                                                      
# 2022-02-27  Residents.Deaths       2759        46 "Arkansas, Georgia, Louisiana, Maine, Massachusetts\n Missis…

## 3 - calc the number of new deaths in BOP since summer 2021
state_historical %>%
    filter(State == "Federal") %>%
    filter(Date == as.Date("2021-05-30") | 
               Date == max(Date)) %>%
    select(Date, Residents.Deaths)
## count 38 new deaths since may 30 2021

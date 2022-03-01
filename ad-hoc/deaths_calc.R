library(behindbarstools)
library(tidyverse)
Sys.Date() #[1] "2022-03-01"

## read data
root <- "https://raw.githubusercontent.com/uclalawcovid19behindbars/data/master/historical-data/"
ntl_historical <- read_csv(str_c(root, "historical_national_counts.csv"))
state_jur_historical <- read_csv(str_c(root, "historical_state_jurisdiction_counts.csv"))
state_historical <- read_csv(str_c(root, "historical_state_counts.csv"))

## 1 - using stat jur: calc the number of new deaths nationally since summer 2021
## NB: noticed a big drop in deaths (~400) among state prisons between 6/20/21 and 6/27/21
state_jur_deaths <- state_jur_historical %>%
    filter(Measure == "Residents.Deaths") %>%
    group_by(Date, Web.Group) %>%
    summarise(sum_deaths = sum_na_rm(Val))
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

library(tidyverse)
library(lubridate)
library(behindbarstools)

## read facility-level data
raw_dat <- behindbarstools::read_scrape_data(all_dates = TRUE, state = "Georgia")
ga_state <- raw_dat %>%
    filter(Jurisdiction == "state",
           !Age %in% c("Juvenile")) %>%
    group_by(Facility.ID) %>%
    mutate(res_deaths_lag = dplyr::lag(Residents.Deaths, order_by = Date),
           res_active_df = diff_roll_sum(Residents.Confirmed, Date),
           res_active_dfr = res_active_df / Residents.Population,
           staff_active_df = diff_roll_sum(Staff.Confirmed, Date))  %>%
    ungroup()
write_csv(ga_state, "georgia/data/ga_historical_facs.csv")

## analysis done in python, here: 
## https://colab.research.google.com/drive/1EiBxVJ5xEomMOE5To8vU74ujdMc4F-oM#scrollTo=GE0hRuju5eL- 
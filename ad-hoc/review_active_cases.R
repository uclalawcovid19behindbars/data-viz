rm(list=ls())
library(tidyverse)
library(behindbarstools)
library(lubridate)

all_data <- read_scrape_data(all_dates = T)

all_active <- all_data %>%
    # look at all jurisdictions
    # only look at these three variables
    select(Facility.ID, Date, Residents.Active) %>%
    # remove anything with missing data
    na.omit() %>%
    # for each date floor to the nearest month
    mutate(Month = floor_date(Date, unit = "month")) %>%
    # group by each facility and each month
    group_by(Facility.ID, Month) %>%
    # only get the first monthly observation for each facility
    arrange(Facility.ID, Date) %>%
    filter(1:n() == 1) %>%
    # only get data that fall within these date ranges
    filter(Month >= ymd("2021-04-01")) %>%
    filter(Month <= ymd("2022-01-01")) %>%
    # only keep facilities that have 10 months of observations
    group_by(Facility.ID) %>%
    mutate(nmonth = n()) %>%
    filter(nmonth == 10)

# of the 915 facilities which have been reporting data on active cases
# here are the number of active cases around the start of the month
all_active %>%
    group_by(Month) %>%
    summarize(Residents.Active = sum(Residents.Active)) %>%
    ggplot(aes(x=Month, y = Residents.Active)) +
    geom_col(fill = "#D7790F", alpha = .95) +
    theme_behindbars() +
    labs(y="Incarcerated People with\nActive Covid Cases")


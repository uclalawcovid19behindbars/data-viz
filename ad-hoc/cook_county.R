library(behindbarstools)
library(tidyverse)

il_dat <- read_scrape_data(all_dates = TRUE, state = "Illinois")
cook_county <- il_dat %>%
    filter(Facility.ID == 454)

plot(cook_county$Date, cook_county$Residents.Confirmed)

cook_county %>%
    select(Date, Residents.Confirmed, Staff.Confirmed, Residents.Deaths, Staff.Deaths) %>%
    arrange(Date) %>%
    write_csv("~/Desktop/cook_county_covid.csv")
   # View()
    
library(behindbarstools)
library(tidyverse)

il_dat <- read_scrape_data(all_dates = TRUE, state = "Illinois")
cook_county <- il_dat %>%
    filter(Facility.ID == 454)

plot(cook_county$Date, cook_county$Residents.Confirmed)
    
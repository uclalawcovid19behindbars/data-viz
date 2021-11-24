library(tidyverse)
library(behindbarstools)

all_dates <- read_scrape_data(all_dates = TRUE)

mo <- all_dates %>% 
    filter(Jurisdiction == "state",
           State == "Missouri")

plot_recent_fac_increases(mo, metric = "Residents.Active", plot_days = 120, num_fac = 3)

plot_recent_fac_increases(mo, metric = "Residents.Confirmed", plot_days = 30, num_fac = 2)

state_df <- calc_aggregate_counts(all_dates = TRUE, state = TRUE) 

mo_state <- state_df %>%
    filter(State == "Missouri") %>%
    filter(Measure == "Residents.Active")

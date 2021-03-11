library(tidyverse)
library(behindbarstools)
library(lubridate)
library(glue)

# Load data 
fl <- read_scrape_data(all_dates = TRUE, state = "Florida")

fl_out <- fl %>%
  filter(Jurisdiction == "state")

fl_out %>% 
  count(Facility.ID, Date)

fl_summarize <- fl_out %>%
  mutate(week = week(Date),
         year = year(Date),
         week_yr = glue('{week}_{year}')) %>%
  group_by(week_yr) %>%
  summarise(sum_infections = sum_na_rm(Residents.Confirmed),
            med_infections = mean(Residents.Confirmed, na.rm = TRUE),
            start_date = first(Date),
            n_facs = n()) %>%
  ungroup() %>%
  # group_by(name) %>%
  mutate(before_count = lag(sum_infections, order_by=start_date),
         change.weekly_infections = sum_infections - before_count) %>%
  arrange(start_date)

plt <- fl_summarize %>% 
  ggplot(., aes(x=start_date, y = change.weekly_infections)) +
  geom_line() + 
  geom_smooth(se = FALSE) + 
  geom_vline(xintercept = as.Date("2020-09-16"), linetype = 4) 
  

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
  group_by(Facility.ID, week_yr) %>%
  summarise(sum_infections = sum_na_rm(Residents.Confirmed),
            mean_infections = mean(Residents.Confirmed, na.rm = TRUE),
            start_date = first(Date),
            n_facs = n()) %>%
  ungroup() %>%
  mutate(before_count = lag(sum_infections, order_by=start_date),
         change.weekly_infections = sum_infections - before_count) %>%
  arrange(start_date)

plt <- fl_summarize %>% 
  ggplot(., aes(x=start_date, y = change.weekly_infections)) +
  geom_line() + 
  geom_smooth(se = FALSE) + 
  geom_vline(xintercept = as.Date("2020-09-16"), linetype = 4) 


fl_by_fac <- fl_out %>%
  group_by(Facility.ID) %>%
  mutate(before_count = lag(Residents.Confirmed, order_by=Date),
         change.reported_infections = Residents.Confirmed - before_count) %>%
  arrange(Date) %>%
  ungroup()


plt2 <- fl_by_fac %>% 
  ggplot(., aes(x=Date, y = change.reported_infections, group = Facility.ID, color = Facility.ID)) +
  geom_line(alpha = 0.5) + 
  scale_x_date(date_breaks = "1 months", date_labels = "%m/%y") + 
  geom_vline(xintercept = as.Date("2020-09-16"), linetype = 4) 

ggsave("fl_newcases.png", plt2, width = 16, height = 10)


  
  

library(behindbarstools)
library(ggplot2)
library(tidyverse)

# read in data
latest_scraped = read_scrape_data()

# number of state facilities in each state
total_state_facilities = latest_scraped %>%
  filter(Jurisdiction == "state",
         Name != "STATEWIDE") %>%
  count(State)

# number of state facilities reporting vax data in each state
num_facilities_reporting = latest_scraped %>%
  filter(!is.na(Residents.Initiated), 
         Jurisdiction == "state",
         Name != "STATEWIDE") %>%
  count(State)

# merge on State and create new variable for percentage of facilities reporting
results = merge(num_facilities_reporting, total_state_facilities, 
                by = "State", 
                suffixes = c("_reporting", "_total"))
results$percentage = results$n_reporting/results$n_total*100


# plot number reporting
plt_num_reporting = ggplot(results, aes(x = State, y = n_reporting)) + 
  geom_bar(aes(fill = n_reporting), stat = "identity") + 
  labs(title = "Only 7 States Are Currently Releasing Any Facility Level Vaccination Data",
       subtitle = "Number of Facilities Reporting By State",
       x = "",
       y = "",
       caption = "*Data as of March 24, 2021") + 
  theme_behindbars() + 
  theme(text = element_text(size=12)) +
  scale_fill_gradient(low = "#82CAA4", high = "#D7790F", na.value = NA, name = "")

# plot percent reporting
plt_percent_reporting = ggplot(results, aes(x = State, y = percentage)) + 
  geom_bar(aes(fill = percentage), stat = "identity") + 
  labs(title = "Only 7 States Are Currently Releasing Any Facility Level Vaccination Data",
       subtitle = "Percentage Facilities Reporting By State",
       x = "",
       y = "",
       caption = "*Data as of March 24, 2021") + 
  theme_behindbars() + 
  theme(text = element_text(size=12)) +
  scale_fill_gradient(low = "#82CAA4", high = "#D7790F", na.value = NA, name = "")

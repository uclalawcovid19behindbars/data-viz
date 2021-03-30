library(behindbarstools)
library(ggplot2)
library(tidyverse)

# number of facilities reporting vax data in each state
latest_scraped = read_scrape_data()

latest_scraped %>%
  filter(!is.na(Residents.Initiated)) %>%
  filter(Name != "STATEWIDE") %>%
  count(State)

vaccines = latest_scraped %>%
  filter(!is.na(Residents.Initiated))

# percentage of Residents.Initiated for each vax reporting facility
scrape_wa <- read_scrape_data(all_dates = TRUE, state = "Washington")

wa_vax = scrape_wa %>% 
  filter(Jurisdiction == "state") %>% 
  group_by(Name, Date) %>% 
  summarise(res_vax = sum_na_rm(Residents.Initiated), 
            pop = Population.Feb20) %>%
  filter(!is.na(res_vax)) %>%
  mutate(percent_vax = round(digits = 2, (res_vax/pop)*100)) %>%
  arrange(desc(percent_vax))

ggplot(wa_vax, aes(x = Name, y = percent_vax)) +
  geom_bar(aes(fill = percent_vax), stat = "identity") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 100)) + 
  labs(title = "Percent of Incarcerated People in Washington State Prisons Who Have Received Vaccine", 
       x = "",
       y = "Percentage Vaccinated", 
       caption = "*Not All Facilities Reporting | As of March 19, 2021 | Max Percentage = 56.03%") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1), 
                 plot.tag.position = c(0.80, 0.05)) +
  theme_behindbars() + 
  theme(text = element_text(size=12)) +
  scale_fill_gradient(low = "#82CAA4", high = "#D7790F", na.value = NA) +
  coord_flip()

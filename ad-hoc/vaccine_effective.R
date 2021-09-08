library(tidyverse)
library(behindbarstools)

scrape_df <- read_scrape_data(all_dates = TRUE, window_pop = 200)

prea_xwalk <- read.csv(str_c(
    "https://raw.githubusercontent.com/uclalawcovid19behindbars/", 
    "facility_data/master/data/prea_crosswalk.csv"))

diff_df <- scrape_df %>% 
    filter(Web.Group == "Prison") %>% 
    filter(State %in% c("Alabama", "California", "Kansas", "Minnesota", "Pennsylvania", "Wisconsin")) %>%
    filter(Date %in% c(as.Date("2021-06-02"), as.Date("2021-09-03"))) %>% 
    group_by(Facility.ID) %>% 
    mutate(Residents.New = Residents.Confirmed - lag(Residents.Confirmed), 
           Staff.New = Staff.Confirmed - lag(Staff.Confirmed)) %>% 
    filter(!is.na(Staff.New) & !is.na(Residents.New)) %>% 
    select(Facility.ID, State, Name, Date, 
           Residents.Confirmed, Staff.Confirmed,
           Residents.New, Staff.New, 
           Residents.Initiated, Staff.Initiated, 
           Residents.Population, Staff.Population) %>% 
    left_join(prea_xwalk %>% select(Facility.ID, PREA.Staff.Population), 
              by = "Facility.ID") %>% 
    mutate(Staff.Population = coalesce(Staff.Population, PREA.Staff.Population)) %>% 
    mutate(Residents.New.Pct = Residents.New / Residents.Population * 10000, 
           Residents.Vax.Pct = Residents.Initiated / Residents.Population, 
           Staff.New.Pct = Staff.New / Staff.Population * 10000, 
           Staff.Vax.Pct = Staff.Initiated / Staff.Population)

p <- diff_df %>% 
    filter(!is.na(Residents.New.Pct) & !is.na(Residents.Vax.Pct)) %>% 
    ggplot(aes(y = Residents.Vax.Pct, x = Residents.New.Pct, size = Residents.Population)) + 
    geom_smooth(color = "black", size = 0.5) + 
    geom_point(alpha = 0.7, shape = 21, fill = "black", color = "white") + 
    scale_x_continuous(trans = "log", 
                       breaks = c(5, 50, 200, 500, 1000, 2000), 
                       labels = scales::comma_format(accuracy = 1)) +
    scale_y_continuous(limits = c(0, 1), 
                       labels = scales::percent) + 
    theme_behindbars(base_size = 16, base_color = "black") + 
    scale_size_continuous(labels = scales::comma_format(accuracy = 1)) + 
    labs(x = "New Cases per 10k (Log-Transformed)", 
         y = "Vaccination Rate Among Incarcerated People", 
         size = "Incarcerated\nPopulation") + 
    theme(axis.title.x = element_text(margin = margin(t = 1.1 * 16)))

ggsave("out2.svg", p, width = 8.5, height = 5.5)

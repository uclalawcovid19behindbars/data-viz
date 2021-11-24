library(tidyverse)
library(behindbarstools)
library(ggalt)

state_df <- read_csv(stringr::str_c(
    "https://raw.githubusercontent.com/uclalawcovid19behindbars/data/master/", 
    "historical-data/historical_state_counts.csv"
))

staff_pop <- read_csv(stringr::str_c(
    "https://raw.githubusercontent.com/themarshallproject/COVID_prison_data/master/", 
    "data/staff_populations.csv")) %>%
    filter(month == "july")

ca_staff_vax <- state_df %>% 
    filter(State == "California") %>%
    left_join(staff_pop %>% 
                  select(name, Staff.Population = pop), 
              by = c("State" = "name")) %>% 
    mutate(staff_pct = Staff.Initiated / Staff.Population) %>% 
    select(State, Date, staff_pct, Staff.Initiated, Staff.Population) 
View(ca_staff_vax)

ca_staff_vax_plot <- ca_staff_vax %>% 
    filter(!is.na(staff_pct),
           Date != "2021-04-04") %>%
    ggplot(aes(x = Date, 
                     y = staff_pct)) +
    geom_line(color = "#4C6788") + 
    geom_point(size = 1.0) +
    geom_vline(xintercept = as.Date("2021-08-19")) + 
    scale_y_continuous(label = scales::percent, limit = c(0, 1)) 
    # scale_x_date() +

ggsave("ca_staff_vaccine.png", width = 10, height = 8)


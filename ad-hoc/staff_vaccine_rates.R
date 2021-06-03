library(tidyverse)
library(behindbarstools)
library(ggalt)

state_df <- read_csv(stringr::str_c(
    "https://raw.githubusercontent.com/uclalawcovid19behindbars/data/master/", 
    "latest-data/state_aggregate_counts.csv"
))

staff_pop <- read_csv(stringr::str_c(
    "https://raw.githubusercontent.com/themarshallproject/COVID_prison_data/master/", 
    "data/staff_populations.csv"
))
    
state_df %>% 
    left_join(staff_pop %>% 
                  select(name, Staff.Population = april_pop), 
              by = c("State" = "name")) %>% 
    mutate(Staff.Population = coalesce(Staff.Population.x, Staff.Population.y)) %>% 
    mutate(staff_pct = Staff.Initiated / Staff.Population) %>% 
    select(State, staff_pct, Staff.Initiated, Staff.Population) %>% 
    filter(!is.na(staff_pct)) %>% 
    filter(State != "Minnesota") %>% 
    ggplot(aes(x = reorder(str_to_title(State), -staff_pct), y = staff_pct, 
               label = paste0(percent(staff_pct, accuracy = 1)))) + 
    geom_point(size = 3.0, color = "#4C6788") + 
    geom_segment(aes(x = reorder(str_to_title(State), -staff_pct), 
                     xend = reorder(str_to_title(State), -staff_pct), 
                     y = 0, 
                     yend = staff_pct), 
                 size = 1.0, color = "#4C6788") + 
    geom_text(size = 4, position = position_nudge(y = 0.06), color = base_color, family = "Helvetica") +
    coord_flip() + 
    scale_y_continuous(label = scales::percent, limit = c(0, 1)) +
    theme_classic(base_family = base_family, base_size = base_size) + 
    theme_custom + 
    theme(panel.grid.major.x = element_line(color = "#cdcfbe", linetype = "dotted"),
          panel.grid.major.y = element_line(color = "#cdcfbe", linetype = "dotted"), 
          axis.ticks.x = element_blank(), 
          axis.text.x = element_blank(), 
          axis.line.x = element_blank())

ggsave("staff_vaccine.svg", width = 10, height = 8)
ggsave("staff_vaccine.png", width = 10, height = 8)

library(tidyverse)
library(skimr)

statewide <- read_csv("https://raw.githubusercontent.com/uclalawcovid19behindbars/data/master/latest-data/latest_state_counts.csv")

sw <- statewide %>%
    mutate(cfr_cumulative = Residents.Deaths / Residents.Confirmed,
           cfr_label = glue("{round(cfr_cumulative, 3)*100}%") )

p <- sw %>% 
    arrange(-cfr_cumulative) %>% 
    filter(cfr_cumulative > .0118) %>% 
    mutate(State_ = forcats::fct_reorder(State, cfr_cumulative)) %>% 
    ggplot(aes(x = State_, y = cfr_cumulative, xend = State_, yend = 0, 
           label = cfr_label)) +
    geom_point(size = 3, color = "#4C6788") +
    geom_segment(size = 1.5, color = "#4C6788") +
    geom_text(size = 5, 
              position = position_nudge(y = .002),
              color = "#555526", family = "Helvetica") +
    coord_flip() +
    theme_behindbars(base_size = 18) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1L),
                       limits = c(0, .04)) + 
    theme(
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(color = "#555526"),
        axis.text.x = element_text(color = "#555526"),
        panel.grid.major.x = element_line(
            color = "#92926C", linetype = "dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none") +
    labs(title = "COVID-19 Case Fatality Rate: State Comparison",
         tag = "D3") 

ggsave("~/Desktop/ga_viz/cfr_state_comparison.svg", p, width = 10, height = 8)
ggsave("~/Desktop/ga_viz/cfr_state_comparison.png", p, width = 10, height = 8)


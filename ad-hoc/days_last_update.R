library(behindbarstools)
library(tidyverse)

last_df <- pull_last_update()

plot_df <- last_df %>%
    filter(!is.na(days_late)) %>%
    # only get these kind of scrapers
    filter(jurisdiction %in% c("state", "federal", "immigration")) %>%
    # remove population scrapers here
    filter(!stringr::str_detect(id, "(?i)population")) %>%
    # remove youth scrapers here
    filter(!stringr::str_detect(id, "(?i)youth")) %>%
    # Do some renaming
    mutate(State = ifelse(jurisdiction == "immigration", "ICE", State)) %>%
    mutate(State = ifelse(jurisdiction == "federal", "BOP", State)) %>%
    # average across all scrapers for a given agency
    group_by(State) %>%
    summarize(days_late = mean(days_late), .groups = "drop") %>%
    mutate(State = forcats::fct_reorder(State, days_late)) %>% 
    mutate(days_late = days_late + 3) %>% 
    # Manually add these 
    add_row(State = "GA", days_late = as.numeric(
        difftime(Sys.Date(), "2021-07-19", units = "days"))) %>% 
    add_row(State = "MA", days_late = as.numeric(
        difftime(Sys.Date(), "2021-07-15", units = "days")))

p <- plot_df %>% 
    # Manually remove these 
    filter(!State %in% c("AL", "DE", "TN")) %>% 
    arrange(-days_late) %>% 
    mutate(State_ = translate_state(State)) %>% 
    filter(days_late > 10) %>% 
    mutate(State_ = forcats::fct_reorder(State_, days_late)) %>% 
    ggplot(aes(x = State_, y = days_late, xend = State_, yend = 0, 
               label = ceiling(days_late))) +
    geom_point(size = 3, color = "#4C6788") +
    geom_segment(size = 1.5, color = "#4C6788") +
    geom_text(size = 5, position = position_nudge(y = 3.5), 
              color = "#555526", family = "Helvetica") +
    coord_flip() +
    theme_behindbars(base_size = 18) +
    theme(
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(color = "#555526"),
        axis.text.x = element_text(color = "#555526"),
        panel.grid.major.x = element_line(
            color = "#92926C", linetype = "dotted"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none") 

ggsave("days_late.svg", p, width = 6, height = 4)

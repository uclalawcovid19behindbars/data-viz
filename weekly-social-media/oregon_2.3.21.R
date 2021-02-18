library(behindbarstools)
library(tidyverse)

# scrape_or <- read_scrape_data(T, T, state = "Oregon")
mp_or <- read.csv("https://raw.githubusercontent.com/themarshallproject/COVID_prison_data/master/data/covid_prison_cases.csv")

# Cumulative cases 
or_cases <- mp_or %>% 
    filter(name == "Oregon") %>% 
    mutate(Date = lubridate::mdy(as_of_date)) %>% 
    mutate(last_value = ifelse(Date == max(Date), as.character(3392), NA_character_)) %>% 
    ggplot(aes(x = Date, y = total_prisoner_cases, color = name, fill = name, label = last_value)) + 
    geom_line(size = 2.0) +
    geom_area(alpha = 0.5) + 
    ggrepel::geom_text_repel(na.rm = T, show.legend = F, size = 6, nudge_x = 1, point.padding = 0.1) + 
    scale_x_date(date_labels = "%b %d",
                 expand = c(0.15, 0)) + 
    scale_color_bbdiscrete() +
    scale_fill_bbdiscrete() +
    theme_behindbars() +
    theme(axis.text.y = element_text(vjust = -0.6, hjust = 0, margin = margin(r = -45)),
          legend.position = "none",
          axis.title.x = element_blank()) + 
    labs(title = "Cumulative Cases Among Oregon State Prisoners", 
         subtitle = get_metric_description("Residents.Confirmed"), 
         y = get_metric_description("Residents.Confirmed", short = TRUE))

# Cumulative deaths 
or_deaths <- mp_or %>% 
    filter(name == "Oregon") %>% 
    mutate(Date = lubridate::mdy(as_of_date)) %>% 
    mutate(last_value = ifelse(Date == max(Date), as.character(42), NA_character_)) %>% 
    ggplot(aes(x = Date, y = total_prisoner_deaths, color = name, fill = name, label = last_value)) + 
    geom_line(size = 2.0) +
    geom_area(alpha = 0.5) + 
    ggrepel::geom_text_repel(na.rm = T, show.legend = F, size = 6, nudge_x = 1, point.padding = 0.1) + 
    scale_x_date(date_labels = "%b %d",
                 expand = c(0.15, 0)) + 
    scale_color_bbdiscrete() +
    scale_fill_bbdiscrete() +
    theme_behindbars() +
    theme(axis.text.y = element_text(vjust = -0.6, hjust = 0, margin = margin(r = -45)),
          legend.position = "none",
          axis.title.x = element_blank()) + 
    labs(title = "Cumulative Deaths Among Oregon State Prisoners", 
         subtitle = get_metric_description("Residents.Deaths"), 
         y = get_metric_description("Residents.Deaths", short = TRUE))

ggsave("or-cases.png", or_cases, width = 14, height = 10)
ggsave("or-cases.svg", or_cases, width = 14, height = 10)
ggsave("or-deaths.png", or_deaths, width = 14, height = 10)
ggsave("or-deaths.svg", or_deaths, width = 14, height = 10)


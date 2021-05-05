library(tidyverse)
library(behindbarstools)

all_dates <- read_scrape_data(all_dates = TRUE)

p <- all_dates %>% 
    filter(State == "Ohio") %>% 
    filter(Jurisdiction == "state") %>% 
    group_by(Date) %>% 
    summarise(Residents.Confirmed = sum_na_rm(Residents.Confirmed), 
              Residents.Recovered = sum_na_rm(Residents.Recovered)) %>% 
    pivot_longer(!Date, names_to = "metric", values_to = "value") %>% 
    filter(!is.na(value)) %>% 
    filter(Date > "2020-11-01") %>% 
    ggplot() + 
    geom_line(aes(x = Date, y = value, color = metric), size = 1) + 
    theme_behindbars(base_size = 14) + 
    theme(axis.title.y = element_blank(), 
          legend.title = element_blank(), 
          legend.position = "top") +
    scale_color_bbdiscrete() +
    labs(title = "Ohio Statewide Total Declines")

ggsave("ohio_declines.png", p, width = 6, height = 4)

p <- all_dates %>% 
    filter(State == "Ohio") %>% 
    filter(Jurisdiction == "state") %>% 
    filter(Name %in% c(
        "MARION CORRECTIONAL INSTITUTION", "PICKAWAY CORRECTIONAL INSTITUTION")) %>% 
    mutate(Name = str_replace(Name, "CORRECTIONAL INSTITUTION", "CI")) %>% 
    filter(Date > "2020-11-01") %>% 
    ggplot() + 
    geom_line(aes(x = Date, y = Residents.Confirmed, color = Name), size = 1) + 
    theme_behindbars(base_size = 14) + 
    scale_y_continuous(limits = c(1000, 2000)) + 
    theme(legend.title = element_blank(), 
          legend.position = "top") +
    scale_color_bbdiscrete() +
    labs(title = "Ohio Facility Declines")

ggsave("ohio_fac.png", p, width = 6, height = 4)

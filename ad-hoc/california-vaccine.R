library(behindbarstools)
library(tidyverse)

scrape_ca <- read_scrape_data(all_dates = TRUE, state = "California")

plt <- scrape_ca %>% 
    filter(Jurisdiction == "state") %>% 
    group_by(Date) %>% 
    summarise(res_vax = sum_na_rm(Residents.Initiated), 
              pop = sum_na_rm(Residents.Population)) %>% 
    filter(!is.na(res_vax)) %>% 
    ggplot() + 
    geom_line(aes(x = Date, y = pop), size = 2.0, color = "#2D6C4A") + 
    geom_line(aes(x = Date, y = res_vax), size = 2.0, color = "#D7790F") +
    geom_area(aes(x = Date, y = res_vax), fill = "#D7790F", alpha = 0.5) + 
    theme_behindbars() + 
    scale_y_continuous(labels = scales::comma, limits = c(0, 100000)) + 
    labs(title = "40% of incarcerated people in California's state prisons have been vaccinated", 
         y = "Number of incarcerated people") + 
    theme(axis.text.y = element_text(vjust = -0.6, hjust = 0.5, margin = margin(r = -45)))

ggsave("ca_vaccine.png", plt, width = 16, height = 10)

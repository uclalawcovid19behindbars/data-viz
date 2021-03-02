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
    geom_area(aes(x = Date, y = res_vax), fill = "#D7790F", alpha = 0.3) + 
    geom_ribbon(aes(x = Date, ymin = res_vax, ymax = pop), fill = "#82CAA4", alpha = 0.3) +
    theme_behindbars() + 
    scale_y_continuous(labels = scales::comma, limits = c(0, 100000)) + 
    labs(title = "44% of incarcerated people in California's state prisons have been vaccinated", 
         y = "Number of incarcerated people", 
         caption = "Vaccinated = 40,531 | Population = 91,310 | Percent Vaccinated = 44.4%")

ggsave("ca_vaccine.png", plt, width = 16, height = 10)
ggsave("ca_vaccine.svg", plt, width = 16, height = 10)

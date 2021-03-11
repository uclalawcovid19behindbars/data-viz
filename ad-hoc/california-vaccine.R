library(behindbarstools)
library(tidyverse)

scrape_ca <- read_scrape_data(all_dates = TRUE, state = "California")

plt1 <- scrape_ca %>% 
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

ggsave("ca_vaccine.png", plt1, width = 16, height = 10)
ggsave("ca_vaccine.svg", plt1, width = 16, height = 10)

plt2 <- scrape_ca %>% 
    filter(Jurisdiction == "state") %>% 
    group_by(Date) %>% 
    summarise(pop = sum_na_rm(Residents.Population), 
              pop_pct = pop / pop, 
              res_vax_pct = sum_na_rm(Residents.Initiated) / pop) %>% 
    filter(!is.na(res_vax_pct)) %>% 
    ggplot() + 
    geom_line(aes(x = Date, y = pop_pct), size = 2.0, color = "#2D6C4A") + 
    geom_line(aes(x = Date, y = res_vax_pct), size = 2.0, color = "#D7790F") +
    geom_area(aes(x = Date, y = res_vax_pct), fill = "#D7790F", alpha = 0.3) + 
    geom_ribbon(aes(x = Date, ymin = res_vax_pct, ymax = pop_pct), fill = "#82CAA4", alpha = 0.3) +
    theme_behindbars() + 
    scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    labs(title = "44% of incarcerated people in California's state prisons have been vaccinated", 
         y = "Percentage of incarcerated people", 
         caption = "Vaccinated = 40,531 | Population = 91,310 | Percent Vaccinated = 44.4%")

ggsave("ca_vaccine_pct.png", plt2, width = 16, height = 10)
ggsave("ca_vaccine_pct.svg", plt2, width = 16, height = 10)

library(behindbarstools)
library(tidyverse)

scrape_wa <- read_scrape_data(state = "Washington", window_pop = 100)

wa_vax <- scrape_wa %>% 
    filter(Jurisdiction == "state") %>% 
    mutate(pct_vax = Residents.Initiated / Residents.Population, 
           Name = stringr::str_to_title(Name)) %>% 
    filter(!is.na(pct_vax))

(plt <- ggplot(wa_vax, aes(x = reorder(Name, pct_vax), y = pct_vax, label = scales::percent(pct_vax, accuracy = 1))) +
    geom_bar(stat = "identity", width = 0.5, fill = "#4C6788") +
    geom_text(size = 4, position = position_nudge(y = -0.05), color = "white", family = "Helvetica") +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    labs(title = "Percentage Vaccinated in Washington State Prisons", 
         caption = stringr::str_c("Data As of: ", Sys.Date())) +
    theme_behindbars(base_size = 15) + 
    theme(axis.title.y = element_blank()) + 
    coord_flip()) 

ggsave("washington_vaccine.png", plt, width = 10, height = 8)
ggsave("washington_vaccine.svg", plt, width = 10, height = 8)

library(behindbarstools)
library(tidyverse)

scrape_pa <- behindbarstools::read_scrape_data(T, T, state = "Pennsylvania")

spikes_out_pa <- plot_recent_fac_increases(scrape_pa, annotate = T, plot_days = 30) + 
    labs(y = "Cases Among Incarcerated People", 
         color = "Facility", 
         title = "Facilities with Recent Spikes in COVID-19 Cases") + 
    theme(plot.tag.position = c(0.8, 0.02)) 

ggsave("recent_spikes_PA.png", spikes_out_pa, width = 12, height = 10)

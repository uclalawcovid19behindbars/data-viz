library(behindbarstools)
library(tidyverse)

scrape_az <- read_scrape_data(T, T, state = "Arizona")

out <- plot_fac_trend("EYMAN STATE PRISON", 
                      state = "Arizona", 
                      metric = "Residents.Deaths", 
                      scrape_df = scrape_az, 
                      plot_days = 120, 
                      area_plot = F) + 
  geom_vline(xintercept = as.Date("2021-05-05"), size = 1, color = "#555526") + 
  labs(title = "COVID Deaths at Eyman State Prison, AZ", 
       y = "COVID-19 Deaths Among Incarcerated People", 
       caption = str_c("Data as of ", Sys.Date())) + 
  scale_y_continuous(breaks=c(1,5,10,14))

ggsave("eyman_az.png", out, width = 14, height = 10)


library(behindbarstools)
library(tidyverse)

az <- read_scrape_data(all_dates = TRUE, window = 7, state = "Arizona")

out <- plot_fac_trend(fac_name = "ICE SAN LUIS REGIONAL DETENTION CENTER", 
                      state = "Arizona", 
                      metric = "Residents.Confirmed", 
                      plot_days = 40, 
                      scrape_df = az, 
                      area_plot = TRUE, 
                      annotate = TRUE, 
                      auto_label = TRUE) + 
  scale_color_manual(values = "#AE91A8") + 
  scale_fill_manual(values = "#AE91A8") + 
  labs(title = "San Luis Regional ICE Detention Center (Yuma County, AZ)", 
       caption = "+61 new cases in the past 30 days")
# capacity: 860 (seems very low capacity right now) 
# population feb 20: 53

ggsave("san_luis_3.30.21.png", out, width = 14, height = 10)
ggsave("san_luis_3.30.21.svg", out, width = 14, height = 10)


active <- plot_fac_trend(fac_name = "ICE SAN LUIS REGIONAL DETENTION CENTER", 
                      state = "Arizona", 
                      metric = "Residents.Active", 
                      plot_days = 30, 
                      scrape_df = az, 
                      area_plot = FALSE, 
                      annotate = TRUE, 
                      auto_label = TRUE) + 
  scale_color_manual(values = "#AE91A8") + 
  scale_fill_manual(values = "#AE91A8") + 
  labs(title = "San Luis Regional ICE Detention Center (Yuma County, AZ)", 
       caption = "+61 new cases in the past 30 days")
# capacity: 860 (seems very low capacity right now) 
# population feb 20: 53

ggsave("active_san_luis_3.30.21.png", active, width = 14, height = 10)
ggsave("active_san_luis_3.30.21.svg", active, width = 14, height = 10)
library(tidyverse)
library(behindbarstools)

historical_nc <- read.csv("https://raw.githubusercontent.com/uclalawcovid19behindbars/historical-data/main/data/NC-historical-data.csv")

out <- plot_fac_trend(
    fac_name = "NORTH CAROLINA CORRECTIONAL INSTITUTION FOR WOMEN", 
    state = "North Carolina", 
    scrape_df = historical_nc %>% mutate(Date = lubridate::ymd(Date)), 
    metric = "Residents.Confirmed", 
    area_plot = TRUE, 
    annotate = TRUE, 
    auto_label = TRUE
) + 
    labs(title = "NORTH CAROLINA CORRECTIONAL INSTITUTION FOR WOMEN", 
         caption = "60% increase (167 new cases) in the past 30 days (442-275)/275 \n 2 residents have died from COVID-19")

ggsave("NCCIW-historical.png", out, width = 14, height = 10)
ggsave("NCCIW-historical.svg", out, width = 14, height = 10)

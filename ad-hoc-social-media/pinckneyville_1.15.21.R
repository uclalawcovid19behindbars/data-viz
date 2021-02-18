library(tidyverse)
library(behindbarstools)

scrape_il <- read_scrape_data(T, T, state = "Illinois")

out <- plot_fac_trend(fac_name = "PINCKNEYVILLE CORRECTIONAL CENTER", 
               state = "Illinois", 
               metric = "Residents.Confirmed", 
               plot_days = 50, 
               scrape_df = scrape_il, 
               area_plot = TRUE, 
               annotate = TRUE, 
               auto_label = TRUE) + 
    labs(title = "PINCKNEYVILLE CORRECTIONAL CENTER (Perry County, IL)", 
         caption = "+472 cases since 2021 \n Population of 1,592 as of 12/31/20 (from IDOC website)")

ggsave("pinckneyville_1.15.21.png", out, width = 14, height = 10)
ggsave("pinckneyville_1.15.21.svg", out, width = 14, height = 10)

library(behindbarstools)
library(tidyverse)

scrape_df <- read_scrape_data(T, T)

clean_df <- scrape_df %>% 
    mutate(Name = case_when(Name == "NCCI GARDN" ~ "NORTH CENTRAL CI - GARDNER", 
                            Name == "COFFIELD" ~ "H.H. COFFIELD UNIT", 
                            TRUE ~ Name))

spikes <- plot_recent_fac_increases(scrape_df = clean_df,
                                    annotate = TRUE, 
                                    plot_days = 20) + 
    labs(title = "Facilities with Recent Spikes in COVID-19 Cases", 
         y = "Cases Among Incarcerated People", 
         color = "Facility") 

ggsave("spikes.svg", spikes, width = 14, height = 10)
ggsave("spikes.png", spikes, width = 14, height = 10)


nyc_v1 <- plot_fac_trend(fac_name = "NEW YORK CITY JAILS", 
               state = "New York", 
               metric = "Residents.Confirmed", 
               plot_days = 40, 
               annotate = TRUE,
               scrape_df = scrape_df) + 
    scale_y_continuous(labels = scales::comma) + 
    labs(y = "Reported Cases Among Incarcerated People", 
         title = "Cumulative COVID-19 Cases in New York City Jails")

ggsave("nyc_v1.svg", nyc_v1, width = 14, height = 10)
ggsave("nyc_v1.png", nyc_v1, width = 14, height = 10)


nyc_v2 <- scrape_df %>% 
    filter(Name == "NEW YORK CITY JAILS") %>% 
    filter(Date > "2020-10-15") %>% 
    ggplot(aes(x = Date, y = Residents.Confirmed)) + 
    geom_line(size = 2.0, color = "#D7790F") + 
    geom_area(aes(x = Date, y = Residents.Confirmed), fill = "#D7790F", alpha = 0.5) + 
    scale_x_date(date_labels = "%b %d") +
    theme_behindbars() + 
    scale_y_continuous(labels = scales::comma, limits = c(0, 4000)) + 
    labs(y = "Reported Cases Among Incarcerated People", 
         title = "Cumulative COVID-19 Cases in New York City Jails")
    
ggsave("nyc_v2.svg", nyc_v2, width = 14, height = 10)
ggsave("nyc_v2.png", nyc_v2, width = 14, height = 10)


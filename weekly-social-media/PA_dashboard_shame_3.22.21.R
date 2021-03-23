library(behindbarstools)
library(tidyverse)
library(plotly)

pa <- read_scrape_data(all_dates = TRUE, window = 7, state = "Pennsylvania")

p <- pa %>% 
    filter(Jurisdiction == "state") %>% 
    group_by(State, Date) %>% 
    summarise(Residents.Confirmed = sum(Residents.Confirmed)) %>% 
    filter(!is.na(Residents.Confirmed)) %>% 
    ggplot(aes(x = Date, y = Residents.Confirmed)) + 
    geom_line(size = 2.0, color = "#D7790F") + 
    geom_area(alpha = 0.5, fill = "#D7790F") + 
    annotate("rect", 
             xmin = as.Date("2021-01-29"), xmax = Sys.Date(), ymin = 0, ymax = 10000,
             alpha = 0.8, fill = "black") + 
    annotate("text", 
             x = as.Date("2020-11-10"), 
             y = 9400, 
             label = "On January 29, the PA DOC\nremoved its public data dashboard", 
             size = 6, 
             color = "#555526") + 
    labs(y = "Reported Cumulative Cases Among Incarcerated People") + 
    theme_behindbars() + 
    scale_y_continuous(label = scales::comma) + 
    scale_x_date(date_labels = "%b %Y", limits = c(as.Date("2020-04-01"), as.Date("2021-04-01")))

ggsave("pa_dash.png", p, width = 13, height = 10)    
ggsave("pa_dash.svg", p, width = 13, height = 10)

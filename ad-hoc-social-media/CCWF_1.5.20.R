library(behindbarstools)
library(tidyverse)

scrape_ca <- read_scrape_data(T, T, state = "California")

plt1 <- scrape_ca %>% 
    filter(Name == "CCWF CENTRAL CA WOMENS FACILITY") %>% 
    filter(Date > "2020-12-15") %>% 
    ggplot(aes(x = Date, y = Residents.Active)) + 
    geom_line(size = 2.0, color = "#D7790F") + 
    geom_area(aes(x = Date, y = Residents.Active), fill = "#D7790F", alpha = 0.5) + 
    geom_hline(yintercept = 2000, color = "#497F63", linetype = "dotted") + 
    scale_x_date(date_labels = "%b %d") +
    theme_behindbars() + 
    scale_y_continuous(labels = scales::comma, limits = c(0, 2500)) + 
    labs(title = "25% of Incarcerated People Have COVID-19 ", 
         subtitle = "In CCWF Central CA Women's Prison - Madera County, California", 
         y = "Active Cases") 

ggsave("ccwf_v1.svg", plt1, width = 11, height = 8)
ggsave("ccwf_v1.png", plt1, width = 11, height = 8)

plt2 <- scrape_ca %>% 
    filter(Name == "CCWF CENTRAL CA WOMENS FACILITY") %>% 
    filter(Date > "2020-12-15") %>% 
    ggplot(aes(x = Date, y = Residents.Active)) + 
    geom_line(size = 2.0, color = "#D7790F") + 
    geom_area(aes(x = Date, y = Residents.Active), fill = "#D7790F", alpha = 0.5) + 
    scale_x_date(date_labels = "%b %d") +
    theme_behindbars() + 
    scale_y_continuous(labels = scales::comma) + 
    labs(title = "25% of Incarcerated People Have COVID-19 ", 
         subtitle = "In CCWF Central CA Women's Prison - Madera County, California", 
         y = "Active Cases") 

ggsave("ccwf_v2.svg", plt2, width = 11, height = 8)
ggsave("ccwf_v2.png", plt2, width = 11, height = 8)
        

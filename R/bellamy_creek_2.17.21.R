library(behindbarstools)
library(tidyverse)

scrape_mi <- read_scrape_data(T, T, state = "Michigan")

out <- plot_fac_trend("BELLAMY CREEK CORRECTIONAL FACILITY", 
               state = "Michigan", 
               metric = "Residents.Active", scrape_df = scrape_mi, 
               plot_days = 60, 
               annotate = T, 
               area_plot = T) + 
    geom_vline(xintercept = as.Date("2021-02-08"), size = 1, color = "#555526") + 
    labs(title = "Active COVID-19 Cases at Bellamy Creek Correctional Facility", 
         y = "Reported Active Cases Among Incarcerated People") + 
    annotate("text", 
             x = as.Date("2021-01-30"), 
             y = 220, 
             label = "First case of the B.1.1.7 \nvariant detected on Feb 8", 
             size = 6, 
             color = "#555526")
    
ggsave("bellamy.png", out, width = 14, height = 10)

out2 <- scrape_mi %>% 
    filter(Name == "BELLAMY CREEK CORRECTIONAL FACILITY") %>% 
    mutate(Staff.Act.Est = diff_roll_sum(Staff.Confirmed, Date)) %>% 
    filter(!is.na(Date)) %>% 
    filter(Date > "2021-01-01") %>% 
    mutate(Res.Act.Est = diff_roll_sum(Residents.Confirmed, Date)) %>% 
    ggplot(aes(x = Date)) + 
    geom_line(aes(y = Residents.Active), color = "#D7790F", size = 1.5) +
    geom_line(aes(y = Staff.Act.Est), color = "#4C6788", size = 1.5) +
    theme_behindbars() + 
    labs(y = "Estimated Active Cases", 
         title = "Comparison of outbreaks beween staff and incarcerated people", 
         subtitle = "Bellamy Creek Correctional Facility, Michigan")
    
ggsave("bellamy_staff_comparison.png", out2, width = 14, height = 10)

library(behindbarstools)
library(tidyverse)

all_scrape <- read_scrape_data(all_dates = TRUE)

federal <- all_scrape %>% 
    filter(Jurisdiction == "federal")
    
# Sum of max of each facility 
federal %>%     
    group_by(Name) %>% 
    summarise(max = max(Residents.Confirmed)) %>% 
    summarise(sum_na_rm(max))

# Total positive tests over time 
p1 <- federal %>% 
    group_by(Date) %>%
    filter(Date > as.Date("2021-02-01")) %>% 
    summarise(total_cases = sum_na_rm(Residents.Confirmed)) %>%
    ggplot(aes(x = Date, y = total_cases)) + 
    geom_line(size = 2.0, color = "#4C6788") + 
    geom_point(size = 4.0, color = "#4C6788") + 
    theme_behindbars() + 
    scale_y_continuous(label = scales::comma) + 
    labs(title = "Reported number of inmates that have ever had a positive test", 
         y = "Positive tests")

plot_df <- federal %>% 
    arrange(Name, Date) %>% 
    group_by(Name) %>% 
    mutate(diff = Residents.Confirmed - lag(Residents.Confirmed), 
           neg = ifelse(diff < 0, 1, 0)) %>% 
    select(Date, Name, Facility.ID, Residents.Confirmed, diff, neg) %>% 
    group_by(Date) %>% 
    summarise(num_neg = sum_na_rm(neg), 
              delta = sum_na_rm(diff)) %>% 
    mutate(wday = lubridate::wday(Date, label = TRUE)) %>% 
    filter(Date > as.Date("2020-11-01"))

# Number of facilities with declines between scrapes 
p2 <- ggplot(plot_df, aes(x = Date, y = num_neg)) + 
    geom_bar(stat = "identity", color = "#4C6788") + 
    labs(subtitle = "Number of facilities reporting declining cumulative cases between scrapes", 
         y = "Number of facilities") + 
    theme_behindbars() 

# Change in totals between scrapes 
p3 <- ggplot(plot_df, aes(x = Date, y = delta)) + 
    geom_bar(stat = "identity", color = "#4C6788") + 
    scale_y_continuous(label = scales::comma) + 
    labs(subtitle = "Total change in cumulative cases between scrapes", 
         y = "Change in cumulave cases") + 
    theme_behindbars() 

ggsave("p1.png", p1, width = 14, height = 10)
ggsave("p2.png", p2, width = 14, height = 10)
ggsave("p3.png", p3, width = 14, height = 10)


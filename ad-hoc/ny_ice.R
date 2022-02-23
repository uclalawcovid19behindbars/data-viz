library(tidyverse)
library(behindbarstools)

scrape_df <- read_scrape_data(all_dates = TRUE)
ice_nys <- scrape_df %>%
    filter(Jurisdiction == "immigration",
           State == "New York") %>%
    mutate(Residents.Active.Pct = Residents.Active / Population.Feb20)

# questions ------------------------------------------------------------------

## what facilities do we have data for?
ice_nys %>% 
    select(Name) %>% 
    unique()

## what was the peak number of cases at each facility, and when did that occur?


## what was the peak % of population infected, out of all time? during omicron?


# graphs ------------------------------------------------------------------

## percent active infections
ice_nys %>% 
    ggplot(aes(x = Date, 
               y = Residents.Active.Pct, 
               color = Name, 
               group = Name)) + 
    geom_line(size = 1.0) +
    facet_wrap(~Name, nrow=2, scales = "free") + 
    theme_behindbars(base_size = 18, base_color = "black") + 
    labs(y = "Percent Active COVID-19 cases") + 
    scale_color_bbdiscrete() +
    theme(legend.position = "none",
          legend.title = element_blank()) + 
    scale_y_continuous(labels = scales::percent_format(accuracy = 1))    

# deaths --> none reported 
ice_nys %>% 
    ggplot(aes(x = Date, 
               y = Residents.Deaths, 
               color = Name, 
               group = Name)) + 
    geom_line(size = 1.0) +
    facet_wrap(~Name, nrow=2, scales = "free") + 
    theme_behindbars(base_size = 18, base_color = "black") + 
    labs(y = "COVID Deaths Among People Detained") + 
    scale_color_bbdiscrete() +
    theme(legend.position = "none",
          legend.title = element_blank())  



library(behindbarstools)
library(tidyverse)

all_wy <- read_scrape_data(all_dates = T, state = "Wyoming")

out <- all_wy %>% 
    ## filter to WYOMING MEDIUM CORRECTIONAL INSTITUTION
    filter(Facility.ID %in% c(1716, 1715)) %>%
    filter(Date > "2021-08-01") %>% 
    group_by(Date) %>% 
    ggplot(aes(x = Date, color = Name)) + 
    # geom_line(aes(y = Residents.Active), color = "#D7790F", size = 1.0) + 
    geom_line(aes(y = Residents.Active), size = 1.0) +
    scale_y_continuous(limits = c(0, 115)) +
    scale_x_date(limits = c(as.Date("2021-08-01"), as.Date("2021-11-01"))) +
    theme_behindbars(base_size = 18, base_color = "black") + 
    labs(y = "Active COVID-19 cases") + 
    scale_color_bbdiscrete() + 
    theme(legend.position = "none", 
          legend.title = element_blank())

ggsave("wyoming_active.svg", out, width = 7, height = 5)


## check latest numbers
all_wy %>%
    filter(Facility.ID %in% c(1716, 1715)) %>%
    select(Date, Name, Residents.Active) %>%
    View()

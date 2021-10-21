library(behindbarstools)
library(tidyverse)

all_wy <- read_scrape_data(all_dates = T, state = "Wyoming")

out <- all_wy %>% 
    ## filter to WYOMING MEDIUM CORRECTIONAL INSTITUTION
    filter(Facility.ID == 1716) %>%
    filter(Date > "2021-08-01") %>% 
    group_by(Date) %>% 
    # summarise(total_staff = sum_na_rm(Staff.Active), 
    #           total_res = sum_na_rm(Residents.Active)) %>% 
    ggplot(aes(x = Date)) + 
    geom_line(aes(y = Residents.Active), color = "#D7790F", size = 1.0) + 
    # geom_line(aes(y = Staff.Active), color = "#4C6788", size = 1.0) + 
    scale_y_continuous(limits = c(0, 115)) +
    scale_x_date(limits = c(as.Date("2021-08-01"), as.Date("2021-11-01"))) +
    theme_behindbars(base_size = 18) + 
    # geom_vline(xintercept = as.Date("2021-06-13"), linetype = "dashed") + 
    labs(y = "Active COVID-19 cases")

ggsave("wyoming_active.svg", out, width = 7, height = 5)

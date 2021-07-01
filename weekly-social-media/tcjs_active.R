library(behindbarstools)
library(tidyverse)

all_tx <- read_scrape_data(all_dates = T, state = "Texas")

out <- all_tx %>% 
    filter(source == "https://www.tcjs.state.tx.us/") %>% 
    filter(Date <= "2021-06-13") %>% 
    group_by(Date) %>% 
    summarise(total_staff = sum_na_rm(Staff.Active), 
              total_res = sum_na_rm(Residents.Active)) %>% 
    ggplot(aes(x = Date)) + 
    geom_line(aes(y = total_staff), color = "#D7790F", size = 1.0) + 
    geom_line(aes(y = total_res), color = "#4C6788", size = 1.0) + 
    scale_y_continuous(limits = c(0, 120)) +
    scale_x_date(limits = c(as.Date("2021-04-15"), as.Date("2021-06-16"))) + 
    theme_behindbars(base_size = 18) + 
    geom_vline(xintercept = as.Date("2021-06-13"), linetype = "dashed") + 
    labs(y = "Active COVID-19 cases in Texas jails")

ggsave("tcjs_active.svg", out, width = 7, height = 5)

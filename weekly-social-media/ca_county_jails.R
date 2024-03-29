library(behindbarstools)
library(tidyverse)

all_ca <- read_scrape_data(all_dates = T, state = "California")

all_ca %>%
    filter(Name %in% c("SACRAMENTO COUNTY JAIL",
                       "SANTA CLARA COUNTY JAIL",
                       "SANTA RITA JAIL")) %>%
    select(Date, Name, Residents.Active) %>%
    View()

start_date <- "2021-10-01"
out <- all_ca %>% 
    filter(Name %in% c("SACRAMENTO COUNTY JAIL",
                              "SANTA CLARA COUNTY JAIL")) %>%
    filter(Date > "2021-09-15") %>% 
    group_by(Date) %>% 
    ggplot(aes(x = Date, color = Name)) + 
    # geom_line(aes(y = Residents.Active), color = "#D7790F", size = 1.0) + 
    geom_line(aes(y = Residents.Active), size = 1.0) +
    # scale_y_continuous(limits = c(0, 115)) +
    scale_x_date(limits = c(as.Date(start_date), as.Date("2021-11-20"))) +
    theme_behindbars(base_size = 18, base_color = "black") + 
    labs(y = "Active COVID-19 cases") + 
    scale_color_bbdiscrete() + 
    theme(legend.position = "none",
          legend.title = element_blank())

ggsave("ca_jails_active.svg", out, width = 5, height = 5)
ggsave("ca_jails_active_wide.svg", out, width = 8, height = 5)

ggsave("ca_jails_active.png", out, width = 7, height = 5)

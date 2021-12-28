library(behindbarstools)
library(tidyverse)

all_dat <- read_scrape_data(all_dates = T)

## .Active
facs_to_keep <- c(2337, #ALLENWOOD FEDERAL CORRECTIONAL COMPLEX, PA
                  925,  #NEW YORK CITY JAILS
                  # 1705, #TAYCHEEDAH CORRECTIONAL INSTITUTION, WI
                  2330)  #ALDERSON FEDERAL PRISON CAMP

start_date <- "2021-11-15"
out <- all_dat %>% 
    filter(Facility.ID %in% facs_to_keep) %>%
    filter(Date >= start_date) %>% 
    group_by(Date) %>% 
    ggplot(aes(x = Date, color = Name)) + 
    geom_line(aes(y = Residents.Active), size = 1.0, 
              position=position_jitter(w=0.02, h=0)) +
    scale_x_date(limits = c(as.Date(start_date), as.Date("2022-01-01"))) +
    theme_behindbars(base_size = 18, base_color = "black") + 
    labs(y = "Active COVID-19 cases") + 
    scale_color_bbdiscrete() + 
    theme(legend.position = "none",
          legend.title = element_blank())

ggsave("outbreaks_12.28.21.png", out, width = 9, height = 5)
ggsave("outbreaks_12.28.21_twitter.svg", out, width = 9, height = 5)
ggsave("outbreaks_12.28.21_insta.svg", out, width = 5, height = 5)
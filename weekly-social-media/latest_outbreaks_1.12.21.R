library(behindbarstools)
library(tidyverse)

all_dat <- read_scrape_data(all_dates = T)
facs_to_keep <- c(
                 #1727, #Santa Rita Jail
                  # 1573,  #State Farm CC (VA)
                  # 683,   #Parnall CF (MI)
                  # 1609,#Olympic CC (WA) --> might only have .Confirmed
                  # 2445, 
                  # 1242,
                  465,
                  1688,
                  488,92,689)  

start_date <- "2021-12-15"
out <- all_dat %>% 
    filter(Facility.ID %in% facs_to_keep) %>%
    filter(Date >= start_date) %>% 
    group_by(Date) %>% 
    ggplot(aes(x = Date, color = Name)) + 
    geom_line(aes(y = Residents.Active), size = 1.0, 
              position=position_jitter(w=0.02, h=0)) +
    theme_behindbars(base_size = 18, base_color = "black") + 
    labs(y = "Active COVID-19 cases") + 
    scale_color_bbdiscrete() + 
    theme(legend.position = "none",
          legend.title = element_blank())

ggsave("outbreaks_1.13.21_twitter.svg", out, width = 9, height = 5)
ggsave("outbreaks_1.13.21_insta.svg", out, width = 5, height = 5)


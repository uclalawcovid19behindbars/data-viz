library(behindbarstools)
library(tidyverse)

all_dat <- read_scrape_data(all_dates = T)

jails <- all_dat %>%
    filter(Name %in% c("LOS ANGELES JAILS",
                        "SACRAMENTO COUNTY JAIL",
                       "MARICOPA COUNTY JAIL",
                       "COOK COUNTY JAIL",
                       "SAN DIEGO COUNTY JAILS",
                       "PHLADELPHIA DEPT OF JAILS")) 

start_date <- "2021-10-15"

f_jails <- jails %>% 
    filter(Date > as.Date("2021-10-01")) %>%
    select(Facility.ID, State, Name, Date, 
           Residents.Confirmed, Staff.Confirmed,
           Residents.Active, Staff.Active, 
           Residents.Initiated, Staff.Initiated, 
           Residents.Population, Staff.Population)

f_jails %>% group_by(Facility.ID) %>% skim()

jails_pct_increase <- jails %>%
    filter(Date %in% c(as.Date(start_date), as.Date("2021-12-07"))) %>% 
    group_by(Facility.ID) %>% 
    mutate(Residents.New.Confirmed = Residents.Confirmed - lag(Residents.Confirmed), 
           Staff.New.Confirmed = Staff.Confirmed - lag(Staff.Confirmed),
           Residents.New.Active = Residents.Active - lag(Residents.Active),
           Staff.New.Active = Staff.Active - lag(Staff.Active),
           Residents.Pct.Increase.Active = Residents.New.Active / Residents.Active,
           Staff.Pct.Increase.Active = Staff.New.Active / Staff.Active,
           Residents.Pct.Increase.Confirmed = Residents.New.Confirmed / Residents.Confirmed,
           Staff.Pct.Increase.Confirmed = Staff.New.Confirmed / Staff.Confirmed) %>%
    ungroup() %>% 
    # filter(!is.na(Staff.New) & !is.na(Residents.New)) %>% 
    select(Facility.ID, State, Name, Date, 
           Residents.Confirmed, Staff.Confirmed,
           Residents.New.Confirmed, Staff.New.Confirmed, 
           Residents.New.Active, Staff.New.Active,
           Residents.Pct.Increase.Active, Staff.Pct.Increase.Active,
           Residents.Pct.Increase.Confirmed, Staff.Pct.Increase.Confirmed,
           Residents.Initiated, Staff.Initiated, 
           Residents.Population, Staff.Population) #%>% 
    # mutate(Residents.New.Pct = Residents.New / Residents.Population * 10000, 
    #        Staff.New.Pct = Staff.New / Staff.Population * 10000)

out <- jails %>%
    group_by(Date) %>% 
    ggplot(aes(x = Date, color = Name)) + 
    # geom_line(aes(y = Residents.Active), color = "#D7790F", size = 1.0) + 
    geom_line(aes(y = Residents.Active), size = 1.0) +
    scale_y_continuous(limits = c(0, 200)) +
    scale_x_date(limits = c(as.Date(start_date), as.Date("2021-11-20"))) +
    theme_behindbars(base_size = 18, base_color = "black") + 
    labs(y = "Active COVID-19 cases") + 
    scale_color_bbdiscrete() 

out_bars <- jails_pct_increase %>%
    filter(Date == as.Date("2021-12-07")) %>%
    ggplot(aes(x = Name, y = Residents.Pct.Increase.Active, fill = Name)) + 
    geom_bar(stat = "identity") + 
    coord_flip() + 
    scale_y_continuous(labels = scales::percent) + 
    labs(y = "Increase in active COVID-19 cases since 10/15/21", x = "") + 
    theme_behindbars(base_size = 18, base_color = "black") + 
    scale_fill_bbdiscrete() + 
    theme(legend.position = "none",
          legend.title = element_blank()) 
    
ggsave("jails_pct_increase_bars.png", out_bars, width = 10, height = 5)
ggsave("jails_active_lines.png", out, width = 10, height = 5)
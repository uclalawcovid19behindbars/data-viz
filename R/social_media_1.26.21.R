library(behindbarstools)
library(tidyverse)

## read in data
ca <- read_csv("https://raw.githubusercontent.com/uclalawcovid19behindbars/historical-data/main/data/CA-historical-data.csv")
nj <- read_csv("https://raw.githubusercontent.com/uclalawcovid19behindbars/historical-data/main/data/NJ-historical-data.csv")
az <- read_csv("https://raw.githubusercontent.com/uclalawcovid19behindbars/historical-data/main/data/AZ-historical-data.csv")
  
## 1 idea: highlight ca jails
ca_jails <- ca %>%
  filter(Name %in% c("SACRAMENTO COUNTY JAIL",
                     "SACRAMENTO COUNTY",
                     "SANTA RITA JAIL",
                     "YOLO COUNTY",
                     'YOLO COUNTY JAIL',
                     "SAN FRANCISCO COUNTY", 
                     "SANTA CLARA COUNTY",
                     "SANTA CLARA COUNTY JAIL")) %>%
  mutate(Name = ifelse(Name == "YOLO COUNTY", "YOLO COUNTY JAIL", Name),
         Name = ifelse(Name == "SACRAMENTO COUNTY", "SACRAMENTO COUNTY JAIL", Name),
         Name = ifelse(Name == "SANTA CLARA COUNTY", "SANTA CLARA COUNTY JAIL", Name)) %>%
  group_by_coalesce(Name, Date)

ca_jail_plot <- plot_recent_fac_increases(ca_jails, annotate = T, plot_days = 30) + 
  labs(y = "Cases Among Incarcerated People", 
       color = "Facility", 
       title = "Recent Spikes in COVID-19 Cases") + 
  theme(plot.tag.position = c(0.8, 0.02)) 

ggsave("sacramento.svg", ca_jail_plot, width = 14, height = 10)



## 1 idea: active cases, resident vs staff
days_active <- 14
bbcolors <- c(
  "#D7790F", "#82CAA4", "#4C6788", "#84816F",
  "#71A9C9", "#AE91A8")

## active cases (residents vs staff): California

# get a sense of facilties with most staff confirmed
ca %>% 
  mutate(st_ratio = (Staff.Confirmed / Residents.Confirmed) + 0.0001)  %>%
  arrange(desc(st_ratio)) %>% 
  View()

# options to test
facility_sel <- "NORTH KERN STATE PRISON" # seems to be some data reporting issue, but this one is interesting
facility_sel <- "CALIFORNIA REHABILITATION CENTER" # interesting but not super striking
facility_sel <- "CALIPATRIA STATE PRISON" # interesting but not super striking

ca %>%
  filter(Name == facility_sel) %>%
  mutate(
    Est.Res.Act = diff_roll_sum(
      Residents.Confirmed, Date, window = days_active),
    Est.Staff.Act = diff_roll_sum(
      Staff.Confirmed, Date, window = days_active)) %>%
  filter(Est.Staff.Act > 0) %>%
  ggplot(aes(x = Date)) +
  geom_line(aes(y = Est.Res.Act), color = "#D7790F", size = 1.5) +
  geom_line(aes(y = Est.Staff.Act), color = "#4C6788", size = 1.5) +
  geom_area(aes(y = Est.Res.Act), alpha = .25, fill = "#D7790F") +
  geom_area(aes(y = Est.Staff.Act), alpha = .25, fill = "#4C6788") +
  theme_behindbars() +
  labs(y = "Estimated Active Cases") +
  ggtitle("Monitoring Facility Outbreaks", str_to_title(facility_sel))

# option 1 
facility_sel <- "CALIFORNIA HEALTH CARE FACILITY" 
ca_hcf_full <- ca %>%
  filter(Name == facility_sel) %>%
  mutate(
    Est.Res.Act = diff_roll_sum(
      Residents.Confirmed, Date, window = days_active),
    Est.Staff.Act = diff_roll_sum(
      Staff.Confirmed, Date, window = days_active)) %>%
  filter(Est.Staff.Act > 0) %>%
  ggplot(aes(x = Date)) +
  geom_line(aes(y = Est.Res.Act), color = "#D7790F", size = 1.5) +
  geom_line(aes(y = Est.Staff.Act), color = "#4C6788", size = 1.5) +
  geom_area(aes(y = Est.Res.Act), alpha = .25, fill = "#D7790F") +
  geom_area(aes(y = Est.Staff.Act), alpha = .25, fill = "#4C6788") +
  theme_behindbars() +
  labs(y = "Estimated Active Cases") +
  ggtitle("Monitoring Facility Outbreaks", str_to_title(facility_sel))
ggsave("ca_hcf_full.svg", ca_hcf_full, width = 14, height = 10)

# option 1, zoom in on post-November 
ca_hcf_filtered <- ca %>%
  filter(Name == facility_sel) %>%
  filter(Date > as.character.Date("2020-09-30")) %>%
  mutate(
    Est.Res.Act = diff_roll_sum(
      Residents.Confirmed, Date, window = days_active),
    Est.Staff.Act = diff_roll_sum(
      Staff.Confirmed, Date, window = days_active)) %>%
  filter(Est.Staff.Act > 0) %>%
  ggplot(aes(x = Date)) +
  geom_line(aes(y = Est.Res.Act), color = "#D7790F", size = 1.5) +
  geom_line(aes(y = Est.Staff.Act), color = "#4C6788", size = 1.5) +
  geom_area(aes(y = Est.Res.Act), alpha = .25, fill = "#D7790F") +
  geom_area(aes(y = Est.Staff.Act), alpha = .25, fill = "#4C6788") +
  theme_behindbars() +
  labs(y = "Estimated Active Cases") +
  ggtitle("Monitoring Facility Outbreaks", str_to_title(facility_sel))
ggsave("ca_hcf_filtered.svg", ca_hcf_filtered, width = 14, height = 10)

# option 2
facility_sel <- "CALIFORNIA CORRECTIONAL CENTER" # compelling, especially november forwards
ca_corr <- ca %>%
  filter(Name == facility_sel) %>%
  filter(Date > as.character.Date("2020-10-31")) %>%
  mutate(
    Est.Res.Act = diff_roll_sum(
      Residents.Confirmed, Date, window = days_active),
    Est.Staff.Act = diff_roll_sum(
      Staff.Confirmed, Date, window = days_active)) %>%
  filter(Est.Staff.Act > 0) %>%
  ggplot(aes(x = Date)) +
  geom_line(aes(y = Est.Res.Act), color = "#D7790F", size = 1.5) +
  geom_line(aes(y = Est.Staff.Act), color = "#4C6788", size = 1.5) +
  geom_area(aes(y = Est.Res.Act), alpha = .25, fill = "#D7790F") +
  geom_area(aes(y = Est.Staff.Act), alpha = .25, fill = "#4C6788") +
  theme_behindbars() +
  labs(y = "Estimated Active Cases") +
  ggtitle("Monitoring Facility Outbreaks", str_to_title(facility_sel))
ggsave("ca_corr.svg", ca_corr)

# option 3
facility_sel <- "AVENAL STATE PRISON" # this one's strange bc outbreak got under control among incarcerated pop but not staff 
ca %>%
  filter(Name == facility_sel) %>%
  filter(Date > as.character.Date("2020-10-31")) %>%
  mutate(
    Est.Res.Act = diff_roll_sum(
      Residents.Confirmed, Date, window = days_active),
    Est.Staff.Act = diff_roll_sum(
      Staff.Confirmed, Date, window = days_active)) %>%
  filter(Est.Staff.Act > 0) %>%
  ggplot(aes(x = Date)) +
  geom_line(aes(y = Est.Res.Act), color = "#D7790F", size = 1.5) +
  geom_line(aes(y = Est.Staff.Act), color = "#4C6788", size = 1.5) +
  geom_area(aes(y = Est.Res.Act), alpha = .25, fill = "#D7790F") +
  geom_area(aes(y = Est.Staff.Act), alpha = .25, fill = "#4C6788") +
  theme_behindbars() +
  labs(y = "Estimated Active Cases") +
  ggtitle("Monitoring Facility Outbreaks", str_to_title(facility_sel))

## active cases (residents vs staff): New Jersey
nj %>% arrange(desc(Staff.Confirmed)) %>% View()

facility_sel <- "SOUTH WOODS STATE PRISON"
facility_sel <- "EAST JERSEY STATE PRISON" # could try to smooth staff on this one
facility_sel <- "GARDEN STATE YOUTH CORRECTIONAL FACILITY"

nj %>%
  filter(Name == facility_sel) %>%
  mutate(
    Est.Res.Act = diff_roll_sum(
      Residents.Confirmed, Date, window = days_active),
    Est.Staff.Act = diff_roll_sum(
      Staff.Confirmed, Date, window = days_active)) %>%
  # filter(Est.Staff.Act > 0,
  #        Est.Res.Act > 0) %>%
  filter(Est.Res.Act > 0 ) %>%
  ggplot(aes(x = Date)) +
  geom_line(aes(y = Est.Res.Act), color = "red", size = 1.5) +
  geom_line(aes(y = Est.Staff.Act), color = "blue", size = 1.5) +
  labs(y = "Estimated Active Cases") +
  ggtitle("Monitoring Facility Outbreaks", str_to_title(facility_sel))

## 2 idea: deaths at AVENAL STATE 

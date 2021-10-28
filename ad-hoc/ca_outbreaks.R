library(tidyverse)
library(behindbarstools)

state_df <- read_csv(stringr::str_c(
    "https://raw.githubusercontent.com/uclalawcovid19behindbars/data/master/", 
    "historical-data/historical_state_counts.csv"
))

ca_staff_pop <- read_csv(stringr::str_c(
    "https://raw.githubusercontent.com/themarshallproject/COVID_prison_data/master/", 
    "data/staff_populations.csv")) %>%
    filter(month == "july" & name == "California") %>%
    pull(pop)
    
ca_df <- state_df %>%
    filter(State == "California") %>%
    mutate(Staff.Population = ca_staff_pop) %>%
    mutate(Res.Active.df = diff_roll_sum(Residents.Confirmed, Date)) 

ca_res_active_plot <- ca_df %>% 
    filter(!is.na(Residents.Active)) %>%
    ggplot(aes(x = Date, 
               y = Residents.Active)) +
    geom_line(color = "#4C6788") + 
    geom_point(size = 1.0) +
    scale_x_date(date_breaks = "2 month", date_labels =  "%b %y")

ca_staff_active_plot <- ca_df %>% 
    filter(!is.na(Staff.Active)) %>%
    ggplot(aes(x = Date, 
               y = Staff.Active)) +
    geom_line(color = "#4C6788") + 
    geom_point(size = 1.0) +
    scale_x_date(date_breaks = "2 month", date_labels =  "%b %y") + 
    ## date of vaccine mandate (?)
    geom_vline(xintercept = as.Date("2021-08-19")) 

## check staff active counts on specific dates
ca_df %>% 
    select(Date, Staff.Active) %>%
    View()

## check facility-level data
ca_facs <- read_scrape_data(all_dates = TRUE, state = "California")

plot_recent_fac_increases(ca_facs, plot_days = 30)

outbreak_facs <- c("NORTH KERN STATE PRISON", "WASCO STATE PRISON")

## check the latest numbers
ca_facs %>%
    filter(Name %in% outbreak_facs) %>%
    group_by(Name) %>%
    filter(Date == max(Date)) %>%
    ungroup() %>%
    select(Date, Name, Residents.Confirmed, Residents.Active, Staff.Confirmed, 
           Staff.Active, Residents.Deaths, Staff.Deaths) %>%
    View()

## check facility-specific historical data
ca_facs %>%
    filter(Name == outbreak_facs[1]) %>%
    select(Date, Name, Residents.Active, Staff.Active) %>%
    View()

ca_facs %>%
    filter(Name == outbreak_facs[2]) %>%
    select(Date, Name, Residents.Active, Staff.Active) %>%
    View()

plot_fac_trend(scrape_df = kern_county, 
               fac_name = "Kern County General Population",
               state = "California",
               metric = "General.Confirmed",
               annotate = TRUE,
               auto_label = TRUE,
               plot_days = 90)

## north kern 
plot_fac_trend(scrape_df = ca_facs, 
               fac_name = "NORTH KERN STATE PRISON",
               state = "California",
               metric = "Residents.Confirmed",
               annotate = TRUE,
               auto_label = TRUE,
               plot_days = 30)

# wasco 
plot_fac_trend(scrape_df = ca_facs, 
               fac_name = "WASCO STATE PRISON",
               state = "California",
               metric = "Residents.Confirmed",
               annotate = TRUE,
               auto_label = TRUE,
               plot_days = 90)

# herlong
plot_fac_trend(scrape_df = ca_facs, 
               fac_name = "HERLONG FEDERAL CORRECTIONAL INSTITUTION",
               state = "California",
               metric = "Residents.Confirmed",
               annotate = TRUE,
               auto_label = TRUE,
               plot_days = 90)

## residents active
## north kern 
plot_fac_trend(scrape_df = ca_facs, 
               fac_name = "NORTH KERN STATE PRISON",
               state = "California",
               metric = "Residents.Active",
               annotate = TRUE,
               auto_label = TRUE,
               plot_days = 30)

# wasco 
plot_fac_trend(scrape_df = ca_facs, 
               fac_name = "WASCO STATE PRISON",
               state = "California",
               metric = "Residents.Active",
               annotate = TRUE,
               auto_label = TRUE,
               plot_days = 90)

## staff active
## north kern 
plot_fac_trend(scrape_df = ca_facs, 
               fac_name = "NORTH KERN STATE PRISON",
               state = "California",
               metric = "Staff.Active",
               annotate = TRUE,
               auto_label = TRUE,
               plot_days = 90)

# wasco 
plot_fac_trend(scrape_df = ca_facs, 
               fac_name = "WASCO STATE PRISON",
               state = "California",
               metric = "Staff.Active",
               annotate = TRUE,
               auto_label = TRUE,
               plot_days = 90)

## check kern county outbreaks
kern_county <- get_genpop_covid(county = "Kern", state = "California")

kern <- kern_county %>%
    mutate(Gen.Active.df = diff_roll_sum(General.Confirmed, Date))

## shows the major jump in cases starting in early august
kern %>% 
    filter(!is.na(Gen.Active.df)) %>%
    filter(Date > as.Date("2021-05-01")) %>%
    ggplot(aes(x = Date, 
               y = Gen.Active.df)) +
    geom_line(color = "#4C6788") + 
    geom_point(size = 1.0) +
    scale_x_date(date_breaks = "2 month", date_labels =  "%b %y")

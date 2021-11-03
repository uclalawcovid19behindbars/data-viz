library(tidyverse)
library(behindbarstools)

raw_dat <- behindbarstools::read_scrape_data(all_dates = TRUE, state = "California")

bay_area_long <- c(-123, -121.5871)
bay_area_lat <- c(37, 38.2033)

bay_area_dat <- raw_dat %>%
    filter(Latitude >= bay_area_lat[1] & Latitude <= bay_area_lat[2],
           Longitude >= bay_area_long[1] & Longitude <= bay_area_long[2])

## check out sacramento county jail
raw_dat %>%
    filter(Name == "SACRAMENTO COUNTY JAIL") %>% 
    select(Date, Residents.Population)

## where is the largest outbreak in a california state prison over the past month?
raw_dat %>%
    filter(Jurisdiction == "state") %>% 
    plot_recent_fac_increases(., 
                              metric = "Residents.Active", 
                              plot_days = 30, 
                              num_fac = 2)

## where is population on the rise in the bay area over the past month?
plot_recent_fac_increases(bay_area_dat, 
                          metric = "Residents.Population", 
                          plot_days = 30, 
                          num_fac = 2)

plot_fac_trend("SANTA CLARA COUNTY JAIL", "California", "Residents.Population") +
    scale_x_date(date_labels = "%m/%y") +
    labs(x = "Population") 

## residents.population 

bay_area_dat %>% 
    filter(Name == "CALIFORNIA STATE PRISON SAN QUENTIN") %>%
    select(Date, Name, Residents.Initiated, Residents.Population,
           Staff.Confirmed, Staff.Initiated, Staff.Population) %>% 
    mutate(res_vax_pct = Residents.Initiated / Residents.Population,
           staff_vax_pct = Staff.Initiated / Staff.Population) %>%
    View()

## residents.confirmed / residents.active

## no staff or vax data here
bay_area_dat %>% 
    filter(Name == "SANTA CLARA COUNTY JAIL") %>%
    # select(Date, Name, Residents.Initiated, Residents.Active, Residents.Population,
    #        Staff.Confirmed, Staff.Initiated, Staff.Population, Capacity) %>% 
    select(Date, Name, Residents.Active, Residents.Population,
           Capacity) %>%
    # mutate(res_vax_pct = Residents.Initiated / Residents.Population,
    #        staff_vax_pct = Staff.Initiated / Staff.Population) %>%
    View()

bay_area_dat %>% 
    filter(Name == "SAN FRANCISCO COUNTY JAIL") %>%
    select(Date, Name, Residents.Initiated, Residents.Active, Residents.Population,
           Staff.Confirmed, Staff.Initiated, Staff.Population, Capacity) %>% 
    mutate(res_vax_pct = Residents.Initiated / Residents.Population,
           staff_vax_pct = Staff.Initiated / Staff.Population) %>%
    View()


### check out sf county jails population over time 
read_sf_raw <- function() {
    z = "1F2iSIveA0jglb2SILgN4fobYozSMNS_Ff-7x8OGs7tM" %>%
    googlesheets4::read_sheet()

    # for some reason this is saved weird and we need to do some
    # minimal edits in order to save the file
    for(c in names(z)){
        for(j in 1:length(z[[c]])){
            if(length(z[[c]][[j]]) == 0){
                z[[c]][[j]] <- NA
            }
        }
    }
    z %>%
        mutate_all(unlist) %>%
        janitor::clean_names(case = "title") %>%
        mutate(Date = lubridate::mdy(`As of Date`),
               Name = "SAN FRANCISCO COUNTY JAIL",
               State = "California") %>%
        select(
            Date, 
            Name,
            State,
            Residents.Confirmed = `Confirmed Cases Incarcerated Population Cumulative`,
            Residents.Active = `Active Cases Incarcerated Population Current`,
            Residents.Tadmin = `Tests Incarcerated Population Cumulative`, 
            Residents.Population = `Population Incarcerated Population Current`)
} 

sf_all <- read_sf_raw()

plot_fac_trend("SAN FRANCISCO COUNTY JAIL", "California", "Residents.Tadmin", 
               scrape_df = sf_all) +
    scale_x_date(date_labels = "%m/%y") +
    labs(x = "Population") 


sac_county_jail <- raw_dat %>%
    filter(Name == "SACRAMENTO COUNTY JAIL")

north_kern <- raw_dat %>%
    filter(Name == "NORTH KERN STATE PRISON") 

north_kern %>%
    select(Date, Residents.Active, Residents.Confirmed, Staff.Active, Residents.Population, Capacity) %>%
    View()

raw_dat %>%
    filter(Name == "CALIFORNIA CITY CORRECTIONAL FACILITY") %>%
    select(Date, Residents.Active, Residents.Confirmed, Staff.Active, Residents.Population, Capacity) %>%
    View()

str <- "California City Correctional Facility"

highlight_facs <- c("NORTH KERN STATE PRISON", "WASCO STATE PRISON", "LOS ANGELES JAILS")


la_jails <- raw_dat %>%
    filter(Name == "LOS ANGELES JAILS") 

la_jails %>%
    select(Date, Residents.Active, Residents.Confirmed, Staff.Active, Residents.Population, Capacity) %>%
    View()

latest_highlight <- raw_dat %>% 
    filter(Name %in% highlight_facs) %>%
    group_by(Name) %>%
    arrange(Date) %>% 
    mutate(staff_active_df = diff_roll_sum(Staff.Confirmed),
           res_active_df = diff_roll_sum(Residents.Confirmed)) %>%
    filter(Date == max(Date)) %>%
    ungroup() %>%
    mutate(res_vax = Residents.Initiated/Residents.Population,
           staff_vax = Staff.Initiated/Staff.Population) %>%
    select(Facility.ID, Name, Date, source, 
           starts_with("Residents"), starts_with("Staff"),
           starts_with("res"), starts_with("staff")
           )

statecounts <- read_csv("https://raw.githubusercontent.com/uclalawcovid19behindbars/data/master/historical-data/historical_state_counts.csv")

ca <- statecounts %>%
    filter(State == "California") %>%
    filter(Date > "2021-04-29") %>%
    filter(Date < "2021-08-01")

write_csv(ca, "~/Desktop/ca_summer21.csv")

View(ca)



















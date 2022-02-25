library(tidyverse)
library(behindbarstools)
library(lubridate)

scrape_df <- read_scrape_data(all_dates = TRUE)
ice_nys <- scrape_df %>%
    filter(Jurisdiction == "immigration",
           State == "New York") %>%
    mutate(Residents.Active.Pct = Residents.Active / Population.Feb20)

# questions ------------------------------------------------------------------

## what facilities do we have data for?
ice_nys %>% 
    select(Name) %>% 
    unique()

## how many people have been infected by facility?
ice_nys %>%
    group_by(Name) %>%
    summarise(last_n = last(Residents.Confirmed),
              last_pop = last(Population.Feb20)) %>%
    ungroup() %>%
    mutate(p_confirmed = last_n / last_pop)

## what was the peak number of cases at each facility, and when did that occur?
## what was the peak % of population infected since COVID started? 
ice_nys %>%
    group_by(Name) %>%
    mutate(max_active = max(Residents.Active, na.rm = TRUE)) %>%
    ungroup() %>%
    filter(Residents.Active == max_active) %>%
    ungroup() %>%
    select(Date, Name, Residents.Active, Residents.Active.Pct, Residents.Confirmed, Residents.Deaths)

## what was the peak during omicron?
ice_nys %>%
    filter(Date > as.Date("2021-11-30")) %>%
    group_by(Name) %>%
    mutate(max_active = max(Residents.Active, na.rm = TRUE)) %>%
    ungroup() %>%
    filter(Residents.Active == max_active) %>%
    ungroup() %>%
    select(Date, Name, Residents.Active, Residents.Active.Pct, Residents.Confirmed, Residents.Deaths)

## what was the percentage increase since the start of omicron?
ice_nys %>%
    filter(Date > as.Date("2021-11-30")) %>%
    group_by(Name) %>%
    arrange(Date) %>%
    summarise(first = first(Residents.Confirmed),
              last = last(Residents.Confirmed)) %>%
    ungroup() %>%
    mutate(pct_change = (last - first) / last)

## how do infections in ICE compare to US overall? 
gen_df <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/" %>%
    str_c("us.csv") %>%
    read_csv(col_types = cols()) %>%
    select(Date = date, Confirmed = cases, Deaths = deaths) %>%
    arrange(Date) %>%
    # not necessary but a good sanity check
    distinct(Date, .keep_all = TRUE) %>%
    mutate(Active = diff_roll_sum(Confirmed, Date)) %>%
    # us census data
    mutate(Population = 328200000) %>%
    mutate(Name = "US\nPopulation")

ice_gen_compare <- ice_nys %>%
    rename(Active = Residents.Active,
           Population = Population.Feb20,
           Confirmed = Residents.Confirmed) %>%
    # combine with US general population data
    bind_rows(gen_df) %>%
    mutate(Percent = Active / Population,
           Name = ifelse(Name == "ICE ORANGE COUNTY JAIL",
                         "Orange County Jail",
                         Name),
           Name = ifelse(Name == "ICE BUFFALO BATAVIA SERVICE PROCESSING CENTER",
                         "Batavia (Buffalo SPC)",
                         Name)
    )

wide_ice_gen_compare <- ice_gen_compare %>% 
    select(Name, Date, Confirmed, Active, Percent) %>%
    pivot_wider(names_from = Name, 
                names_glue = "{Name}_{.value}",
                values_from = c(Active, Percent, Confirmed)) %>%
    arrange(desc(`US\nPopulation_Percent`))
View(wide_ice_gen_compare)

# Percent of time in outbreak by facility?
ice_nys %>% 
    group_by(Name) %>% 
    mutate(outbreak_ = ifelse(Residents.Active >= 5, 1, 0)) %>% 
    summarise(n_outbreak = sum(outbreak_, na.rm = TRUE), 
              n = n(), 
              pct = n_outbreak / n) 

# How fast outbreaks erupt?
speed <- ice_nys %>% 
    mutate(outbreak_ = ifelse(Residents.Active >= 5, 1, 0), 
           worst_ = ifelse(Residents.Active.Pct >= 0.05, 1, 0)) %>% 
    group_by(Name) %>%
    arrange(Date) %>%
    mutate(outbreak_start_ = ifelse(outbreak_ == 1 & lag(outbreak_) == 0, 1, 0), 
           outbreak_end_ = ifelse(outbreak_ == 1 & lead(outbreak_) == 0, 1, 0), 
           worst_start_ = ifelse(worst_ == 1 & lag(worst_) == 0, 1, 0), 
           worst_end_ = ifelse(worst_ == 1 & lead(outbreak_) == 0, 1, 0)) %>% 
    ungroup() %>%
    filter((outbreak_start_ == 1) | (worst_start_ == 1))

start_dates_list <- c()

for(i in 1:nrow(speed)) {
    row <- speed[i,]
    
    if (row$outbreak_start_ == 1){
        new_start_date_ <- row$Date
    }
    start_dates_list <- append(start_dates_list, new_start_date_)
}

worst <- speed %>% 
    mutate(outbreak_start_date_ = start_dates_list) %>% 
    filter(worst_start_ == 1) %>% 
    mutate(time_ = as.duration(Date - outbreak_start_date_) / ddays(1)) %>% 
    distinct(Name, outbreak_start_date_, .keep_all = TRUE) %>%
    select(Name, outbreak_start_date_, Residents.Active, Residents.Active.Pct,
           time_) %>%
    arrange(Name, outbreak_start_date_)

# Average time 
median(worst$time_)
mean(worst$time_)

## how has the population in NY ICE detention centers changed over time?
pop_df <- tribble(
    ~Name, ~Pop_20, ~Pop_21, ~Pop_22,
    "Batavia", 369  ,     251   ,    279,
    "Orange County", 100, 79 , 144
)

# graphs ------------------------------------------------------------------

## percent active infections
ice_nys %>% 
    ggplot(aes(x = Date, 
               y = Residents.Active.Pct, 
               color = Name, 
               group = Name)) + 
    geom_line(size = 1.0) +
    facet_wrap(~Name, nrow=2, scales = "free") + 
    theme_behindbars(base_size = 18, base_color = "black") + 
    labs(y = "Percent Active COVID-19 cases") + 
    scale_color_bbdiscrete() +
    theme(legend.position = "none",
          legend.title = element_blank()) + 
    scale_y_continuous(labels = scales::percent_format(accuracy = 1))    

## ice facilities compared to us overall
ice_gen_compare %>%
    filter(Date >= "2021-01-01") %>%
    ggplot(aes(x=Date, y=Percent, color = Name)) +
    geom_line(size = 2) +
    # geom_area(alpha=0.4) +
    theme_behindbars(base_color = "black") +
    scale_color_bbdiscrete() +
    theme(legend.position="top") + 
    labs(title = "Comparison of ICE and US Population with Active Infections", 
         y = "Percent of Population Actively Infected") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
    labs(color = "")

# deaths --> none reported 
ice_nys %>% 
    ggplot(aes(x = Date, 
               y = Residents.Deaths, 
               color = Name, 
               group = Name)) + 
    geom_line(size = 1.0) +
    facet_wrap(~Name, nrow=2, scales = "free") + 
    theme_behindbars(base_size = 18, base_color = "black") + 
    labs(y = "COVID Deaths Among People Detained") + 
    scale_color_bbdiscrete() +
    theme(legend.position = "none",
          legend.title = element_blank())  



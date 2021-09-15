library(tidyverse)
library(behindbarstools)

## read facility-level data
raw_dat <- behindbarstools::read_scrape_data(all_dates = TRUE, state = "Georgia")
ga_state <- raw_dat %>%
    filter(Jurisdiction == "state",
           Age != "Juvenile")
latest_ga <- ga_state %>%
    filter(Date == max(Date))
latest_ga$Residents.Initiated / latest_ga$Residents.Population

## read statewide data
historical_statewide <- read_csv("https://raw.githubusercontent.com/uclalawcovid19behindbars/data/master/historical-data/historical_state_counts.csv")
ga_prison_pop <- behindbarstools::read_mpap_pop_data() %>%
    filter(State == "Georgia") %>%
    pull(Population.Feb20)
ga_statewide <- historical_statewide %>%
    filter(State == "Georgia") %>%
    ## NB: denominator is feb 2020 prison population!
    mutate(res_active_df = diff_roll_sum(Residents.Confirmed),
           res_active_dfr = res_active_df / ga_prison_pop,
           res_cumulative_rate = Residents.Confirmed / ga_prison_pop) %>%
    ## diff_roll_sum() still makes estimate from NA data
    filter(Date <= as.Date("2021-07-11"))

## get general population data
ga_general <- behindbarstools::get_genstate_covid() %>%
    filter(State == "Georgia") %>%
    mutate(gen_active_df = diff_roll_sum(General.Confirmed),
           gen_active_dfr = gen_active_df / General.Population,
           gen_cumulative_rate = General.Confirmed / General.Population)

ga_statewide_df <- ga_general %>%
    left_join(ga_statewide, by = "Date")

## plot of estimated active cases over time
ga_statewide_df %>% 
    filter(!is.na(res_active_dfr)) %>%
    ggplot() + 
    ## blue = state pop
    geom_line(aes(x = Date, y = gen_active_dfr), size = 1.0, color = "#4C6788") + 
    ## orange = prison 
    geom_line(aes(x = Date, y = res_active_dfr), size = 1.0, color = "#D7790F") + 
    theme_behindbars() + 
    theme(
        axis.ticks.y = element_line(color = "#555526"), 
        axis.title.y = element_blank(), 
        axis.line.y = element_line(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_blank()) + 
    geom_vline(xintercept = as.Date("2021-07-16"), size = 2.0, color = "#000000") + 
    scale_x_date(date_breaks = "2 month", date_labels =  "%b %y") 

ggsave("laj_pop.png", laj_pop, width = 9, height = 5)
ggsave("laj_pop.svg", laj_pop, width = 10, height = 8)
rm(list=ls())
library(tidyverse)
library(behindbarstools)
library(lubridate)
library(plotly)

state_df <- calc_aggregate_counts(all_dates = TRUE, state = TRUE)

# average CFR
"https://raw.githubusercontent.com/uclalawcovid19behindbars/data/" %>%
    str_c("master/latest-data/state_aggregate_counts.csv") %>%
    read_csv(col_types = cols()) %>% select(Residents.Confirmed, Residents.Deaths) %>% na.omit() %>% summarise_all(sum) %>% 
    mutate(CFR = Residents.Deaths / Residents.Confirmed*1000)

"https://raw.githubusercontent.com/uclalawcovid19behindbars/data/" %>%
    str_c("master/latest-data/state_aggregate_counts.csv") %>%
    read_csv(col_types = cols()) %>%
    mutate(CFR = Residents.Deaths / Residents.Confirmed*1000) %>%
    select(State, CFR) %>%
    filter(!is.na(CFR)) %>%
    mutate(State = fct_reorder(State, CFR)) %>%
    mutate(IsAL = State != "Alabama") %>%
    ggplot(aes(x = State, y = CFR, xend = State, yend = 0, color=IsAL)) + 
    geom_point(size=3) +
    geom_segment(size = 1.5) +
    coord_flip() +
    theme_behindbars() +
    theme(
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(color = "#555526", size = 13),
        axis.text.x = element_text(color = "#555526", size = 18),
        panel.grid.major.x = element_line(
            color = "#92926C", linetype = "dotted"),
        axis.title.x = element_text(margin = margin(r = 20)),
        axis.title.y = element_blank(),
        legend.position = "none") +
    scale_color_bbdiscrete() +
    ylab("COVID Deaths Per 1000 Cases")

al_fac_df <- read_scrape_data(all_dates = TRUE, state = "Alabama")

al_pop_df <- al_fac_df %>%
    filter(Jurisdiction == "state" & Date >= ymd("2020-04-08")) %>%
    group_by(Date) %>%
    summarize(Residents.Population = sum_na_rm(Residents.Population))

al_gen_df <- "https://data.cdc.gov/resource/9mfq-cb36.csv?state=AL" %>%
    read_csv(col_types = cols()) %>%
    mutate(Date = as.Date(submission_date)) %>%
    select(Date, Confirmed = tot_cases, Deaths = tot_death) %>%
    arrange(Date) %>%
    # not necessary but a good sanity check
    distinct(Date, .keep_all = TRUE) %>%
    mutate(Active = diff_roll_sum(Confirmed, Date)) %>%
    mutate(Population = 4903000) %>%
    mutate(name = "AL State\nPopulation")

al_all_df <- state_df %>%
    filter(State == "Alabama") %>%
    filter(Measure %in% c("Residents.Confirmed")) %>%
    mutate(Val = ifelse(is.na(MP), UCLA, MP)) %>%
    left_join(al_pop_df, by = "Date") %>%
    arrange(Date) %>%
    mutate(Residents.Population = last_not_na(Residents.Population)) %>%
    select(Date, Confirmed = Val, Population = Residents.Population) %>%
    mutate(Active = diff_roll_sum(Confirmed, Date)) %>%
    mutate(name = "AL DOC Prison\nPopulation") %>%
    bind_rows(al_gen_df) %>%
    mutate(Active.Rate = Active / Population)

al_alld_df <- state_df %>%
    filter(State == "Alabama") %>%
    filter(Measure %in% c("Residents.Deaths")) %>%
    mutate(Val = ifelse(is.na(MP), UCLA, MP)) %>%
    left_join(al_pop_df, by = "Date") %>%
    arrange(Date) %>%
    mutate(Residents.Population = last_not_na(Residents.Population)) %>%
    select(Date, Deaths = Val, Population = Residents.Population) %>%
    mutate(name = "AL DOC Prison\nPopulation") %>%
    bind_rows(al_gen_df) %>%
    mutate(Death.Rate = Deaths / Population * 100000)

al_alld_df %>%
    ggplot(aes(x=Date, y = Death.Rate, color = name)) +
    geom_line(size = 2) +
    theme_behindbars() +
    scale_color_bbdiscrete() +
    labs(color = "", y = "COVID Deaths Per\n100K People") +
    scale_x_date(breaks = scales::pretty_breaks(n = 10))

al_alld_df %>%
    group_by(name) %>% 
    filter(Date == max(Date)) %>%
    pull(Death.Rate) %>%
    {.[1]/.[2]}

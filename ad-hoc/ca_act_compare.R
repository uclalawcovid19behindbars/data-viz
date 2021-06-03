library(tidyverse)
library(lubridate)
library(behindbarstools)

agg_df <-
    calc_aggregate_counts(state = TRUE, all_dates = TRUE)

collap_df <- agg_df %>%
    filter(Measure %in% c("Residents.Active", "Staff.Active")) %>%
    group_by(State, Date) %>%
    filter(n() == 2) %>%
    group_by(Measure, Date) %>%
    summarize(Val = sum(Val), N = n())

collap_df %>%
    filter(Date >= ymd("2021-04-20")) %>%
    group_by(Date) %>%
    summarise(Ratio = Val[1]/Val[2]) %>%
    ggplot(aes(x=Date, y = Ratio)) +
    geom_line()

sanaly <- "California"

bind_rows(
    agg_df %>%
        filter(State == sanaly & Measure == "Staff.Active") %>%
        #mutate(Staff.New = diff_roll_sum(UCLA, Date)) %>%
        mutate(Population = filter(
            agg_df,
            State==sanaly & Measure=="Staff.Population" & Date==max(Date)
            )$Val) %>%
        mutate(NCR = Val/Population*10000) %>%
        select(Date, NCR) %>%
        mutate(Name = "CDCR\nStaff"),

    agg_df %>%
        filter(State == sanaly & Measure == "Residents.Active") %>%
        #mutate(Staff.New = diff_roll_sum(UCLA, Date)) %>%
        mutate(Population = filter(
            agg_df,
            State==sanaly & Measure=="Residents.Population" & Date==max(Date)
        )$Val) %>%
        mutate(NCR = Val/Population*10000) %>%
        select(Date, NCR) %>%
        mutate(Name = "CDCR\nPrisoners"),

    "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv" %>%
        read_csv(col_types = cols()) %>%
        filter(state == sanaly) %>%
        rename(Date = date) %>%
        arrange(Date) %>%
        mutate(New.Cases = diff_roll_sum(cases, Date)) %>%
        mutate(Population = 39510000) %>%
        mutate(NCR = New.Cases/Population*10000) %>%
        select(Date, NCR) %>%
        mutate(Name = "California")) %>%
    arrange(Date, Name) %>%
    filter(!is.na(NCR)) %>%
    group_by(Date) %>%
    filter(n() == 3) %>%
    ungroup() %>%
    filter(Date == max(Date)) %>%
    ggplot(aes(x=Name, y=NCR, fill = Name)) +
    geom_col() +
    theme_behindbars() +
    scale_fill_bbdiscrete() +
    theme(legend.position = "none") +
    labs(y = "Active Case Rate\nPer 10,000")


new_scrape <- read_scrape_data(state = "California")

new_scrape %>%
    select(Name, Staff.Population, Staff.Completed, Staff.Active) %>%
    na.omit() %>%
    mutate(Vax= Staff.Completed/Staff.Population) %>%
    mutate(Act = Staff.Active/Staff.Population) %>%
    ggplot(aes(x=Vax, y=Act)) +
    geom_point() +
    geom_smooth() +
    labs(x = "Vaccination Proportion", y = "Active Proportion")

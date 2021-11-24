rm(list=ls())
library(tidyverse)
library(behindbarstools)
library(lubridate)

# get general pop data for Cook county Illinois based on fips code
gen_il_df <- get_genpop_covid(17031)
# get carceral data for the state of Illinois
all_df <- read_scrape_data(TRUE, state = "Illinois")

all_df %>%
    # only look at cook county jail
    filter(Facility.ID == 454) %>%
    # trim the data down
    select(
        Date, Active = Residents.Active, Pop = Population.Feb20) %>%
    mutate(Group = "Cook County\nJail Population") %>%
    # pull in the gen population data
    bind_rows(
        gen_il_df %>%
            arrange(Date) %>%
            mutate(Active = diff_roll_sum(General.Confirmed, Date)) %>%
            select(Date, Active, Pop = General.Population) %>%
            mutate(Group = "Cook County\nOverall Population")
    ) %>%
    # remove missing values
    arrange(Group, Date) %>%
    na.omit() %>%
    # only look at recent dates
    filter(Date >= ymd("2021-10-01")) %>%
    # calculate rate
    mutate(Rate = Active/Pop * 100000) %>%
    # make a purty plot
    ggplot(aes(x = Date, y = Rate, color = Group)) +
    geom_line(size = 2) +
    theme_behindbars() +
    scale_color_bbdiscrete() +
    labs(color = "", y = "Active Cases\nPer 100,000")

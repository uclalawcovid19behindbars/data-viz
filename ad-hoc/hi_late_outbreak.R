rm(list=ls())
library(tidyverse)
library(behindbarstools)
library(lubridate)


all_df <- read_scrape_data(all_dates = TRUE)

all_df %>%
    filter(State == "Hawaii" & Date == max(Date, na.rm = T)) %>%
    View()

(hi_plot <- all_df %>%
    filter(Facility.ID == 415) %>%
    filter(Date >= ymd("2021-01-20") & Date <= ymd("2021-06-09")) %>%
    ggplot(aes(x=Date, y = Residents.Active)) +
    geom_line(size = 2, color = "#D7790F") +
    geom_area(alpha=.5, fill = "#D7790F") +
    theme_behindbars() +
    ylab("Active COVID Cases\nAmong Prison Population"))

ggsave("~/Downloads/hi_outbreak.svg", hi_plot)

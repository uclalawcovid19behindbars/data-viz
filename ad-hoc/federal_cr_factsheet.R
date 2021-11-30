library(behindbarstools)
library(tidyverse)

agg_month <- calc_aggregate_counts(state = T, week_grouping = FALSE, all_dates = T)
agg_month_fed <- agg_month %>% 
    filter(Measure == "Residents.Active") %>% 
    mutate(Grouping = case_when(State == "ICE" ~ "ICE", 
                                State == "Federal" ~ "Federal", 
                                TRUE ~ "State")) %>% 
    ## comment out this line for stacked bars by jurisdiction
    filter(Grouping == "Federal") %>%
    arrange(Date) %>% 
    mutate(lag = lag(Val),
           perc_change = (Val - lag) / lag)  %>%
    select(State, Date, Val, lag, perc_change)

hist_state <- read_csv("https://raw.githubusercontent.com/uclalawcovid19behindbars/data/master/historical-data/historical_state_counts.csv")
hist_fed <- hist_state %>% filter(State == "Federal")

## create using daily counts of active cases
daily_fed_plot <- hist_fed %>% 
    ## uncomment line below to show all dates
    # filter(Date > as.Date("2020-08-30") & Date < as.Date("2021-01-01")) %>% 
    ggplot() + 
    geom_bar(aes(x = Date, y = Residents.Active), stat = "identity") + 
    theme_behindbars(base_size = 14, base_color = "black") + 
    scale_y_continuous(label = scales::comma) +
    theme(legend.position = "right", legend.title = element_blank()) + 
    scale_fill_manual(values = c("#9DC183", "#664d60", "#D7790F")) + 
    scale_x_date(breaks = scales::pretty_breaks(n = 12)) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    labs(title = "COVID Daily Cases in Federal Prisons", 
         y = "Reported Active Cases")
ggsave("bop_daily.png", daily_fed_plot, width = 7, height = 5)
## version with only winter 2020
ggsave("bop_daily_winter20.png", daily_fed_plot, width = 7, height = 5)

## create using monthly counts of active cases
monthly_fed_plot <- agg_month_fed %>% 
    filter(Date > as.Date("2020-08-01") & Date < as.Date("2021-01-01")) %>% 
    ggplot() + 
    geom_bar(aes(x = Date, y = Val, fill = Grouping), stat = "identity") + 
    theme_behindbars(base_size = 14, base_color = "black") + 
    scale_y_continuous(label = scales::comma) +
    theme(legend.position = "right", legend.title = element_blank()) + 
    scale_fill_manual(values = "#9DC183") + 
    scale_x_date(breaks = scales::pretty_breaks(n = 4)) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    labs(title = "COVID Cases in Federal Prisons", 
         y = "Reported Monthly Cases")
ggsave("bop_montly_winter20.png", monthly_fed_plot, width = 7, height = 5)




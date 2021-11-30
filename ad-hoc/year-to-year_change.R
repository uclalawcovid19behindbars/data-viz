library(behindbarstools)
library(tidyverse)

## LOAD DATA
state_df <- "https://raw.githubusercontent.com/uclalawcovid19behindbars/data/master/historical-data/historical_state_counts.csv" %>% 
    read_csv(col_types = cols()) 
agg_week <- calc_aggregate_counts(state = T, week_grouping = TRUE, all_dates = T)
covid_suffixes <- c(
    ".Confirmed", ".Deaths", ".Tadmin", ".Active", ".Initiated", ".Completed")
covid_suff <- paste(covid_suffixes, collapse = "|")

## TRANSFORM DATA
agg_week_out <- agg_week %>%
    mutate(year = as.character(lubridate::year(Date)),
           week = lubridate::week(Date)) %>% 
    filter(stringr::str_detect(Measure, covid_suff)) %>%
    group_by(State, Measure) %>%
    arrange(Date) %>% 
    mutate(rollavg_measure = zoo::rollmean(Val, k = 3, fill = NA)) %>%
    ungroup()

## CREAT PLOTTING FUNCTION - works for a single metric
plot_lags <- function(dat) {
    transformed_dat <- dat %>%
        arrange(Date) %>%
        mutate(lag = Val - lag(Val)) %>%
        mutate(lag = ifelse(lag < 0, 0, lag),
               dfr = diff_roll_sum(Val)) %>% ## might need to change the window on this to two, not sure
        ## want diff_roll_sum for .Confirmed and lag for all else but .Active
        mutate(val_to_show = case_when(
            str_detect(metric, ".Confirmed") ~ dfr,
            str_detect(metric, ".Active")  ~ Val,
            TRUE ~ lag
        )) %>%
        mutate(rollavg_weeklycase = zoo::rollmean(val_to_show, k = 3, fill = NA))
    plot <- transformed_dat %>%
        ggplot() + 
        geom_line(aes(x = week, y = rollavg_weeklycase, color = year, ), stat = "identity") + 
        theme_behindbars(base_size = 14, base_color = "black") + 
        scale_y_continuous(label = scales::comma) +
        theme(legend.position = "right", legend.title = element_blank()) 
    return(plot)
}

## GENERATE PLOTS FOR EACH STATE FOR A GIVEN METRIC
create_state_plots <- function(dat, metric) {
    dat %>%
        filter(Measure == metric) %>%
        split(.$State) %>%
        imap(function(dat, State) {
            plot_lags(dat)
        }) 
}

## TO DO 
# - add title
# - add metric indicator 
# - save plots somewhere 

agg_week_out %>%
    create_state_plots(metric = "Residents.Confirmed")

library(behindbarstools)
library(tidyverse)
library(zoo)

## LOAD DATA
agg_week <- calc_aggregate_counts(state = T, week_grouping = TRUE, all_dates = T)
covid_suffixes <- c(
    ".Confirmed", ".Deaths", ".Tadmin", ".Active", ".Initiated", ".Completed")
covid_suff <- paste(covid_suffixes, collapse = "|")

## TRANSFORM DATA
# agg_week_out <- agg_week %>%
#     mutate(year = as.character(lubridate::year(Date)),
#            week = lubridate::week(Date),
#            month = lubridate::month(Date, label = T),
#            ) %>% 
#     filter(stringr::str_detect(Measure, covid_suff)) %>%
#     group_by(State, Measure) %>%
#     arrange(Date) %>% 
#     mutate(rollavg_measure = zoo::rollmean(Val, k = 3, fill = NA)) %>%
#     ungroup() %>%
#     arrange(week)

## 
# State, metric, data, plot, perc_increase_since_sept
# Alabama, res.confirmed, 
create_plotting_vars <- function(dat, state, metric) {
    transformed_dat <- dat %>%
        filter(Measure == metric,
               State == state,
               ) %>%
        mutate(year = as.character(lubridate::year(Date)),
               week = lubridate::week(Date),
               month = lubridate::month(Date, label = T),
        ) %>%
        arrange(Date) %>%
        mutate(lag = Val - lag(Val)) %>%
        mutate(lag = ifelse(lag < 0, 0, lag),
               dfr = diff_roll_sum(Val, date_vec = Date)) %>% ## might need to change the window on this to two, not sure
        ## want diff_roll_sum for .Confirmed and lag for all else but .Active
        mutate(val_to_show = case_when(
            str_detect(metric, ".Confirmed") ~ dfr,
            str_detect(metric, ".Active")  ~ Val,
            TRUE ~ lag
        )) %>%
        mutate(rollavg_weeklycase = zoo::rollmean(val_to_show, k = 3, fill = NA)) %>%
        arrange(Date) %>%
        mutate(lag_three_months = lag(Val, n = 12),
               perc_ch_three_months = (Val - lag_three_months)/ lag_three_months)
}

## CREAT PLOTTING FUNCTION - works for a single metric
plot_lags <- function(dat) {
    plot <- dat %>%
        ggplot() + 
        geom_line(aes(x = week, y = rollavg_weeklycase, color = year), 
                  stat = "identity") + 
        theme_behindbars(base_size = 14, base_color = "black") + 
        scale_y_continuous(label = scales::comma) +
        scale_x_continuous(breaks = dat$week, labels = dat$month, n.breaks = 12) + 
        # scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
        theme(legend.position = "right", legend.title = element_blank()) #+ 
        # labs(title = paste(unique(State), unique(Measure)))  
    return(plot)
}

## GENERATE PLOTS
## TO DO: check vaccination rates for these states
###       check illinois
###       add graph titles
agg_week %>% 
    create_plotting_vars(., 
                         state = "ICE",
                         metric = "Residents.Active") %>%
    plot_lags()

ggsave("~/Desktop/ICE_res_active.png", width = 15, height = 8)

agg_week %>% 
    create_plotting_vars(., 
                         state = "Connecticut",
                         metric = "Residents.Confirmed") %>%
    plot_lags()

ggsave("~/Desktop/CT_res_confirmed.png", width = 15, height = 8)

agg_week %>% 
    create_plotting_vars(., 
                         state = "Maryland",
                         metric = "Staff.Confirmed") %>%
    plot_lags()

ggsave("~/Desktop/MD_staff_confirmed.png", width = 15, height = 8)



############################################################
##########
##########   Don't think the stuff below is helpful!
##########
############################################################

## GENERATE PLOTS FOR EACH STATE FOR A GIVEN METRIC
create_state_plots <- function(dat, metric) {
    dat %>%
        filter(Measure == metric) %>%
        split(.$State) %>%
        .[c(1, 2, 3)] %>%
        imap(function(dat, State) {
            plot_lags(dat)
        }) 
}

metric <- "Residents.Confirmed"
testing_dat <- agg_week_out %>% 
    filter(Measure == metric) %>%
    split(.$State) %>%
    .[c(1, 2, 3)]
dat <- testing_dat[[1]]

## TO DO 
# - add title
# - add metric indicator 
# - save plots somewhere 
# - add flag for winter spike starting 2021
# - add parameter to start_date, end_date

dat %>%
    plot_lags()

agg_week_out %>%
    create_state_plots(metric = "Residents.Confirmed")

agg_week_out %>%
    create_state_plots(metric = "Staff.Confirmed")

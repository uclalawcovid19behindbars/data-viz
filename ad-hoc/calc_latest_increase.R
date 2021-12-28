library(tidyverse)
library(behindbarstools)
library(glue)

## calculate latest facility-level and state increases

#' @param metric character string of the metric to calc 
#' @param delta_days integer, number of days to calculate increase over  
#' @param num_fac integer, number of facilities to plot 
#' @param arrange_by grab the top `num_fac` facilities by percentage increase? other option is `raw_change`
#' @param write_data write two CSV files to your desktop with facility and state tables
#' 
#' @return list of dataframes

## code borrowed from: https://github.com/uclalawcovid19behindbars/behindbarstools/blob/master/R/plot_recent_fac_increases.R
track_recent_covid_increases <- function(
    metric = "Residents.Confirmed", 
    delta_days = 14, 
    num_fac = 5,
    arrange_by = "pct_increase",
    write_data = F) {
    
    date <- Sys.Date()
    
    scrape_df <- read_scrape_data(T, T)
    state_df <- read_csv("https://raw.githubusercontent.com/uclalawcovid19behindbars/data/master/historical-data/historical_state_counts.csv")
    latest_scrape_date <-  max(scrape_df$Date)
    delta_start_date <- latest_scrape_date - lubridate::days(delta_days)
    
    fac_data <- scrape_df %>%
        filter(!(stringr::str_detect(Name, "(?i)state") & stringr::str_detect(Name, "(?i)wide"))) %>%
        filter(Date >= delta_start_date) %>%
        # filter(State == "North Carolina") %>%
        group_by(Name, State) %>%
        mutate(start_val = first(!!sym(metric)),
               last_val = last(!!sym(metric)),
               raw_change = last_val - start_val,
               pct_increase = (raw_change / start_val)*100) %>%
        distinct(Facility.ID, Name, State, start_val, last_val, raw_change, pct_increase) %>% 
        filter(start_val > 3) 
    keep_facs_pct_increase <- fac_data %>%
        arrange(desc(pct_increase), Name) %>% 
        mutate(metric_arrange = "pct_increase") %>% 
        head(num_fac) 
    keep_facs_raw_increase <- fac_data %>%
        arrange(desc(raw_change), Name) %>% 
        mutate(metric_arrange = "raw_increase") %>% 
        head(num_fac) 
    ## bind dfs together to get both % increase and raw number jump
    keep_facs <- keep_facs_pct_increase %>%
        bind_rows(keep_facs_raw_increase) %>%
        distinct(Name, State, .keep_all = TRUE)
        
    ## do the same for state
    state_data <- state_df %>%
        filter(Date >= delta_start_date) %>%
        group_by(State) %>%
        mutate(start_val = first(!!sym(metric)),
               last_val = last(!!sym(metric)),
               raw_change = last_val - start_val,
               pct_increase = (raw_change / start_val)*100) %>%
        distinct(State, start_val, last_val, raw_change, pct_increase) 
    keep_states_pct_increase <- state_data %>%
        arrange(desc(pct_increase), State) %>% 
        mutate(metric_arrange = "pct_increase") %>% 
        head(num_fac) 
    keep_states_raw_increase <- state_data %>%
        arrange(desc(raw_change), State) %>% 
        mutate(metric_arrange = "raw_increase") %>% 
        head(num_fac) 
    keep_states <- keep_states_pct_increase %>%
        bind_rows(keep_states_raw_increase) %>%
        distinct(State, .keep_all = TRUE)
    
    if(write_data) {
        write_csv(keep_facs, glue('~/Desktop/highest_fac_increases_{metric}_{date}.csv'))
        write_csv(keep_states, glue("~/Desktop/highest_state_increases_{metric}_{date}.csv"))
    }
    
    out <- lst(facilities = keep_facs, 
               states = keep_states)
    
    return(out)
} 
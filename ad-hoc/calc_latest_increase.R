library(tidyverse)
library(behindbarstools)
library(glue)
library(googlesheets4)

## calculate latest facility-level and state increases

#' @param scrape_df facility-level data to use
#' @param outbreaks_sheet_loc optional, google sheets ID to use if writing to google sheets
#' @param metric character string of the metric to calc 
#' @param delta_days integer, number of days to calculate increase over  
#' @param num_fac integer, number of facilities to plot 
#' @param overwrite_data T/F, write output data to google sheet specified in outbreaks_sheet_loc
#' 
#' @return dataframe with highest state and facility-level outbreaks

## code borrowed from: https://github.com/uclalawcovid19behindbars/behindbarstools/blob/master/R/plot_recent_fac_increases.R
track_recent_covid_increases <- function(
    scrape_df, 
    outbreaks_sheet_loc,
    metric = "Residents.Confirmed", 
    delta_days = 14, 
    num_fac = 5,
    overwrite_data = TRUE) {
    ## define inputs for data filtering
    date <- Sys.Date()
    state_df <- read_csv("https://raw.githubusercontent.com/uclalawcovid19behindbars/data/master/historical-data/historical_state_counts.csv")
    latest_scrape_date <-  max(scrape_df$Date)
    delta_start_date <- latest_scrape_date - lubridate::days(delta_days)
    
    fac_data <- scrape_df %>%
        filter(!(stringr::str_detect(Name, "(?i)state") & stringr::str_detect(Name, "(?i)wide"))) %>%
        filter(Date >= delta_start_date) %>%
        filter(State == "Pennsylvania") %>%
        group_by(Name, State) %>%
        mutate(start_val = first(!!sym(metric)),
               last_val = last(!!sym(metric)),
               raw_change = last_val - start_val,
               pct_increase = (raw_change / start_val)*100) %>%
        distinct(Facility.ID, Name, State, start_val, last_val, raw_change, pct_increase) %>% 
        ## NEED TO CHANGE THIS AWAY FOR .DEATHS
        filter(raw_change > 0) 
    if(str_detect(metric, ".Deaths")) {
        fac_data <- fac_data %>%
            filter(start_val > 1)
    }
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
        distinct(Name, State, .keep_all = TRUE) %>%
        mutate(Name = str_to_title(Name))
    # ## do the same for state
    state_data <- state_df %>%
        filter(Date >= delta_start_date) %>%
        group_by(State) %>%
        mutate(start_val = first(!!sym(metric)),
               last_val = last(!!sym(metric)),
               raw_change = last_val - start_val,
               pct_increase = (raw_change / start_val)*100) %>%
        distinct(State, start_val, last_val, raw_change, pct_increase) %>%
        filter(raw_change > 0)
    if(str_detect(metric, ".Deaths")) {
        fac_data <- fac_data %>%
            filter(start_val > 0)
    }
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
        distinct(State, .keep_all = TRUE) %>%
        mutate(Facility.ID = NA,
               Name = "Statewide") %>%
        relocate(Facility.ID, State, Name)
    
    out <- bind_rows(keep_facs, keep_states) %>%
        mutate(pct_increase = na_if(pct_increase, Inf),
               pct_increase = na_if(pct_increase, -Inf))
    
    ##TO DO: add date to sheet title
    if(overwrite_data){
        range_write(
            data = out, 
            ss = outbreaks_sheet_loc, 
            sheet = glue("{metric}"), 
            reformat = FALSE)
    }
    return(out)
} 

############################
## RUN THE FUNCTION
############################
# define inputs
outbreaks_sheet_loc <- "1I7oubSBZT1GnDL30f4jHzIQwQGso5RulrrBUgxFfRAM" 
scrape_df <- read_scrape_data(T, T)
metrics <- c("Residents.Confirmed", "Residents.Active", 
             "Residents.Deaths", "Staff.Confirmed", "Staff.Deaths")
out <- metrics %>%
    map(~ track_recent_covid_increases(metric = .x, 
                                       scrape_df = scrape_df,
                                       outbreaks_sheet_loc = outbreaks_sheet_loc))
## create some metadata 
all_states <- purrr::transpose(out) %>% 
    .[["State"]] %>%
    unlist() %>%
    as_tibble() %>%
    group_by(value) %>%
    summarise(n = n()) %>%
    arrange(-n) %>%
    filter(n > 1) %>%
    rename(State = value,
           times_flagged = n)

## write the metadata to google sheet
range_write(
    data = all_states, 
    ss = outbreaks_sheet_loc, 
    sheet = glue("Top states"), 
    reformat = FALSE)
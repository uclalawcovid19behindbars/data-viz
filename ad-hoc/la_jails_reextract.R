library(tidyverse)
library(data.table)
library(behindbarstools)

## for testing purposes
test <- tibble(Date = c("2021-09-27", "2021-09-28", "2021-09-29", "2021-09-30", "2021-10-01"),
               Residents.Confirmed = c(4, NA, 4, 5, 5),
               Residents.Deaths = c(1, NA, 2, 2, NA),
               Staff.Confirmed = c(5, NA, 5, NA, 5),
               Staff.Deaths = c(3, 4, NA, NA, 4)
)

### Step 0: Load re-extracted LA Jails data
df <- list.files("~/Desktop/lasd_extract/extracted_data/", full.names = T) %>%
    lapply(function(x){
        df_ <- fread(x)
        if(nrow(df_) == 0){
            df_ <- data.table()
        }
        if("Date" %in% names(df_)){
            df_[,Date := lubridate::as_date(Date)]
        }
        df_
    }) %>%
    rbindlist(fill=TRUE, use.names = TRUE) 

### Step 1: If the lag and lead of an NA value are the same, replace the NA with that value
mutate_na_to_surrounding <- function(df, col) {
    varname_lead <- paste0("drop_lead_", col)
    varname_lag <- paste0("drop_lag_", col)
    varname_is_na <- paste0("drop_na_", col)
    varname_mod <- paste0("drop_mod_", col)
    out <- df
    out[[varname_lag]] <- lag(out[[col]], n = 1)
    out[[varname_lead]] <- lead(out[[col]], n = 1)
    out[[varname_is_na]] <- is.na(out[[col]])
    out[[varname_mod]] <- ifelse((out[[varname_is_na]]) & 
                                 (out[[varname_lead]] == out[[varname_lag]]),
                                 out[[varname_lead]],
                                 out[[col]]
                                 )
    out[[col]] <- out[[varname_mod]]
    out <- out %>%
        select(-starts_with("drop"))
    return(out)
}

numeric_cols <- df %>%
    select(where(is.numeric)) %>%
    names()

out <- df
for (name in numeric_cols){
    out <- mutate_na_to_surrounding(out, name)
}

### Step 2: Check if the value for a cumulative value dropped, and if so, flag it
flag_cumulative_drop <- function(df, col) {
    varname_lag <- paste0("drop_lag_", col)
    varname_is_na <- paste0("drop_na_", col)
    varname_flag <- paste0("flag_", col)
    out <- df
    out[[varname_lag]] <- lag(out[[col]], n = 1)
    out[[varname_is_na]] <- is.na(out[[col]])
    out[[varname_flag]] <- ifelse(!(out[[varname_is_na]]) & 
                                     (out[[col]] < out[[varname_lag]]),
                                 TRUE,
                                 FALSE
    )
    out <- out %>%
        select(-starts_with("drop"))
    return(out)
}

cumulative_cols <- c("Asymptomatic Negative Test Results",
                     "Asymptomatic Positive - Released",
                     "Asymptomatic Total",
                     "Asymptomatic.Total",
                     "Historical Total Total Positive Asymptomatic",
                     "Historical Total Total Positive Symptomatic",
                     "Historical Total Total Positives",
                     "Residents.Deaths",
                     "Residents.Recovered",
                     "Symptomatic Negative Test Results",
                     "Symptomatic Positive - Recovered",
                     "Symptomatic Positive - Released",
                     "Symptomatic Total",
                     "Symptomatic.Total"
                     )

flagged_out <- out
for (name in cumulative_cols){
    flagged_out <- flag_cumulative_drop (flagged_out, name)
}

#### OUTSTANDING OUESTIONS: 
# - what's the diff between `Asymptomatic Total` and `Asymptomatic.Total`?
# - what's the diff between `Symptomatic Total` and `Symptomatic.Total`?

### Step 3: create a flag for any non-cumulative drop 
data_towrite <- flagged_out %>%
    ## add flag indicator for any unexpected drop in cumulative variable
    mutate(n_flags = reduce(select(., starts_with("flag")), vector_sum_na_rm),
           any_flag = ifelse(n_flags > 0, TRUE, FALSE))

### Step 4: Plot each metric over time to visually inspect anomalies
plot_timeseries <- function(dat, y_var) {
    flag_name <- paste0("flag_", y_var)
    p <- dat %>%
        ggplot(aes(x = Date, y = !!sym(y_var))) +
        ## comment out this line for non-cumulative var run
        geom_point(alpha=1 , size=3, aes(color = !!sym(flag_name))) + 
        geom_line(alpha=.6 , size=.5) + 
        labs(x = "Date",
             y = y_var) + 
        scale_x_date(date_minor_breaks = "1 month", date_labels = "%m/%y", 
                     date_breaks = "1 months") + 
        ggtitle(paste0("LA Jails re-scrape: ", y_var))
    return(p)
}

plot_timeseries(data_towrite, "Residents.Deaths")

## save plots for each variable
## all variables (including non-cumulative, commenting out geom_point line in function)
for (name in numeric_cols){
    plot_timeseries(data_towrite, name)
    ggsave(paste0(name, "_plt.png"), last_plot(),
           path = file.path("~", "Desktop", "lasd_extract", "plts"))
}

## expected cumulative variables
for (name in cumulative_cols){
    plot_timeseries(data_towrite, name)
    ggsave(paste0(name, "_plt.png"), last_plot(),
           path = file.path("~", "Desktop", "lasd_extract", "plts"),
           height = 10, width = 15)
}

final_data <- data_towrite %>%
    select(-starts_with("flag_")) %>%
    relocate(Date, any_flag, n_flags, starts_with("Residents.")) %>%
    relocate(where(is.numeric), .before = where(is.character))

write_csv(final_data, 
          file = file.path("~", "Desktop", "lasd_extract", "lasd_reextract.csv"))





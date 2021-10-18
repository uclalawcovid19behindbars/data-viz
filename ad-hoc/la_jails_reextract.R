library(tidyverse)
library(data.table)

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

test <- tibble(Date = c("2021-09-27", "2021-09-28", "2021-09-29", "2021-09-30", "2021-10-01"),
               Residents.Confirmed = c(4, NA, 4, 5, 5),
               Residents.Deaths = c(1, NA, 2, 2, NA),
               Staff.Confirmed = c(5, NA, 5, NA, 5),
               Staff.Deaths = c(3, 4, NA, NA, 4)
               )

# If the lag and lead of an NA value are the same, replace the NA with that value
fill_na_surrounding <- function(dataframe, x) {
    out <- dataframe %>%
        select(Date, x) %>%
        arrange(Date) %>%
        mutate(date_before = lag(x, Date),
               date_after = lead(x, Date),
               ## for na values, check if surrounding values are equal 
               surrounding_dates_equal = ifelse((date_before == date_after) & 
                                                    (is.na(x)),
                                                TRUE, FALSE),
               x = ifelse(surrounding_dates_equal, date_before, x))
    # 
    # summarise(across(starts_with("Sepal"), mean, .names = "mean_{.col}"))
    # 
    # mutate(across(where(is.double) & !c(Petal.Length, Petal.Width), round))
    return(out)
}


# Plot each metric over time to visually inspect anomalies
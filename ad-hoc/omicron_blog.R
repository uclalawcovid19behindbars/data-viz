library(tidyverse)
library(behindbarstools)
library(scales)

## read in data
df <- read_scrape_data(all_dates = TRUE)
hist_state <- read_csv("https://raw.githubusercontent.com/uclalawcovid19behindbars/data/master/historical-data/historical_state_counts.csv")


## for data look-up purposes, only keep facs with non-NA values for res.active
active_df <- df %>%
    filter(!is.na(Residents.Active)) %>%
    mutate(Total.Active = vector_sum_na_rm(Residents.Active, Staff.Active)) 

# ------------------------------------------------------------------------------

## How many prisons have experienced at least one outbreak after december 1st?
active_df %>% 
    select(Date, Facility.ID, Name, State, Jurisdiction, Residents.Active, Staff.Active, Residents.Population, Total.Active) %>% 
    filter(Date > "2021-12-01") %>% 
    group_by(Facility.ID, Jurisdiction) %>% 
    summarise(max_ = max(Total.Active, na.rm = TRUE)) %>% 
    filter(max_ >= 5, 
           Jurisdiction != "county") %>% ## rm jails from this statement
    nrow()

## How many facilities have been in constant outbreak since omicron was first detected?
outbreak_duration <- active_df  %>% 
    filter(Date > "2021-12-01") %>% 
    group_by(Facility.ID, Jurisdiction) %>% 
    mutate(outbreak_ = ifelse(Total.Active >= 5, 1, 0)) %>% 
    summarise(n_outbreak = sum(outbreak_), 
              n = n(), 
              pct = n_outbreak / n) 

outbreak_duration %>%
    filter(pct == 1,
           Jurisdiction != "county") %>%
    nrow()

## what percentage of facilities are experiencing an outbreak right now?
pct_outbreak <- active_df %>% 
    filter(Jurisdiction != "county") %>%
    group_by(Date) %>% 
    mutate(outbreak_ = ifelse(Total.Active >= 5, 1, 0)) %>% 
    summarise(n_outbreak = sum(outbreak_), 
              n = n(), 
              pct = n_outbreak / n)
View(pct_outbreak)

## How many deaths in prison in January? 
agg_dat <- calc_aggregate_counts(all_dates = TRUE) 

deaths_over_time <- agg_dat %>% 
    filter(Measure == "Residents.Deaths") %>% 
    select(Date, Count) %>%
    mutate(lag_count = lag(Count),
           lag_change = Count - lag_count)

staff_deaths_over_time <- agg_dat %>% 
    filter(Measure == "Staff.Deaths") %>% 
    select(Date, Count) %>%
    mutate(lag_count = lag(Count),
           lag_change = Count - lag_count)

agg_dat %>%
    filter(Date == as.Date("2022-01-02") | Date == max(Date),
            Measure == "Residents.Deaths" | Measure == "Staff.Deaths") %>%
    group_by(Date, Measure) %>%
    summarise(ntl_sum = sum(Count),
              n = sum(Reporting))

# ------------------------------------------------------------------------------

# Share of facilities with outbreaks over time 
outbreaks_overtime <- pct_outbreak %>%
    filter(n > 150) %>%
    ggplot(aes(x = Date, y = pct)) + 
    geom_line(size = 1.0, color = "#4C6788") + 
    scale_y_continuous(limits = c(0, 1), labels = percent) +
    scale_x_date(breaks = pretty_breaks(n = 6), label = date_format(format = "%b '%y")) +
    theme_behindbars(base_size = 16, base_color = "black") + 
    labs(y = "% of federal prisons with COVID outbreak")
ggsave("outbreaks_overtime.png", outbreaks_overtime, width = 7, height = 5)

# ------------------------------------------------------------------------------

## state-specific graphs

# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# california incarcerated people 
ca_df <- df %>% 
    filter(Facility.ID %in% c(
        # 83,
        171,
        141,
        # 120,
        139, # mule creek
        161 #substance abuse treatment fac
        # 135, # LA county jails
        # 1725
    )) %>% 
    filter(!is.na(Residents.Active)) %>%
    mutate(Staff.Active = replace_na(Staff.Active, 0)) %>% 
    mutate(Total.Active = Residents.Active + Staff.Active) 

## california plot
ca_plot <- ca_df %>%
    mutate(Label = str_to_title(Name), 
           outbreak_ = case_when(
               Residents.Active / Residents.Population > 0.05 ~ "Worst outbreak (5+% actively infected)",
               Total.Active >= 5 ~ "Outbreak (5+ active cases)", 
               TRUE ~ "No outbreak (<5 active cases)")) %>% 
    ggplot(aes(x = Date, y = Residents.Active)) + 
    geom_line(size = 0.6, color = "#fee6ce") + 
    geom_point(aes(color = outbreak_), size = 0.6) + 
    facet_wrap(~Label, nrow = 2, scales = "free") + 
    scale_x_date(breaks = pretty_breaks(n = 3), label = date_format(format = "%b '%y")) + 
    theme_behindbars(base_size = 12) + 
    labs(y = "Active cases among incarcerated people") + 
    theme(strip.background = element_blank(), 
          legend.title = element_blank(), 
          legend.position = "top") + 
    scale_color_manual(values = c("#fee6ce", "#fdae6b", "#e6550d"))
ggsave("ca_omicron.png", ca_plot, width = 8, height = 5)


# ------------------------------------------------------------------------------
# california staff 
ca_staff_df <- df %>% 
    filter(Facility.ID %in% c(
        2089,
        91,
        92,
        141,
        99,
        147
    )) %>% 
    filter(!is.na(Staff.Active)) 

## california plot
ca_plot <- ca_staff_df %>%
    mutate(Label = str_to_title(Name),
           Label = str_replace(Label, "Cdcr Cchcs Worksite - Sacramento County", "Sacramento County CDCR Worksite"),
           ) %>%
    ggplot(aes(x = Date, y = Staff.Active)) + 
    geom_line(size = 0.6, color = "#fee6ce") + 
    geom_point(color = "#fdae6b", size = 0.6) + 
    facet_wrap(~Label, nrow = 2, scales = "free") + 
    scale_x_date(breaks = pretty_breaks(n = 3), label = date_format(format = "%b '%y")) + 
    theme_behindbars(base_size = 12) + 
    labs(y = "Active cases among prison staff") + 
    theme(strip.background = element_blank(), 
          legend.title = element_blank(), 
          legend.position = "top")
ggsave("ca_staff_omicron.png", ca_plot, width = 8, height = 5)

# ------------------------------------------------------------------------------
# illinois

daily_il_plot <- hist_state %>% 
    filter(State == "Illinois") %>%
    ## uncomment line below to show all dates
    filter(Date > as.Date("2021-11-01")) %>%
    ggplot() + 
    geom_bar(aes(x = Date, y = Residents.Active), stat = "identity") + 
    theme_behindbars(base_size = 14, base_color = "black") + 
    scale_y_continuous(label = scales::comma) +
    theme(legend.position = "right", legend.title = element_blank()) + 
    scale_fill_manual(values = c("#9DC183", "#664d60", "#D7790F")) + 
    scale_x_date(breaks = scales::pretty_breaks(n = 12)) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    labs(title = "COVID Daily Cases in Illinois Prisons", 
         y = "Reported Active Cases")
ggsave("illinois_daily.png", daily_il_plot, width = 7, height = 5)

# ------------------------------------------------------------------------------
# BOP
bop_df <- df %>% 
    filter(Facility.ID %in% c(
        2433,
        2450,
        2330,
        2379,
        2369,
        2445
    )) %>% 
    filter(!is.na(Residents.Active)) %>%
    mutate(Staff.Active = replace_na(Staff.Active, 0)) %>% 
    mutate(Total.Active = Residents.Active + Staff.Active) 

##  bop plot
bop_plot <- bop_df %>%
    mutate(Label = str_to_title(Name), 
           Label = str_replace(Label, "Federal Correctional Institution", "FCI"),
           Label = str_replace(Label, "United States Penitentiary", "USP"),
           Label = str_replace(Label, "Federal Correctional Complex", "FCC"),
           Label = str_replace(Label, "Metropolitan Correctional Center", "MCC"),
           Label = str_c(Label, 
                         " (", State, ")"), 
           outbreak_ = case_when(
               Residents.Active / Residents.Population > 0.05 ~ "Worst outbreak (5+% actively infected)",
               Total.Active >= 5 ~ "Outbreak (5+ active cases)", 
               TRUE ~ "No outbreak (<5 active cases)")) %>% 
    ggplot(aes(x = Date, y = Residents.Active)) + 
    geom_line(size = 0.6, color = "#fee6ce") + 
    geom_point(aes(color = outbreak_), size = 0.6) + 
    facet_wrap(~Label, nrow = 3, scales = "free") + 
    scale_x_date(breaks = pretty_breaks(n = 3), label = date_format(format = "%b '%y")) + 
    theme_behindbars(base_size = 12) + 
    labs(y = "Active cases among incarcerated people") + 
    theme(strip.background = element_blank(), 
          legend.title = element_blank(), 
          legend.position = "top") + 
    scale_color_manual(values = c("#fee6ce", "#fdae6b", "#e6550d"))
ggsave("bop_omicron.png", bop_plot, width = 9, height = 5)

# ------------------------------------------------------------------------------
# michigan 

mi <- df %>%
    filter(State == "Michigan",
           Jurisdiction == "state") 

behindbarstools::plot_recent_fac_increases(scrape_df = mi, 
                                           metric = "Staff.Confirmed")


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------


## ccreate google sheet to help data stuff!

track_covid_dec_to_present <- function(
    scrape_df, 
    outbreaks_sheet_loc = "1v_hOdQX_ikckEKw9wRRF_jRegteCIesZpZDfqklyHgY",
    metric = "Residents.Confirmed", 
    num_fac = 8,
    overwrite_data = TRUE) {
    ## define inputs for data filtering
    date <- Sys.Date()
    state_df <- read_csv("https://raw.githubusercontent.com/uclalawcovid19behindbars/data/master/historical-data/historical_state_counts.csv")
    latest_scrape_date <-  max(scrape_df$Date)
    delta_start_date <- as.Date("2021-11-21")
    
    fac_data <- df %>%
        filter(!(stringr::str_detect(Name, "(?i)state") & stringr::str_detect(Name, "(?i)wide"))) %>%
        filter(Date >= delta_start_date) %>%
        filter(State == "Michigan",
               Jurisdiction == "state") %>%
        # filter(State %in% c("California", "Illinois", "Michigan") | 
        #            Jurisdiction == "federal",
        #        Jurisdiction != "county") %>%
        group_by(Name, State) %>%
        # mutate(start_val = first(!!sym(metric)),
        #        last_val = last(!!sym(metric)),
        mutate(start_val = first(Residents.Confirmed),
               last_val = last(Residents.Confirmed),
               raw_change = last_val - start_val,
               pct_increase = (raw_change / start_val)*100) %>%
        distinct(Facility.ID, Name, State, start_val, last_val, raw_change, pct_increase) %>% 
        filter(raw_change > 0) 
    if(!str_detect(metric, ".Deaths")) {
        fac_data <- fac_data %>%
            filter(start_val > 0)
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
        filter(State == "Illinois") %>%
        group_by(State) %>%
        mutate(start_val = first(!!sym(metric)),
               last_val = last(!!sym(metric)),
               raw_change = last_val - start_val,
               pct_increase = (raw_change / start_val)*100) %>%
        distinct(State, start_val, last_val, raw_change, pct_increase) %>%
        filter(raw_change > 0)
    if(!str_detect(metric, ".Deaths")) {
        state_data <- state_data %>%
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
    if(overwrite_data){
        range_write(
            data = out, 
            ss = outbreaks_sheet_loc, 
            sheet = glue("{metric}"), 
            reformat = FALSE)
    }
    return(out)
} 

metrics <- c("Residents.Confirmed", "Residents.Active", 
             "Residents.Deaths", "Staff.Confirmed", "Staff.Deaths")
out <- metrics %>%
    map(~ track_covid_dec_to_present(metric = .x, 
                                       scrape_df = df))

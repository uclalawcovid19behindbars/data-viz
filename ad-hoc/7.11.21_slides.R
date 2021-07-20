library(behindbarstools)
library(tidyverse)

# Plot refreshes for 7-11-2021 slides for Sharon 

# ------------------------------------------------------------------------------
# Weekly / monthly new cases cases and deaths plots
# ------------------------------------------------------------------------------

# Updated version that excludes juvenile facilities 
calc_aggregate_counts <- function(
    window = 31, ucla_only = FALSE, state = FALSE, collapse_vaccine = TRUE,
    all_dates = FALSE, week_grouping = TRUE){
    
    round_ <- ifelse(week_grouping, "week", "month")
    
    to_report <- c(
        datasets::state.name, "Federal", "ICE", "District of Columbia")
    
    mp_data_wide <- read_mpap_data(window = window, all_dates = all_dates)
    
    if(all_dates){
        mp_data <- mp_data_wide %>%
            filter(!is.na(Date)) %>%
            mutate(Date = lubridate::floor_date(Date, round_)) %>%
            tidyr::pivot_longer(
                -(State:Date), names_to = "Measure", values_to = "MP") %>%
            group_by(State, Date, Measure) %>%
            summarize(MP = max_na_rm(MP), .groups = "drop")
    }
    else{
        mp_data <- mp_data_wide %>%
            tidyr::pivot_longer(
                -(State:Date), names_to = "Measure", values_to = "MP")
    }
    
    if(ucla_only){
        mp_data$MP <- NA_real_
    }
    
    ucla_df <- read_scrape_data(
        window = window, all_dates = all_dates, wide_data = FALSE)
    
    # Added this here!! Not in barstools function 
    ucla_df <- ucla_df %>% 
        filter(!Age %in% c("Juvenile"))
    
    fac_long_df <- ucla_df %>%
        mutate(State = ifelse(Jurisdiction == "federal", "Federal", State)) %>%
        mutate(State = ifelse(Jurisdiction == "immigration", "ICE", State)) %>%
        filter(
            Jurisdiction %in% c("state", "federal", "immigration") |
                (State == "District of Columbia" & Jurisdiction == "county")) %>%
        select(Name, Date, State, Measure, value)
    
    if(all_dates){
        state_df <- fac_long_df %>%
            mutate(Date = lubridate::floor_date(Date, round_)) %>%
            rename(UCLA = value) %>%
            filter(!is.na(UCLA)) %>%
            group_by(State, Date, Measure, Name) %>%
            summarize(UCLA = max_na_rm(UCLA), .groups = "drop_last") %>%
            mutate(has_statewide = "STATEWIDE" %in% Name) %>%
            # if state wide and other counts exist for a measure only take max date
            filter(!(has_statewide) | Date == max(Date)) %>%
            mutate(has_statewide = "STATEWIDE" %in% Name) %>%
            # if state wide and other counts still exist for a measure only use statewide
            filter(!(has_statewide & Name != "STATEWIDE")) %>%
            group_by(State, Date, Measure) %>%
            summarise(UCLA = sum_na_rm(UCLA), .groups = "drop")
        
        if(collapse_vaccine){
            sub_vac_res <- state_df %>%
                group_by(State, Date) %>%
                mutate(No.Initiated = !("Residents.Initiated" %in% Measure)) %>%
                filter(No.Initiated) %>%
                # remove vadmin in the vector if you dont want to sub for that val
                filter(Measure %in% c("Residents.Completed")) %>%
                arrange(State, Date, Measure) %>%
                filter(1:n() == 1) %>%
                mutate(Measure = "Residents.Initiated") %>%
                ungroup()
            
            sub_vac_staff <- state_df %>%
                group_by(State, Date) %>%
                mutate(No.Initiated = !("Staff.Initiated" %in% Measure)) %>%
                filter(No.Initiated) %>%
                # add vadmin in the vector if you dont to sub for that val
                filter(Measure %in% c("Staff.Completed")) %>%
                arrange(State, Date, Measure) %>%
                filter(1:n() == 1) %>%
                mutate(Measure = "Staff.Initiated") %>%
                ungroup()
            
            state_df <- bind_rows(state_df, sub_vac_res, sub_vac_staff) %>%
                select(-No.Initiated)
        }
        
        comb_df <- state_df %>%
            full_join(mp_data, by = c("State", "Measure", "Date")) %>%
            arrange(State, Date, Measure)
    }
    else{
        state_df <- fac_long_df %>%
            rename(UCLA = value) %>%
            filter(!is.na(UCLA)) %>%
            group_by(State, Measure) %>%
            mutate(has_statewide = "STATEWIDE" %in% Name) %>%
            # if state wide and other counts exist for a measure only take more
            # recently scraped data
            filter(!(has_statewide) | Date == max(Date)) %>%
            mutate(has_statewide = "STATEWIDE" %in% Name) %>%
            # if state wide and other counts still exist for a measure only
            # use statewide measures
            filter(!(has_statewide & Name != "STATEWIDE")) %>%
            group_by(State, Measure) %>%
            summarise(
                UCLA = sum_na_rm(UCLA), Date = max(Date), .groups = "drop")
        
        if(collapse_vaccine){
            sub_vac_res <- state_df %>%
                group_by(State) %>%
                mutate(No.Initiated = !("Residents.Initiated" %in% Measure)) %>%
                filter(No.Initiated) %>%
                # add vadmin in the vector if you also want to sub for that val
                filter(Measure %in% c("Residents.Completed")) %>%
                arrange(State, Measure) %>%
                filter(1:n() == 1) %>%
                mutate(Measure = "Residents.Initiated") %>%
                ungroup()
            
            sub_vac_staff <- state_df %>%
                group_by(State) %>%
                mutate(No.Initiated = !("Staff.Initiated" %in% Measure)) %>%
                filter(No.Initiated) %>%
                # add vadmin in the vector if you also want to sub for that val
                filter(Measure %in% c("Staff.Completed")) %>%
                arrange(State, Measure) %>%
                filter(1:n() == 1) %>%
                mutate(Measure = "Staff.Initiated") %>%
                ungroup()
            
            state_df <- bind_rows(state_df, sub_vac_res, sub_vac_staff) %>%
                select(-No.Initiated)
        }
        
        comb_df <- state_df %>%
            rename(Date.UCLA = Date) %>%
            full_join(
                rename(mp_data, Date.MP = Date), by = c("State", "Measure")) %>%
            arrange(State, Measure)
    }
    
    harm_df <- comb_df %>%
        mutate(Val = case_when(
            is.na(UCLA) & is.na(MP) ~ NA_real_,
            is.na(UCLA) ~ MP,
            is.na(MP) ~ UCLA,
            UCLA >= MP ~ UCLA,
            TRUE ~ MP
        ))
    
    # Join with anchored population data
    if(state){
        aggregate_pop_df <- read_aggregate_pop_data()
        
        out_state_df <- harm_df %>%
            left_join(select(aggregate_pop_df, -Date) , by = "State") %>%
            mutate(Pop.Anchor = case_when(
                str_detect(Measure, "Residents.") ~ Residents.Population,
                str_detect(Measure, "Staff.") ~ Staff.Population)) %>%
            select(-Residents.Population, -Staff.Population)
        
        return(out_state_df)
    }
    
    agg_df <- harm_df %>%
        filter(!is.na(Val)) %>%
        group_by(Measure)
    
    if(all_dates){
        agg_df <- group_by(agg_df, Date, Measure)
    }
    
    out_agg_df <- agg_df %>%
        summarize(
            Count = sum_na_rm(Val), Reporting = sum(!is.na(Val)),
            Missing = paste0(
                to_report[!(to_report %in% State)], collapse = ", "),
            .groups = "drop")
    
    return(out_agg_df)
}

agg_month <- calc_aggregate_counts(state = T, week_grouping = FALSE, all_dates = T)
agg_week <- calc_aggregate_counts(state = T, week_grouping = TRUE, all_dates = T)

create_lag_bars <- function(df, metric, bar_color = "#D7790F"){
    agg_month %>% 
        filter(Measure == metric) %>% 
        group_by(Date) %>% 
        summarise(cases = sum_na_rm(MP)) %>%
        mutate(lag = cases - lag(cases)) %>%
        mutate(lag = ifelse(lag < 0, 0, lag)) %>%
        ggplot() + 
        geom_bar(aes(x = Date, y = lag), fill = bar_color, stat = "identity") + 
        theme_behindbars(base_size = 14, base_color = "black") + 
        scale_y_continuous(label = scales::comma) +
        scale_x_date(breaks = scales::pretty_breaks(n = 6)) + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
}

simple_cases <- create_lag_bars(agg_month, "Residents.Confirmed", "#D7790F") + 
    labs(y = "Reported Monthly Cases")

simple_deaths <- create_lag_bars(agg_month, "Residents.Deaths", "#4C6788") + 
    labs(y = "Reported Monthly Deaths") + 
    scale_y_continuous(label = scales::comma, limits = c(0, 500)) 

jur_cases <- agg_month %>% 
    filter(Measure == "Residents.Confirmed") %>% 
    mutate(Grouping = case_when(State == "ICE" ~ "ICE", 
                                State == "Federal" ~ "Federal", 
                                TRUE ~ "State")) %>% 
    group_by(State) %>% 
    fill(Val) %>% 
    group_by(Grouping, Date) %>% 
    summarise(cases = sum_na_rm(Val)) %>%
    mutate(lag = cases - lag(cases)) %>%
    mutate(lag = ifelse(lag < 0, 0, lag)) %>% 
    ggplot() + 
    geom_bar(aes(x = Date, y = lag, fill = Grouping), stat = "identity") + 
    theme_behindbars(base_size = 14, base_color = "black") + 
    scale_y_continuous(label = scales::comma) +
    theme(legend.position = "right", legend.title = element_blank()) + 
    scale_fill_manual(values = c("#9DC183", "#664d60", "#D7790F")) + 
    scale_x_date(breaks = scales::pretty_breaks(n = 12)) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    labs(title = "COVID Cases in State, Federal, and ICE Carceral Facilities", 
        y = "Reported Monthly Cases")

# ------------------------------------------------------------------------------
# Scorecard hex maps
# ------------------------------------------------------------------------------

scores <- googlesheets4::read_sheet("1fHhRAjwYGVmgoHLUENvcYffHDjEQnpp7Rwt9tLeX_Xk", 
                                    sheet = "June") %>% 
    select(state, score)

spdf <- geojsonio::geojson_read("https://raw.githubusercontent.com/uclalawcovid19behindbars/data-viz/master/data-templates/us_states_hexgrid.geojson", what = "sp")

spdf@data = spdf@data %>% 
    mutate(google_name = gsub(" \\(United States\\)", "", google_name))

spdf_fortified <- broom::tidy(spdf, region = "google_name")

centers <- cbind.data.frame(data.frame(
    rgeos::gCentroid(spdf, byid = TRUE), 
    id = spdf@data$iso3166_2))

joined_hex <- spdf_fortified %>% 
    left_join(scores, by = c("id" = "state")) 

theme_map_behindbars <- function(
    base_size = 24, base_family = "Helvetica") {
    
    behindbarstools::theme_behindbars(
        base_size = base_size,
        base_family = base_family, 
        base_color = "black"
    ) +
        ggplot2::theme(
            axis.line =           element_blank(),
            axis.ticks =          element_blank(),
            axis.text =           element_blank(),
            axis.title =          element_blank(),
            axis.title.y =        element_blank(),
            panel.grid.major =    element_blank(),
            panel.grid.minor =    element_blank(),
            panel.grid.major.y =  element_blank(),
            legend.position =     "top",
            legend.title =        element_blank())
}

out <- joined_hex %>%
    ggplot() +
    geom_polygon(
        aes(x = long, y = lat, group = group, fill = score), 
        color = "white", 
        size = 0.8) + 
    geom_text(
        data = centers, 
        aes(x = x, y = y, label = id), size = 4, color = "white") + 
    coord_map() + 
    theme_map_behindbars(base_size = 14) + 
    scale_fill_manual(
        values = c("F" = "#d7301f",
                   "D" = "#fc8d59", 
                   "C" = "#fdcc8a", 
                   "B" = "#fef0d9"), 
        guide = guide_legend(ncol = 1)) + 
    theme(legend.position = "right") 

# ------------------------------------------------------------------------------
# Case fatality rate lollipop plots 
# ------------------------------------------------------------------------------

state_agg <- read.csv(str_c(
    "https://raw.githubusercontent.com/uclalawcovid19behindbars/data/", 
    "master/latest-data/state_aggregate_counts.csv"))

state_agg %>% 
    mutate(cfr = Residents.Deaths / Residents.Confirmed * 1000) %>% 
    mutate(State = fct_reorder(State, cfr)) %>% 
    arrange(desc(cfr)) %>% 
    head(20) %>% 
    ggplot(aes(x = State, y = cfr, xend = State, yend = 0)) + 
    geom_point(size = 3, color = "#D7790F") +
    geom_segment(size = 1.5, color = "#D7790F") +
    coord_flip() +
    theme_behindbars(base_color = "black", base_size = 16) +
    theme(axis.title.y = element_blank(), 
          axis.title.x = element_text()) + 
    ylab("COVID Deaths Per 1,000 Cases")

# ------------------------------------------------------------------------------
# Update Hawaii outbreak plot 
# ------------------------------------------------------------------------------

all_hi <- read_scrape_data(all_dates = T, state = "Hawaii")

all_hi %>% 
    filter(Name == "HAWAII COMMUNITY CORRECTIONAL CENTER") %>%
    filter(Date > as.Date("2021-03-01")) %>% 
    filter(!is.na(Residents.Confirmed)) %>% 
    ggplot(aes(x = Date, y = Residents.Confirmed)) +  
    geom_line(size = 1.5, color = "#D7790F") + 
    theme_behindbars(base_size = 14) + 
    scale_x_date(breaks = scales::pretty_breaks(n = 6), label = scales::date_format(format = "%b %Y")) + 
    labs(y = "Cumulative COVID Cases\nAmong Incarcerated People", 
         title = "COVID Outbreak at the Hawaii Community Correctional Center")

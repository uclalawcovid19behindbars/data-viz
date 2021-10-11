rm(list = ls())

library(tidyverse)
library(behindbarstools)
library(scales)

GITHUB_PATH <- "https://raw.githubusercontent.com/"

STATE_DF_PATH <- str_c(GITHUB_PATH, "uclalawcovid19behindbars/data/master/latest-data/latest_state_counts.csv")
NYT_PATH <- str_c(GITHUB_PATH, "/nytimes/covid-19-data/master/prisons/systems.csv")
HEX_GEOJSON_PATH <- str_c(GITHUB_PATH, "uclalawcovid19behindbars/data-viz/master/data-templates/us_states_hexgrid.geojson")

state_df <- read.csv(STATE_DF_PATH)
nyt_df <- read.csv(NYT_PATH)

# Clean data, create rates and bins 
plot_df <- state_df %>% 
    left_join(nyt_df %>% 
                  mutate(inmate_tests = ifelse(inmate_tests == 0, NA, inmate_tests)),
              by = c("State" = "system")) %>% 
    mutate(Residents.Population = case_when(
        State == "Minnesota" ~ 7078, 
        TRUE ~ as.double(Residents.Population))) %>% 
    mutate(Residents.Tadmin = case_when(
        State == "Arizona" ~ 47899, 
        State == "Washington" ~ 88491, 
        TRUE ~ as.double(Residents.Tadmin))) %>% 
    mutate(nyt_flag_ = ifelse(
        is.na(Residents.Tadmin), 1, 0)) %>% 
    mutate(Tadmin_nyt = ifelse(
        nyt_flag_ == 1, inmate_tests, Residents.Tadmin)) %>% 
    mutate(Confirmed_nyt = ifelse(
        nyt_flag_ == 1, total_inmate_cases, Residents.Confirmed)) %>% 
    mutate(Case.Rate = Residents.Confirmed / Residents.Population, 
           Testing.Rate = Tadmin_nyt / Residents.Population, 
           CF.Rate = Residents.Deaths / Residents.Confirmed, 
           TP.Rate = Confirmed_nyt / Tadmin_nyt) %>% 
    mutate(Testing.Bin = case_when(
        is.na(Testing.Rate) ~ "No data", 
        Testing.Rate <= 1 ~ "Fewer than 1", 
        Testing.Rate > 1 & Testing.Rate <= 2 ~ "1 to 2", 
        Testing.Rate > 2 & Testing.Rate <= 3 ~ "2 to 3", 
        Testing.Rate > 3 ~ "More than 3")) %>% 
    mutate(CF.Bin = case_when(
        is.na(CF.Rate) ~ "No data", 
        CF.Rate < 0.01 ~ "Below 1%", 
        CF.Rate > 0.01 & CF.Rate < 0.02 ~ "1 to 2%", 
        CF.Rate > 0.02 & CF.Rate < 0.03 ~ "2 to 3%", 
        CF.Rate > 0.03 ~ "Above 3%")) %>% 
    mutate(TP.Bin = case_when(
        is.na(TP.Rate) ~ "No data", 
        TP.Rate < 0.05 ~ "Below 5%", 
        TP.Rate > 0.05 & TP.Rate < 0.1 ~ "5 to 10%", 
        TP.Rate > 0.10 & TP.Rate < 0.2 ~ "10 to 20%", 
        TP.Rate > 0.20 & TP.Rate < 0.3 ~ "20 to 30%",
        TP.Rate > 0.3 ~ "Above 30%"
    ))

# Hex map skeleton 
spdf <- geojsonio::geojson_read(HEX_GEOJSON_PATH, what = "sp")

spdf@data = spdf@data %>% 
    mutate(google_name = gsub(" \\(United States\\)", "", google_name))

spdf_fortified <- broom::tidy(spdf, region = "google_name")

centers <- cbind.data.frame(data.frame(
    rgeos::gCentroid(spdf, byid = TRUE), 
    id = spdf@data$iso3166_2))

joined_df <- spdf_fortified %>% 
    left_join(plot_df, by = c("id" = "State")) 

fill_testing <- c(
    "Fewer than 1" = "#d94701", 
    "1 to 2" = "#fd8d3c", 
    "2 to 3" = "#fdbe85", 
    "More than 3" = "#feedde", 
    "No data" = "#d9d9d9")

fill_CF <- c(
    "Below 1%" = "#feedde", 
    "1 to 2%" = "#fdbe85", 
    "2 to 3%" = "#fd8d3c", 
    "Above 3%" = "#d94701", 
    "No data" = "#d9d9d9")

fill_TP <- c(
    "Below 5%" = "#feedde", 
    "5 to 10%" = "#fdbe85", 
    "10 to 20%" = "#fd8d3c", 
    "20 to 30%" = "#e6550d", 
    "Above 30%" = "#a63603", 
    "No data" = "#d9d9d9")

breaks_testing <- c("Fewer than 1", "1 to 2", "2 to 3", "More than 3", "No data")
breaks_CF <- c("Below 1%", "1 to 2%", "2 to 3%", "Above 3%", "No data")
breaks_TP <- c("Below 5%", "5 to 10%", "10 to 20%", "20 to 30%", "Above 30%", "No data")

hex_theme <- theme(
    plot.title =          element_text(hjust = 0.5), 
    plot.subtitle =       element_text(hjust = 0.5), 
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

barplot_theme <- theme(
    axis.ticks.y = element_blank(),
    panel.grid.major.y =  element_blank(), 
    plot.title =          element_text(hjust = 0.5), 
    plot.subtitle =       element_text(hjust = 0.5), 
    axis.title.y =        element_blank(),
    axis.title.x =        element_blank()
)

# Testing rate hex map 
plot_hex_map <- function(df, fill_var, values, breaks, labels){
    ggplot() +
        geom_polygon(
            data = df, 
            aes(x = long, y = lat, group = group, fill = !!sym(fill_var)), 
            color = "white") + 
        geom_text(
            data = centers, 
            aes(x = x, y = y, label = id), size = 4) + 
        scale_fill_manual(
            values = values,
            breaks = breaks, 
            labels = labels) + 
        coord_map() + 
        guides(fill = guide_legend(nrow = 1, byrow = TRUE)) + 
        theme_behindbars(base_size = 14, base_color = "black") +
        hex_theme
}

plot_hex_map(joined_df, "Testing.Bin", fill_testing, breaks_testing, breaks_testing) + 
    labs(title = "Several states have rarely tested incarcerated people for COVID", 
        subtitle = "Number of COVID tests reported per person incarcerated in state prisons")

plot_hex_map(joined_df, "CF.Bin", fill_CF, breaks_CF, breaks_CF) + 
    labs(title = "Alabama state prisons have reported a case fatality rate above 3%", 
         subtitle = "Case fatality rates among incarcerated people in state prisons")

plot_hex_map(joined_df, "TP.Bin", fill_TP, breaks_TP, breaks_TP) + 
    labs(title = "Several prison systems have reported alarmingly high positivity rates", 
         subtitle = "Test positivity rates among incarcerated people in state prisons")

# CFR lollipop plot 
plot_lollipop <- function(df, plot_var, filter_min) {
    ggplot(
        data = df %>% 
            filter(!!sym(plot_var) > filter_min) %>% 
            arrange(!!sym(plot_var)) %>% 
            mutate(State = factor(State, levels = State)) %>% 
            filter(!State %in% c("ICE", "Federal", "District of Columbia")) %>% 
            filter(!is.na(!!sym(plot_var))),  
        aes(x = State, y = !!sym(plot_var))) + 
    geom_segment(aes(xend = State, yend = 0), size = 0.8, color = "#b6b6a1") +
    geom_point(color = "#565629", size = 2.9) +
    geom_text(aes(
        label = percent(!!sym(plot_var), accuracy = 0.1)), 
        hjust = -0.4, color = "#565629", size = 4) + 
    coord_flip()  + 
    theme_classic(base_family = "Helvetica", base_size = 14)  + 
    barplot_theme 
}

ggplot(
    data = plot_df %>% 
        filter(Testing.Rate < 2.0) %>% 
        arrange(-Testing.Rate) %>% 
        mutate(State = factor(State, levels = State)) %>% 
        filter(!State %in% c("ICE", "Federal", "District of Columbia")) %>% 
        filter(!is.na(Testing.Rate)),  
    aes(x = State, y = Testing.Rate)) + 
    geom_segment(aes(xend = State, yend = 0), size = 0.8, color = "#b6b6a1") +
    geom_point(color = "#565629", size = 2.9) +
    geom_text(aes(
        label = comma(Testing.Rate, accuracy = 0.1)), 
        hjust = -0.4, color = "#565629", size = 4) + 
    coord_flip()  + 
    theme_classic(base_family = "Helvetica", base_size = 14) + 
    barplot_theme + 
    scale_y_continuous(
        limits = c(0, 2.1),
        expand = c(0, 0)) +
    labs(title = "11 state prison systems have tested incarcerated people fewer than 2 times on average", 
         subtitle = "Number of COVID tests reported per person incarcerated in state prisons")

plot_lollipop(plot_df, "CF.Rate", 0.01) + 
    scale_y_continuous(
        labels = percent_format(accuracy = 1.0), 
        limits = c(0, 0.041), 
        expand = c(0, 0)) + 
    labs(title = "12 state prison systems have case fatality rates above 1%", 
         subtitle = "Case fatality rates among incarcerated people in state prisons")

plot_lollipop(plot_df, "TP.Rate", 0.15) + 
    scale_y_continuous(
        labels = percent_format(accuracy = 1.0), 
        limits = c(0, 0.51),
        expand = c(0, 0)) + 
    labs(title = "10 state prison systems have reported test positivity rates above 15%", 
         subtitle = "Test positivity rates among incarcerated people in state prisons")

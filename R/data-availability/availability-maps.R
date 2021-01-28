library(tidyverse)
library(behindbarstools)

# Load data 
scrape_df <- behindbarstools::read_scrape_data()
machine_readable <- read.csv("data/interim/machine-readability-by-jurisdiction.csv")

# ------------------------------------------------------------------------------
# Facilities by jurisdiction 
# ------------------------------------------------------------------------------

# Plot facilities by jurisdiction (lollipop)
jur_lolli <- scrape_df %>% 
    mutate(Jurisdiction = stringr::str_to_title(Jurisdiction)) %>% 
    group_by(Jurisdiction) %>% 
    count() %>% 
    ggplot(aes(x = Jurisdiction, y = n)) + 
    geom_segment(
        aes(x = Jurisdiction, xend = Jurisdiction, y = 0, yend = n), 
        size = 1.5, 
        color = "#4C6788") + 
    geom_point(
        color = "#4C6788", size = 4.0) + 
    geom_text(
        aes(label = scales::comma(n)), 
        color = "#555526", 
        size = 3.5, 
        hjust = -0.5, vjust = -0.8) + 
    coord_flip() + 
    scale_y_continuous(
        label = scales::comma, 
        limits = c(0, 1500)) + 
    behindbarstools::theme_behindbars(base_size = 14) + 
    theme(axis.title.y = element_blank()) 

ggsave("img/facilities-by-jurisdiction.svg", jur_lolli, width = 8, height = 3)

# Plot facilities by jurisdiction (donut)
plot_df <- scrape_df %>% 
    mutate(Jurisdiction = stringr::str_to_title(Jurisdiction)) %>% 
    group_by(Jurisdiction) %>% 
    count() %>% 
    ungroup() %>% 
    mutate(fraction = n / sum(n), 
           ymax = cumsum(fraction), 
           ymin = c(0, head(ymax, n = -1)),
           label_position = (ymax + ymin) / 2, 
           Jurisdiction = factor(Jurisdiction, 
                                 levels = c("State", "Federal", "County", "Immigration")))

jur_donut <- ggplot(plot_df, 
       aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = Jurisdiction, label = n)) + 
    geom_rect(color = "white") + 
    geom_text(x = 3.5, aes(y = label_position, label = n), size = 4) + 
    coord_polar(theta = "y") +
    xlim(c(2, 4)) + 
    behindbarstools::theme_map_behindbars(base_size = 14) + 
    scale_fill_manual(
        values = c("#D7790F", "#82CAA4", "#4C6788", "#AE91A8")) + 
    theme(legend.position = "right") 

ggsave("img/facilities-by-jurisdiction_donut.svg", jur_donut, width = 8, height = 3)

# ------------------------------------------------------------------------------
# Data availability maps 
# ------------------------------------------------------------------------------

# Hex map skeleton 
spdf <- geojsonio::geojson_read("data/raw/us_states_hexgrid.geojson", what = "sp")

spdf@data = spdf@data %>% 
    mutate(google_name = gsub(" \\(United States\\)", "", google_name))

spdf_fortified <- broom::tidy(spdf, region = "google_name")

centers <- cbind.data.frame(data.frame(
    rgeos::gCentroid(spdf, byid = TRUE), 
    id = spdf@data$iso3166_2))

# Get plotting data (with hex grid) 
get_plotting_data <- function(metric) {

    # States with facility-level data 
    facility_df <- scrape_df %>% 
        filter(Date > Sys.Date() - 14) %>% 
        filter(Jurisdiction == "state") %>% 
        filter(Name != "STATEWIDE") %>% 
        filter(!is.na(!!sym(metric))) %>% 
        select(State) %>% 
        distinct() %>% 
        mutate(level = "facility")
    
    # States with statewide data 
    statewide_df <- scrape_df %>% 
        filter(Date > Sys.Date() - 14) %>% 
        filter(Jurisdiction == "state") %>% 
        filter(Name == "STATEWIDE") %>% 
        filter(!is.na(!!sym(metric))) %>% 
        select(State) %>% 
        distinct() %>% 
        mutate(level = "statewide")
    
    # Combine and merge with hex grid 
    combined_df <- facility_df %>% 
        full_join(statewide_df, by = c("State")) %>% 
        mutate(level = coalesce(level.x, level.y)) %>% 
        select(State, level) %>% 
        mutate(State = replace(State, State == "DC", "District of Columbia")) %>% 
        left_join(machine_readable %>% 
                      filter(Jurisdiction == "state"), by = c("State"))
    
    spdf_fortified %>% 
        left_join(combined_df, by = c("id" = "State")) %>% 
        mutate(level = ifelse(Machine.Readable == "No", "unavailable", level), 
               level = replace_na(level, "unavailable"))
}

# Plot map  
plot_hex_map <- function(df) {
    ggplot() +
        geom_polygon(
            data = df, 
            aes(x = long, y = lat, group = group, fill = level), 
            color = "white") + 
        geom_text(
            data = centers, 
            aes(x = x, y = y, label = id), size = 3) + 
        scale_fill_manual(
            values = fill_bb,
            breaks = c("facility", "statewide", "unavailable"), 
            labels = c("Facility-Level Data", "Statewide Data", "Unavailable")) + 
        coord_map() + 
        theme_map_behindbars(base_size = 14) 
}

# Plot aesthetics 
fill_bb <- c("facility" = "#D7790F", 
             "statewide" = "#82CAA4", 
             "unavailable" = "#E7F1E2")

# Residents coverage maps 
map_Residents.Confirmed <- 
    plot_hex_map(get_plotting_data("Residents.Confirmed")) + 
    labs(title = "Cumulative COVID-19 Cases")

map_Residents.Deaths <- 
    plot_hex_map(get_plotting_data("Residents.Deaths")) + 
    labs(title = "Cumulative COVID-19 Deaths")

map_Residents.Active <- 
    plot_hex_map(get_plotting_data("Residents.Active")) + 
    labs(title = "Active COVID-19 Cases")

map_Residents.Tadmin <- 
    plot_hex_map(get_plotting_data("Residents.Tadmin")) + 
    labs(title = "COVID-19 Tests Administered")

res_map <- ggpubr::ggarrange(map_Residents.Confirmed, map_Residents.Deaths, 
                  map_Residents.Active, map_Residents.Tadmin, 
                  common.legend = TRUE) 

ggsave("img/residents-maps.svg", res_map, width = 8, height = 8)

# Staff coverage maps 
map_Staff.Confirmed <- 
    plot_hex_map(get_plotting_data("Staff.Confirmed")) + 
    labs(title = "Cumulative COVID-19 Cases")

map_Staff.Deaths <- 
    plot_hex_map(get_plotting_data("Staff.Deaths")) + 
    labs(title = "Cumulative COVID-19 Deaths")

staff_map <- ggpubr::ggarrange(map_Staff.Confirmed, map_Staff.Deaths, 
                  common.legend = TRUE)

ggsave("staff-maps.svg", staff_map, width = 8, height = 4)

# ------------------------------------------------------------------------------
# Vaccine data maps 
# ------------------------------------------------------------------------------

fill_vax <- c("Reporting" = "#71A9C9", 
              "Unavailable" = "#E7F1E2")

vax_availability <- scrape_df %>% 
    mutate(level = ifelse(
        is.na(Residents.Initiated) & is.na(Residents.Vadmin) & is.na(Staff.Initiated) & is.na(Staff.Vadmin), 
        0, 1)) %>% 
    group_by(State) %>% 
    summarise(level = sum(level)) %>% 
    mutate(level = case_when(level == 0 ~ "Unavailable", 
                             level > 0 ~ "Reporting"))

plot_df <- spdf_fortified %>% 
    left_join(vax_availability, by = c("id" = "State")) 

vax_map <- ggplot() +
    geom_polygon(
        data = plot_df, 
        aes(x = long, y = lat, group = group, fill = level), 
        color = "white") + 
    geom_text(
        data = centers, 
        aes(x = x, y = y, label = id), size = 3) + 
    scale_fill_manual(
        values = fill_vax) + 
    coord_map() + 
    theme_map_behindbars(base_size = 14) + 
    labs(title = "Which state DOCs are currently reporting vaccine data?") + 
    theme(plot.title = element_text(hjust = 0.5))

ggsave("img/vax-map.svg", vax_map, width = 8, height = 4)

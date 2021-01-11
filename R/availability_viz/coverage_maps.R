library(tidyverse)
library(behindbarstools)

scrape_df <- behindbarstools::read_scrape_data()

# Plot facilities by jurisdiction 
plt1 <- scrape_df %>% 
    mutate(jurisdiction = stringr::str_to_title(jurisdiction)) %>% 
    group_by(jurisdiction) %>% 
    count() %>% 
    ggplot(aes(x = jurisdiction, y = n)) + 
    geom_segment(
        aes(x = jurisdiction, xend = jurisdiction, y = 0, yend = n), 
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

ggsave("facilities-by-jurisdiction.svg", plt1, width = 8, height = 3)

# Source: https://www.r-graph-gallery.com/328-hexbin-map-of-the-usa.html
spdf <- geojsonio::geojson_read("us_states_hexgrid.geojson", what = "sp")

spdf@data = spdf@data %>% 
    mutate(google_name = gsub(" \\(United States\\)", "", google_name))

spdf_fortified <- broom::tidy(spdf, region = "google_name")

centers <- cbind.data.frame(data.frame(
    rgeos::gCentroid(spdf, byid = TRUE), 
    id = spdf@data$iso3166_2))

# Set plotting aesthetics 
fill_bb <- c("facility" = "#D7790F", 
             "statewide" = "#82CAA4", 
             "unavailable" = "#C7C6C5")

theme_map_behindbars <- function(
    base_size = 24, base_family = "Helvetica") {
    
    behindbarstools::theme_behindbars(
        base_size = base_size,
        base_family = base_family
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

# Get plotting data (with hex grid) 
get_plotting_data <- function(metric) {
    
    # States with facility-level data 
    facility_df <- scrape_df %>% 
        filter(jurisdiction == "state") %>% 
        filter(Name != "STATEWIDE") %>% 
        filter(!is.na(!!sym(metric))) %>% 
        select(State) %>% 
        distinct() %>% 
        mutate(level = "facility")
    
    # States with statewide data 
    statewide_df <- scrape_df %>% 
        filter(jurisdiction == "state") %>% 
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
        mutate(State = replace(State, State == "DC", "District of Columbia"))
    
    spdf_fortified %>% 
        left_join(combined_df, by = c("id" = "State")) %>% 
        mutate(level = replace_na(level, "unavailable"))
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

plt2 <- ggpubr::ggarrange(map_Residents.Confirmed, map_Residents.Deaths, 
                  map_Residents.Active, map_Residents.Tadmin, 
                  common.legend = TRUE)

ggsave("residents-maps.svg", plt2, width = 8, height = 8)

# Staff coverage maps 
map_Staff.Confirmed <- 
    plot_hex_map(get_plotting_data("Staff.Confirmed")) + 
    labs(title = "Cumulative COVID-19 Cases")

map_Staff.Deaths <- 
    plot_hex_map(get_plotting_data("Staff.Deaths")) + 
    labs(title = "Cumulative COVID-19 Deaths")

plt3 <- ggpubr::ggarrange(map_Staff.Confirmed, map_Staff.Deaths, 
                  common.legend = TRUE)

ggsave("staff-maps.svg", plt3, width = 8, height = 4)


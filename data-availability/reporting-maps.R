library(tidyverse)
library(behindbarstools)

# Load data 
scorecard <- read.csv("data/interim/scorecard-data.csv", skip = 1)

# ------------------------------------------------------------------------------
# Data reporting maps 
# ------------------------------------------------------------------------------

# Hex map skeleton 
spdf <- geojsonio::geojson_read("data/raw/us_states_hexgrid.geojson", what = "sp")

spdf@data = spdf@data %>% 
    mutate(google_name = gsub(" \\(United States\\)", "", google_name))

spdf_fortified <- broom::tidy(spdf, region = "google_name")

centers <- cbind.data.frame(data.frame(
    rgeos::gCentroid(spdf, byid = TRUE), 
    id = spdf@data$iso3166_2))

# Join with DOC scorecard data 
joined <- spdf_fortified %>% 
    left_join(scorecard, by = c("id" = "State")) %>% 
    mutate_if(is.character, list(~na_if(.,""))) %>% 
    mutate_at(vars(starts_with(c("Residents.", "Staff"))), ~replace(., is.na(.), "Unavailable")) 

# Plot map  
plot_hex_map <- function(df, metric) {
    ggplot() +
        geom_polygon(
            data = df, 
            aes(x = long, y = lat, group = group, fill = !!sym(metric)), 
            color = "white") + 
        geom_text(
            data = centers, 
            aes(x = x, y = y, label = id), size = 4) + 
        scale_fill_manual(
            values = fill_bb,
            breaks = c("Facility-Level", "Statewide", "Unavailable"), 
            labels = c("Facility-Level", "Aggregate Totals", "Unavailable")) + 
        coord_map() + 
        theme_map_behindbars(base_size = 14) +
        labs(title = metric)
}

# Plot aesthetics 
fill_bb <- c("Facility-Level" = "#DDE2C6", 
             "Statewide" = "#E8B828", 
             "Unavailable" = "#BC544B")

map_res_cases <- plot_hex_map(joined, "Residents.Cumulative.Cases")
map_res_deaths <- plot_hex_map(joined, "Residents.Cumulative.Deaths") 
maps_res_active <- plot_hex_map(joined, "Residents.Active.Cases") 
maps_res_tests <- plot_hex_map(joined, "Residents.Tests") 
maps_staff_cases <- plot_hex_map(joined, "Staff.Cumulative.Cases") 
maps_staff_deaths <- plot_hex_map(joined, "Staff.Cumulative.Deaths")

reporting_map <- ggpubr::ggarrange(
    map_res_cases, map_res_deaths, maps_res_active, maps_res_tests, 
    maps_staff_cases, maps_staff_deaths, common.legend = TRUE, 
    nrow = 3, ncol = 2) 

ggsave("data/out/data-reporting-maps.svg", reporting_map, width = 12, height = 12)

# ------------------------------------------------------------------------------
# Vaccine data maps 
# ------------------------------------------------------------------------------

scrape_df <- read_scrape_data()

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

ggsave("data/out/vax-map.svg", vax_map, width = 8, height = 4)

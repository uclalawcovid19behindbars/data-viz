library(tidyverse)
library(behindbarstools)

# Hex map skeleton 
spdf <- geojsonio::geojson_read("~/UCLA/misc-data/us_states_hexgrid.geojson", what = "sp")

spdf@data = spdf@data %>% 
    mutate(google_name = gsub(" \\(United States\\)", "", google_name))

spdf_fortified <- broom::tidy(spdf, region = "google_name")

centers <- cbind.data.frame(data.frame(
    rgeos::gCentroid(spdf, byid = TRUE), 
    id = spdf@data$iso3166_2))

scrape_df <- read_scrape_data()

fill_booster <- c("Facility-Level" = "#DDE2C6", 
              "Statewide" = "#E8B828", 
              "Unavailable" = "#BC544B")

facility_level <- c("Maryland", "Michigan", "Minnesota", "Washington")
statewide <- c("Delaware", "Missouri", "New Jersey")

booster_availability <- scrape_df %>% 
    select(State) %>%
    unique() %>%
    mutate(level = case_when(State %in% facility_level ~ "Facility-Level", 
                             State %in% statewide ~ "Statewide",
                             TRUE ~ "Unavailable"))

plot_df <- spdf_fortified %>% 
    left_join(booster_availability, by = c("id" = "State")) 

booster_map <- ggplot() +
    geom_polygon(
        data = plot_df, 
        aes(x = long, y = lat, group = group, fill = level), 
        color = "white") + 
    geom_text(
        data = centers, 
        aes(x = x, y = y, label = id), size = 3) + 
    scale_fill_manual(
        values = fill_booster,
        breaks = c("Facility-Level", "Statewide", "Unavailable"), 
        labels = c("Facility-Level", "Aggregate Totals", "Unavailable")) + 
    coord_map() + 
    theme_map_behindbars(base_size = 14) + 
    labs(title = "Which state DOCs are currently reporting data on boosters?") + 
    theme(plot.title = element_text(hjust = 0.5))

ggsave("booster-map.png", booster_map, width = 8, height = 4)
ggsave("booster-map.svg", booster_map, width = 8, height = 4)

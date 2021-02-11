library(tidyverse)
library(behindbarstools)
library(googlesheets4)

# Load data 
vaccine_data <- googlesheets4::read_sheet("1tc3p3FHG0tCHnlfb8YAABno_oh9JSNL4MNfhn_clyX0") %>% 
    janitor::clean_names()

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
    left_join(vaccine_data, by = c("id" = "state")) %>% 
    mutate_if(is.character, list(~na_if(.,""))) %>% 
    mutate_at(vars(starts_with(c("Vaccinating", "Reporting"))), ~replace(., is.na(.), "No")) %>% 
    mutate(map1 = case_when(vaccinating_residents == "Yes" & reporting_residents_data == "Yes" ~ "1", 
                            vaccinating_residents == "Yes" & reporting_residents_data == "No" ~ "2", 
                            vaccinating_residents == "No" ~ "3"), 
           map2 = case_when(vaccinating_residents == "Yes" & vaccinating_staff == "Yes" ~ "1", 
                            vaccinating_residents == "No" & vaccinating_staff == "Yes" ~ "2", 
                            vaccinating_residents == "Yes" & vaccinating_staff == "No" ~ "3", 
                            TRUE ~ "4"))
          
# Plot map  
plot_hex_map <- function(df, metric) {
    ggplot() +
        geom_polygon(
            data = df, 
            aes(x = long, y = lat, group = group, fill = !!sym(metric)), 
            color = "white") + 
        geom_text(
            data = centers, 
            aes(x = x, y = y, label = id), size = 3) + 
        coord_map() + 
        theme_map_behindbars(base_size = 14) 
}

# Plot aesthetics 
fill_bb <- c("1" = "#DDE2C6", 
             "2" = "#77AAC7", 
             "3" = "#E7963C", 
             "4" = "#e3e3e3")

map1 <- plot_hex_map(joined, "map1") + 
    scale_fill_manual(
        values = fill_bb, 
        breaks = c("1", "2", "3"), 
        labels = c("Vaccinating incarcerated people and reporting data", 
                   "Reportedly vaccinating incarcerated people but not reporting data", 
                   "Not yet vaccinating incarcerated people"), 
        guide = guide_legend(ncol = 1))

map2 <- plot_hex_map(joined, "map2") + 
    scale_fill_manual(
        values = fill_bb, 
        breaks = c("1", "2", "3", "4"), 
        labels = c("Vaccinating staff and incarcerated people", 
                   "Vaccinating staff only", 
                   "Vaccinating residents only", 
                   "Not yet vaccinating staff or residents"), 
        guide = guide_legend(ncol = 1))

reporting_map <- ggpubr::ggarrange(map1, map2, common.legend = TRUE) 

ggsave("data/out/map1.png", map1, width = 6, height = 4)
ggsave("data/out/map2.png", map2, width = 6, height = 4)

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

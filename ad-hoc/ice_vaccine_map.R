rm(list = ls())
library(tidyverse)
# Only need to run this once! 
# devtools::install_github("uclalawcovid19behindbars/behindbarstools")
library(behindbarstools)

# Load vaccine data from Google Sheets 
GOOGLE_SHEETS_PATH <- "1cpBsMEM7RaCC9s-3Pg0aQ-7O0ghwMf7kSLUPc6BtMh4"

vaccine_df <- googlesheets4::read_sheet(GOOGLE_SHEETS_PATH)

# Load hex map skeleton 
HEX_GEOJSON_PATH <- str_c(
    "https://raw.githubusercontent.com/uclalawcovid19behindbars/", 
    "data-viz/master/data-templates/us_states_hexgrid.geojson")

spdf <- geojsonio::geojson_read(HEX_GEOJSON_PATH, what = "sp")

spdf@data = spdf@data %>% 
    mutate(google_name = gsub(" \\(United States\\)", "", google_name))

spdf_fortified <- broom::tidy(spdf, region = "google_name")

centers <- cbind.data.frame(data.frame(
    rgeos::gCentroid(spdf, byid = TRUE), 
    id = spdf@data$iso3166_2))

joined_df <- spdf_fortified %>% 
    left_join(vaccine_df, by = c("id" = "State")) %>% 
    mutate(Rank = as.factor(Rank))

# Create map 
colors <- c(
    "1" = "#e6550d", 
    "2" = "#fdae6b", 
    "3" = "#fee6ce"
)

breaks <- c("1", "2", "3")
labels <- c(
    "No mention of ICE detainees or no information available", 
    "Some mention", 
    "Specific plan mentioned for ICE detainees"
)

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
    legend.title =        element_blank()
)

vaccine_plot <- ggplot() +
    geom_polygon(
        data = joined_df, 
        aes(x = long, y = lat, group = group, fill = Rank), 
        color = "white") + 
    geom_text(
        data = centers, 
        aes(x = x, y = y, label = id), size = 4) + 
    scale_fill_manual(
        values = colors,
        breaks = breaks, 
        labels = labels) + 
    coord_map() + 
    guides(fill = guide_legend(ncol = 1)) + 
    theme_behindbars(base_size = 14, base_color = "black") +
    hex_theme + 
    labs(title = "Most state vaccination plans have neglected ICE detainees")

ggsave("ice-vaccine.png", width = 7, height = 5)

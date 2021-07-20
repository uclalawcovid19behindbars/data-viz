library(behindbarstools)
library(tidyverse)
library(ggbeeswarm)
library(ggalt)

# ---------------------------------------------------------------------------- #
# Read and process data 
# ---------------------------------------------------------------------------- #

# Read from Google Sheet with media reports, etc. 
data <- googlesheets4::read_sheet("1tc3p3FHG0tCHnlfb8YAABno_oh9JSNL4MNfhn_clyX0", 
                                  sheet = "Full Data") %>% 
    select(!ends_with("Source")) %>% 
    select(-Notes)

# Pull latest CDC data 
raw_cdc <- str_c(
    "https://covid.cdc.gov/covid-data-tracker/COVIDData/", 
    "getAjaxData?id=vaccination_data") %>% 
    jsonlite::read_json(simplifyVector = TRUE) 

cdc_joined <- as_tibble(raw_cdc$vaccination_data) %>% 
    mutate(pct_overall = Administered_Dose1_Recip_18PlusPop_Pct / 100)  %>% 
    select(LongName, pct_overall) %>% 
    full_join(data, by = c("LongName" = "State")) %>% 
    mutate(staff_diff = Staff.Pct - pct_overall)

# Estimate overall percentage 
cdc_joined %>% 
    mutate(est_num = Res.Pct * Res.Pop) %>% 
    mutate(total_vac = coalesce(Res.Vaccine, est_num)) %>% 
    filter(!is.na(total_vac)) %>% 
    filter(!is.na(Res.Pop)) %>% 
    summarise(num = sum_na_rm(total_vac), 
              denom = sum_na_rm(Res.Pop)) %>% 
    mutate(num / denom)
             
# ---------------------------------------------------------------------------- #
# Make plots 
# ---------------------------------------------------------------------------- #

# Facility beeswarm plot 
scrape <- read_scrape_data()

bee <- scrape %>%
    filter(State %in% c("California", "Georgia", "Michigan", "Minnesota", "Pennsylvania", "West Virginia", "Wisconsin")) %>% 
    filter(Jurisdiction == "state") %>% 
    filter(!Age %in% c("Juvenile")) %>% 
    # Hard-code order based on max rate by state 
    mutate(state_order = factor(State, levels = c(
        "Michigan", "West Virginia", "California", "Georgia", "Pennsylvania", "Minnesota", "Wisconsin"))) %>% 
    mutate(pct = Residents.Initiated / Residents.Population) %>% 
    filter(pct > 0) %>%
    filter(pct < 1) %>%
    ggplot(aes(x = state_order, y = pct)) + 
    geom_beeswarm(size = 0.8, color = "#D7790F") + 
    theme_behindbars(base_size = 14, base_color = "black") + 
    scale_y_continuous(label = scales::percent, limits = c(0, 1)) + 
    theme(axis.title.y = element_blank()) + 
    labs(title = "Vaccination rates vary tremendously across states and prisons", 
         subtitle = "Percentage of incarcerated population that received at least one dose of a vaccine")

ggsave("bee.svg", bee, width = 9, height = 5)

# Staff dumbell plot 
staff_bars <- cdc_joined %>% 
    filter(!is.na(Staff.Pct) & !is.na(pct_overall)) %>%
    mutate(Name = str_to_title(LongName)) %>% 
    mutate(staff_label = Staff.Pct - 0.05) %>% 
    mutate(overall_label = pct_overall + 0.05) %>% 
    rowwise() %>% 
    mutate(sort_ = min(pct_overall, Staff.Pct)) %>% 
    ggplot() +
    geom_dumbbell(aes(x = Staff.Pct, 
                      xend = pct_overall, 
                      y = reorder(Name, -sort_)), 
                  size_x = 2, 
                  size_xend = 2,
                  colour = "#c1c4b9",
                  size = 1.0,
                  colour_x = "#D7790F", 
                  alpha = 0.8,
                  colour_xend = "#4C6788") + 
    geom_text(aes(x = staff_label, 
                  y = reorder(Name, -sort_), 
                  label = scales::percent(Staff.Pct, accuracy = 1)), 
              color = "#D7790F") + 
    geom_text(aes(x = overall_label, 
                  y = reorder(Name, -Staff.Pct), 
                  label = scales::percent(pct_overall, accuracy = 1)), 
              color = "#4C6788") + 
    scale_x_continuous(label = scales::percent, limits = c(0, 1)) +
    theme_minimal(base_size = 13) + 
    theme(axis.title.x = element_blank(), 
          axis.title.y = element_blank())

ggsave("staff_bars.svg", staff_bars, width = 6, height = 7.5)

# Hex maps
spdf <- geojsonio::geojson_read("https://raw.githubusercontent.com/uclalawcovid19behindbars/data-viz/master/data-templates/us_states_hexgrid.geojson", what = "sp")

spdf@data = spdf@data %>% 
    mutate(google_name = gsub(" \\(United States\\)", "", google_name))

spdf_fortified <- broom::tidy(spdf, region = "google_name")

centers <- cbind.data.frame(data.frame(
    rgeos::gCentroid(spdf, byid = TRUE), 
    id = spdf@data$iso3166_2))

joined_hex <- spdf_fortified %>% 
    left_join(data, by = c("id" = "State")) %>% 
    mutate(res_value = case_when(
        Res.Pct < 0.5 ~ "Less than 50%",
        Res.Pct >= 0.5 & Res.Pct < 0.6 ~ "50% - 60%", 
        Res.Pct >= 0.6 & Res.Pct < 0.7 ~ "60% - 70%", 
        Res.Pct >= 0.7 ~ "More than 70%", 
        TRUE ~ "No data")) %>% 
    mutate(staff_value = case_when(
        Staff.Pct < 0.5 ~ "Less than 50%",
        Staff.Pct >= 0.5 & Staff.Pct < 0.6 ~ "50% - 60%", 
        Staff.Pct >= 0.6 & Staff.Pct < 0.7 ~ "60% - 70%", 
        Staff.Pct >= 0.7 ~ "More than 70%", 
        TRUE ~ "No data"))
    
plot_hex_map <- function(df, metric) {
    ggplot() +
        geom_polygon(
            data = df, 
            aes(x = long, y = lat, group = group, fill = !!sym(metric)), 
            color = "white", 
            size = 0.8) + 
        geom_text(
            data = centers, 
            aes(x = x, y = y, label = id), size = 3.5) + 
        coord_map() + 
        theme_map_behindbars(base_size = 14) 
}

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

fill_map1 <- c("Less than 50%" = "#d7301f",
               "50% - 60%" = "#fc8d59", 
               "60% - 70%" = "#fdcc8a", 
               "More than 70%" = "#fef0d9", 
               "Unavailable" = "#262922")

# Residents 
res_hex <- plot_hex_map(joined_hex, "res_value") + 
    scale_fill_manual(
        values = fill_map1, 
        breaks = c("Less than 50%", "50% - 60%", "60% - 70%", "More than 70%", "Unavailable"), 
        guide = guide_legend(ncol = 1)) + 
    theme(legend.position = "right") 

# Staff 
staff_hex <- plot_hex_map(joined_hex, "staff_value") + 
    scale_fill_manual(
        values = fill_map1, 
        breaks = c("Less than 50%", "50% - 60%", "60% - 70%", "More than 70%", "Unavailable"), 
        guide = guide_legend(ncol = 1)) + 
    theme(legend.position = "right") 

ggsave("staff_hex.svg", staff_hex, width = 8, height = 8)
ggsave("res_hex.svg", res_hex, width = 8, height = 8)

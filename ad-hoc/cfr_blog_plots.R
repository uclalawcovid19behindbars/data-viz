rm(list = ls())

library(tidyverse)
library(behindbarstools)
library(scales)

state_df <- googlesheets4::read_sheet("14lsg8LwTA895-o3JDW3OOrCof57fQldKUEi8FNJIntI")

plot_df <- state_df %>% 
    select(State, Cases, Deaths, Tests, Population) %>% 
    mutate_at(c("Cases", "Deaths", "Tests", "Population"), as.numeric) %>% 
    mutate(Case.Rate = Cases / Population, 
           Testing.Rate = Tests / Population, 
           CF.Rate = Deaths / Cases, 
           TP.Rate = Cases / Tests) %>% 
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
spdf <- geojsonio::geojson_read(
    "https://raw.githubusercontent.com/uclalawcovid19behindbars/data-viz/master/data-templates/us_states_hexgrid.geojson", 
    what = "sp")

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
            aes(x = x, y = y, label = id), size = 4.0) + 
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
    labs(subtitle = "Number of COVID tests reported per person incarcerated in state prisons")
ggsave("testing_map.svg", width = 8, height = 5)
ggsave("testing_map.png", width = 8, height = 5)

plot_hex_map(joined_df, "CF.Bin", fill_CF, breaks_CF, breaks_CF) + 
    labs(subtitle = "Case fatality rates among incarcerated people in state prisons")
ggsave("cfr_map.svg", width = 8, height = 5)
ggsave("cfr_map.png", width = 8, height = 5)

plot_hex_map(joined_df, "TP.Bin", fill_TP, breaks_TP, breaks_TP) + 
    labs(subtitle = "Test positivity rates among incarcerated people in state prisons")
ggsave("tpr_map.svg", width = 8, height = 5)
ggsave("tpr_map.png", width = 8, height = 5)

# CFR lollipop plot 
plot_lollipop <- function(df, plot_var, filter_min) {
    ggplot(
        data = df %>% 
            filter(!!sym(plot_var) > filter_min) %>% 
            arrange(!!sym(plot_var)) %>% 
            mutate(State = factor(State, levels = State)) %>% 
            filter(!is.na(!!sym(plot_var))),  
        aes(x = State, y = !!sym(plot_var))) + 
        geom_segment(aes(xend = State, yend = 0), size = 0.8, color = "#b6b6a1") +
        geom_point(color = "#565629", size = 1.5) +
        geom_text(aes(
            label = percent(!!sym(plot_var), accuracy = 0.1)), 
            hjust = -0.4, color = "#565629", size = 3) + 
        coord_flip()  + 
        theme_classic(base_family = "Helvetica", base_size = 12)  + 
        barplot_theme 
}

ggplot(
    data = plot_df %>% 
        filter(Testing.Rate < Inf) %>%
        arrange(-Testing.Rate) %>% 
        mutate(State = factor(State, levels = State)) %>% 
        filter(!is.na(Testing.Rate)),  
    aes(x = State, y = Testing.Rate)) + 
    geom_segment(aes(xend = State, yend = 0), size = 0.8, color = "#b6b6a1") +
    geom_point(color = "#565629", size = 1.5) +
    geom_text(aes(
        label = comma(Testing.Rate, accuracy = 0.1)), 
        hjust = -0.4, color = "#565629", size = 3) + 
    coord_flip()  + 
    theme_classic(base_family = "Helvetica", base_size = 12) + 
    barplot_theme + 
    scale_y_continuous(
        limits = c(0, 45),
        expand = c(0, 0)) +
    labs(subtitle = "Number of COVID tests reported\nper person incarcerated in state prisons")
ggsave("testing.svg", width = 4, height = 10)
ggsave("testing.png", width = 4, height = 10)

plot_lollipop(plot_df, "CF.Rate", 0.0) + 
    scale_y_continuous(
        labels = percent_format(accuracy = 1.0), 
        limits = c(0, 0.041), 
        expand = c(0, 0)) + 
    labs(subtitle = "Case fatality rates among\nincarcerated people in state prisons")
ggsave("cfr.svg", width = 4, height = 10)
ggsave("cfr.png", width = 4, height = 10)

plot_lollipop(plot_df, "TP.Rate", 0.0) + 
    scale_y_continuous(
        labels = percent_format(accuracy = 1.0), 
        limits = c(0, 0.51),
        expand = c(0, 0)) + 
    labs(subtitle = "Test positivity rates among\nincarcerated people in state prisons")
ggsave("tpr.svg", width = 4, height = 10)
ggsave("tpr.png", width = 4, height = 10)

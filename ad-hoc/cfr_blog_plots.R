rm(list = ls())

library(tidyverse)
library(behindbarstools)

state_df <- read.csv(str_c(
    "https://raw.githubusercontent.com/uclalawcovid19behindbars/data/master/", 
    "latest-data/latest_state_counts.csv"))

plot_df <- state_df %>% 
    mutate(Residents.Tadmin = case_when(
        # Incorporate external testing data 
        # NYT: https://github.com/nytimes/covid-19-data/blob/master/prisons/systems.csv
        # TODO: Should probably take NYT numerators too here...? 
        State == "Arizona" ~ 47899, # from Marshall Project 
        State == "Florida" ~ 87677, 
        State == "Georgia" ~ 11947, 
        State == "Kansas" ~ 43391, 
        State == "Kentucky" ~ 26433, 
        State == "Nevada" ~ 12368, 
        State == "Rhode Island" ~ 41623, 
        State == "Washington" ~ 88491, # from DOC dashboard 
        TRUE ~ as.double(Residents.Tadmin)
    )) %>% 
    mutate(Residents.Population = case_when(
        State == "Minnesota" ~ 7078, 
        TRUE ~ as.double(Residents.Population)
    )) %>% 
    mutate(testing_rate = Residents.Tadmin / Residents.Population, 
           case_rate = Residents.Confirmed / Residents.Population, 
           cf_rate = Residents.Deaths / Residents.Confirmed, 
           tp_rate = Residents.Confirmed / Residents.Tadmin) %>% 
    mutate(testing_bin = case_when(
        is.na(testing_rate) ~ "No Data", 
        testing_rate <= 1 ~ "<1", 
        testing_rate > 1 & testing_rate <= 2 ~ "1 to 2", 
        testing_rate > 2 & testing_rate <= 3 ~ "2 to 3", 
        testing_rate > 3 ~ "3+"))
    
# Hex map skeleton 
spdf <- geojsonio::geojson_read("data/us_states_hexgrid.geojson", what = "sp")

spdf@data = spdf@data %>% 
    mutate(google_name = gsub(" \\(United States\\)", "", google_name))

spdf_fortified <- broom::tidy(spdf, region = "google_name")

centers <- cbind.data.frame(data.frame(
    rgeos::gCentroid(spdf, byid = TRUE), 
    id = spdf@data$iso3166_2))

joined_df <- spdf_fortified %>% 
    left_join(plot_df, by = c("id" = "State")) 

fill_testing <- c("<1" = "#d94701", 
                  "1 to 2" = "#fd8d3c", 
                  "2 to 3" = "#fdbe85", 
                  "3+" = "#feedde", 
                  "No Data" = "#d9d9d9")

# Testing rate hex map 
testing_plot <- ggplot() +
    geom_polygon(
        data = joined_df, 
        aes(x = long, y = lat, group = group, fill = testing_bin), 
        color = "white") + 
    geom_text(
        data = centers, 
        aes(x = x, y = y, label = id), size = 4) + 
    scale_fill_manual(
        values = fill_testing,
        breaks = c("<1", "1 to 2", "2 to 3", "3+", "No Data"), 
        labels = c("<1", "1 to 2", "2 to 3", "3+", "No Data")) + 
    coord_map() + 
    theme_behindbars(base_size = 14, base_color = "black") +
    theme(
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
        legend.title =        element_blank()) + 
    labs(title = "Several states rarely tested incarcerated people for COVID", 
        subtitle = "Number of COVID tests reported per person incarcerated in state prisons")

ggsave("testing_map.svg", width = 7, height = 6)

# Testing rate vs. CFR scatterplot  
ggplot(data = plot_df %>% 
           filter(State != "ICE") %>% 
           mutate(label_ = ifelse(State %in% c(
               "Alabama", "Georgia", "Mississippi", "Michigan", "Pennsylvania", 
               "Illinois", "New Jersey", "Texas", "California", "Ohio", "Florida"), 
               State, "")),  
       aes(x = testing_rate, y = cf_rate, label = label_)) + 
    geom_smooth(method = "lm", color = "#9d9987", fill = "#d4d5c6", alpha = 0.2, size = 0.5, linetype = "dashed") + 
    geom_point(aes(size = Residents.Population), shape = 21, fill = "#4d5271", color = "white") + 
    ggrepel::geom_text_repel(box.padding = 0.4) + 
    scale_size_continuous(labels = scales::comma, breaks = c(50000, 100000)) + 
    scale_y_continuous(labels = scales::percent_format(accuracy = 1.0)) + 
    labs(title = "States with the highest case fatality rates rarely tested for COVID",
         subtitle = "Case fatality and testing rates across state prisons", 
         y = "Case fatality rate\n(reported deaths / cases)", 
         x = "Testing rate\n(reported tests / incarcerated population)", 
         size = "Prison population") + 
    theme_classic(base_family = "Helvetica", base_size = 14) + 
    theme(
        panel.grid.major.y =  element_blank(), 
        plot.title =          element_text(hjust = 0.5), 
        plot.subtitle =       element_text(hjust = 0.5), 
        plot.caption =        element_text(margin = margin(t = 1.2 * 14)),
        axis.title.y =        element_text(margin = margin(r = 1.2 * 14)),
        legend.position = c(0.8, 0.8)
    )

# CFR lollipop plot 
ggplot(data = plot_df %>% 
           filter(cf_rate > 0.01) %>% 
           arrange(cf_rate) %>% 
           mutate(State = factor(State, levels = State)) %>% 
           filter(!State %in% c("ICE", "Federal", "District of Columbia")) %>% 
           filter(!is.na(cf_rate)),  
       aes(x = State, y = cf_rate)) + 
    geom_segment(aes(xend = State, yend = 0), size = 0.8, color = "#b6b6a1") +
    geom_point(color = "#565629", size = 2.9) +
    geom_text(aes(label = scales::percent(cf_rate, accuracy = 0.1)), hjust = -0.4, color = "#565629", size = 4) + 
    coord_flip() + 
    scale_y_continuous(labels = scales::percent_format(accuracy = 1.0), limits = c(0, 0.041), expand = c(0, 0)) + 
    theme_classic(base_family = "Helvetica", base_size = 14) + 
    theme(
        axis.ticks.y = element_blank(),
        panel.grid.major.y =  element_blank(), 
        plot.title =          element_text(hjust = 0.5), 
        plot.subtitle =       element_text(hjust = 0.5), 
        axis.title.y =        element_blank(),
        axis.title.x =        element_blank()
    ) + 
    labs(title = "12 state prison systems have case fatality rates above 1%", 
         subtitle = "COVID case fatality rates across state prisons")

rm(list=ls())

library(tidyverse)
library(behindbarstools)
library(ggmap)
library(maps)
library(mapdata)
library(ggimage)

# ------------------------------------------------------------------------------

all_dates_ <- read_scrape_data(all_dates = T, date_cutoff = "2020-03-01")
latest_ <- read_scrape_data(all_dates = F, date_cutoff = "2020-03-01")

# Total cases 
latest_ %>% 
    filter(Jurisdiction %in% c("federal", "state")) %>% 
    filter(Gender %in% c("Female")) %>% 
    summarise(sum_na_rm(Residents.Confirmed))

# >100 cases 
latest_ %>% 
    filter(Jurisdiction %in% c("federal", "state")) %>% 
    filter(Gender %in% c("Female")) %>% 
    filter(Residents.Confirmed > 100) %>% 
    nrow()

# >15% infection rates  
latest_ %>% 
    filter(Jurisdiction %in% c("federal", "state")) %>% 
    filter(Gender %in% c("Female")) %>% 
    mutate(Population.Feb20 = coalesce(Population.Feb20, Residents.Population)) %>% 
    mutate(pct = Residents.Confirmed / coalesce(Population.Feb20)) %>% 
    filter(pct > 0.15) %>% 
    nrow()

latest_ %>% 
    filter(Jurisdiction %in% c("federal", "state")) %>% 
    filter(Gender %in% c("Female")) %>% 
    filter(!is.na(Residents.Confirmed)) %>% 
    nrow()

# ------------------------------------------------------------------------------

all_dates <- read_scrape_data(all_dates = TRUE, state = "California")
latest <- read_scrape_data()

staff_pop <- latest %>% 
    filter(State == "California") %>% 
    filter(!is.na(Staff.Population)) %>% 
    select(Facility.ID, Staff.Population)

scale_fill_bbdiscrete_alt <- function(){
    ggplot2::discrete_scale(
        aesthetics = "fill", scale_name = "bbdiscrete",
        palette = function(n) {
            if (n > 6) stop("bbcolors palette only has 6 colors.")
            
            bbcolors <- c(
                "#D7790F", "#71A9C9", "#82CAA4", "#4C6788", "#84816F", "#AE91A8")
            bbcolors[1:n]
        })
}

theme_behindbars <- function(
    base_size = 24, base_family = "Helvetica", base_color = "#555526") {
    
    ggplot2::theme_classic(
        base_family = base_family,
        base_size = base_size
    ) +
        ggplot2::theme(
            text =                element_text(color = base_color),
            strip.text =          element_text(color = base_color),
            axis.text =           element_text(color = base_color),
            panel.grid.major.y =  element_line(color = "#92926C", linetype = "dotted"),
            plot.title.position = "plot",
            plot.tag.position =   "bottomright",
            axis.line.y =         element_blank(),
            axis.line =           element_line(color = base_color),
            axis.ticks =          element_line(color = base_color),
            plot.caption =        element_text(margin = margin(t = 1.2 * base_size)),
            plot.subtitle =       element_text(margin = margin(b = 1.2 * base_size)),
            axis.title.y =        element_text(margin = margin(r = 1.2 * base_size)),
            plot.tag =            element_text(size = base_size / 1.6, hjust = 0)
        )
}

latest %>% 
    filter(Jurisdiction == "state") %>% 
    filter(State == "California") %>% 
    filter(!is.na(Residents.Confirmed)) %>% 
    filter(Residents.Population > 20) %>% 
    mutate(Gender = ifelse(Gender == "Female", "Female", "Mixed")) %>% 
    arrange(desc(Gender)) %>% 
    ggplot(aes(x = Residents.Population, y = Residents.Confirmed)) +
    geom_abline(slope = 0.1, linetype = "dashed", color = "#5c5859") + 
    geom_abline(slope = 0.2, linetype = "dashed", color = "#5c5859") + 
    geom_abline(slope = 0.4, linetype = "dashed", color = "#5c5859") + 
    geom_abline(slope = 0.6, linetype = "dashed", color = "#5c5859") + 
    geom_abline(slope = 0.8, linetype = "dashed", color = "#5c5859") + 
    geom_point(aes(color = Gender, fill = Gender), 
               shape = 21, size = 5, color = "white") +
    theme_behindbars(base_color = "black", base_size = 18) + 
    scale_x_continuous(label = scales::comma, limits = c(0, 5200), expand = c(0, 0)) +
    scale_y_continuous(label = scales::comma, limits = c(0, 4000), expand = c(0, 0)) +
    scale_fill_bbdiscrete_alt() +     
    labs(x = "Current Incarcerated Population",
         y = "Cumulative COVID-19 Cases") +
    theme_behindbars() + 
    theme(
        axis.line.y = element_line(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_blank(), 
        axis.title.x = element_text(margin = margin(t = 1.2 * 18)), 
        legend.position = "none") 

ggsave("cdcr_case_rates.svg", width = 11, height = 9)

# ------------------------------------------------------------------------------

all_dates %>% 
    filter(Name == "CALIFORNIA INSTITUTION FOR WOMEN") %>% 
    filter(!is.na(Residents.Confirmed)) %>% 
    ggplot() + 
    geom_line(aes(x = Date, y = Residents.Population), size = 1.0, color = "#4C6788") + 
    geom_line(aes(x = Date, y = Residents.Confirmed), size = 1.0, color = "#D7790F") + 
    geom_hline(yintercept = 1281, linetype = "dashed", size = 1.0, color = "#84816F") +
    scale_y_continuous(label = scales::comma, limits = c(0, 2000)) + 
    scale_x_date(breaks = scales::pretty_breaks(n = 7), date_labels =  "%b '%y") + 
    labs(title = "California Institution for Women") + 
    theme_behindbars() + 
    theme(
        axis.title.x = element_blank(),
        axis.ticks.y = element_line(color = "#555526"), 
        axis.title.y = element_blank(), 
        axis.line.y = element_line(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_blank())

ggsave("ciw.svg", width = 9, height = 6)

all_dates %>% 
    filter(Name == "CENTRAL CALIFORNIA WOMENS FACILITY") %>% 
    filter(!is.na(Residents.Confirmed)) %>% 
    ggplot() + 
    geom_line(aes(x = Date, y = Residents.Population), size = 1.0, color = "#4C6788") + 
    geom_line(aes(x = Date, y = Residents.Confirmed), size = 1.0, color = "#D7790F") + 
    geom_hline(yintercept = 1990, linetype = "dashed", size = 1.0, color = "#84816F") +
    scale_y_continuous(label = scales::comma, limits = c(0, 3000)) +
    scale_x_date(breaks = scales::pretty_breaks(n = 7), date_labels =  "%b '%y") + 
    labs(title = "Central California Women's Facility") + 
    theme_behindbars() + 
    theme(
        axis.title.x = element_blank(),
        axis.ticks.y = element_line(color = "#555526"), 
        axis.title.y = element_blank(), 
        axis.line.y = element_line(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_blank())

ggsave("ccwf.svg", width = 9, height = 6)

# ------------------------------------------------------------------------------

# Active cases 
p <- all_dates %>% 
    filter(Name %in% c(
        "CENTRAL CALIFORNIA WOMENS FACILITY", 
        "CALIFORNIA INSTITUTION FOR WOMEN")) %>% 
    filter(!is.na(Residents.Confirmed)) %>%
    filter(Date < "2021-05-01") %>% 
    group_by(Facility.ID) %>% 
    mutate(Res.Act.Est = diff_roll_sum(Residents.Confirmed, Date)) %>% 
    mutate(rollavg = zoo::rollmean(Res.Act.Est, k = 5, fill = NA)) %>% 
    mutate(rollavg = ifelse(rollavg < 0, 0, rollavg)) %>% 
    ungroup() %>% 
    ggplot() + 
    geom_line(aes(x = Date, y = rollavg, color = Name), size = 1.0) + 
    theme_behindbars() + 
    theme(
        legend.position = "none",
        axis.ticks.y = element_line(color = "#555526"), 
        axis.title.y = element_blank(), 
        axis.line.y = element_line(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_blank())

ggsave("active.svg", width = 10, height = 6)

# ------------------------------------------------------------------------------

female <- latest_ %>% 
    filter(Gender == "Female") %>% 
    filter(!Age %in% c("Juvenile")) %>% 
    filter(Jurisdiction %in% c("state", "federal")) %>% 
    select(longitude = "Longitude", 
           latitude = "Latitude", 
           Name, Residents.Confirmed, Jurisdiction, State) %>% 
    filter(Residents.Confirmed >= 100)

cross_asset <- "https://raw.githubusercontent.com/uclalawcovid19behindbars/data-viz/master/data-templates/cross.png"

female_transformed <- usmap::usmap_transform(female) %>% 
    mutate(image = sample(c(cross_asset))) 

state <- female_transformed %>% 
    filter(Jurisdiction == "state")

federal <- female_transformed %>% 
    filter(Jurisdiction == "federal")

usmap::plot_usmap(fill = "#f3f3ed", color = "#e5e5dc") +
    ggimage::geom_image(data = state, aes(x = longitude.1, y = latitude.1, image = image),
                        color = "#DA7C0F", size = 0.02) + 
    ggimage::geom_image(data = federal, aes(x = longitude.1, y = latitude.1, image = image),
                        color = "#9DC183", size = 0.02) 

ggsave("map.png", width = 8, height = 8, bg = "transparent")
ggsave("map.svg", width = 8, height = 8)

# ------------------------------------------------------------------------------

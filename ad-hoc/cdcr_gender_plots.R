library(tidyverse)
library(behindbarstools)

# Get historical active staff data 
ind_vars <- c("Date", "Name", "State")

combined <- list.files("results/extracted_data/", full.names = TRUE) %>%
    lapply(function(x){
        df_ <- read_csv(x, col_types = cols())
        if("Date" %in% names(df_)){
            df_ <- df_ %>%
                mutate(Date = lubridate::as_date(Date)) %>%
                mutate_at(vars(starts_with("Residents")), as.numeric) %>%
                mutate_at(vars(starts_with("Staff")), as.numeric)
        }
        df_
    }) %>%
    bind_rows() %>%
    # select(-Resident.Deaths) %>%
    # remove values if they are missing a data name or state
    filter(!is.na(Date) & !is.na(Name) & State != "") %>%
    # order the names alphabetically
    select(!!sort(names(.))) %>%
    # put the indicator variables first
    select(!!ind_vars, !!(names(.)[!(names(.) %in% ind_vars)])) %>%
    filter(!is.na(id), !is.na(jurisdiction)) 

write_csv(combined, "results/combined_ca_staff.csv")

# ------------------------------------------------------------------------------

all_dates <- read_scrape_data(all_dates = TRUE)
latest <- read_scrape_data()

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

case_rates <- latest %>% 
    filter(Jurisdiction == "state") %>% 
    filter(State == "California") %>% 
    filter(!is.na(Residents.Confirmed)) %>% 
    mutate(Gender = ifelse(Gender == "Female", "Female", "Mixed")) %>% 
    arrange(desc(Gender)) %>% 
    ggplot(aes(x = Residents.Population, y = Residents.Confirmed)) +
    geom_abline(slope = 0.09, linetype = "dashed", color = "#5c5859") + 
    geom_abline(slope = 0.2, linetype = "dashed", color = "#5c5859") + 
    geom_abline(slope = 0.4, linetype = "dashed", color = "#5c5859") + 
    geom_abline(slope = 0.6, linetype = "dashed", color = "#5c5859") + 
    geom_abline(slope = 0.8, linetype = "dashed", color = "#5c5859") + 
    geom_abline(slope = 1.0, linetype = "dashed", color = "#5c5859") + 
    geom_point(aes(color = Gender, fill = Gender), 
               shape = 21, size = 5, color = "white") +
    theme_behindbars(base_color = "black", base_size = 18) + 
    scale_x_continuous(label = scales::comma, limits = c(0, 5000)) + 
    scale_y_continuous(label = scales::comma, limits = c(0, 4000)) + 
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

ggsave("ca_staff_vax.png", staff_vax, width = 9, height = 5)

ciw <- all_dates %>% 
    filter(Name == "CALIFORNIA INSTITUTION FOR WOMEN") %>% 
    filter(!is.na(Residents.Confirmed)) %>% 
    ggplot() + 
    geom_line(aes(x = Date, y = Residents.Population), size = 1.0, color = "#4C6788") + 
    geom_line(aes(x = Date, y = Residents.Confirmed), size = 1.0, color = "#D7790F") + 
    geom_hline(yintercept = 1298, linetype = "dashed", size = 1.0, color = "#84816F") +
    scale_y_continuous(label = scales::comma, limits = c(0, 2000)) + 
    labs(title = "Total Incarcerated Population",
         subtitle = "California Institution for Women") + 
    theme_behindbars() + 
    theme(
        axis.ticks.y = element_line(color = "#555526"), 
        axis.title.y = element_blank(), 
        axis.line.y = element_line(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_blank())

ggsave("ccwf.svg", ccwf, width = 10, height = 8)

# Staff / resident cumulative cases 
staff_ccwf <- all_dates %>% 
    filter(Name == "CENTRAL CALIFORNIA WOMENS FACILITY") %>% 
    filter(Date > "2020-11-01") %>%
    filter(Date < "2021-02-01") %>%
    left_join(combined %>% 
                  filter(Name == "Central California Womens Facility CCWF"), by = "Date") %>%
    filter(!is.na(Staff.Confirmed.x) & !is.na(Residents.Confirmed)) %>%
    ggplot() + 
    geom_line(aes(x = Date, y = Residents.Confirmed), size = 1.0, color = "#D7790F") + 
    geom_line(aes(x = Date, y = Staff.Confirmed.x), size = 1.0, color = "#4C6788") + 
    theme_behindbars() + 
    scale_y_continuous(limits = c(0, 800)) + 
    theme(
        axis.ticks.y = element_line(color = "#555526"), 
        axis.title.y = element_blank(), 
        axis.line.y = element_line(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_blank())
    
# Dual axis version  
coeff <- 3
staff_ccwf <- all_dates %>% 
    filter(!is.na(Staff.Confirmed) & !is.na(Residents.Confirmed)) %>% 
    filter(Date < as.Date("2021-02-15")) %>% 
    filter(Date > as.Date("2020-10-01")) %>% 
    filter(Name == "CENTRAL CALIFORNIA WOMENS FACILITY") %>% 
    ggplot() + 
    geom_line(aes(x = Date, y = Residents.Confirmed / 5), size = 1.0, color =  "#D7790F") + 
    geom_line(aes(x = Date, y = Staff.Confirmed), size = 1.0, color = "#4C6788") + 
    scale_y_continuous(
        limits = c(0, 280), breaks = c(0, 50, 100, 150, 200, 250),  
        name = "Confirmed Cases Among Staff",
        sec.axis = sec_axis(~.*coeff, name = "Confirmed Cases Among Incarcerated People")
    ) + 
    theme_behindbars() + 
    theme(
        axis.ticks.y = element_line(color = "#555526"), 
        axis.title.y = element_blank(), 
        axis.line.y = element_line(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_blank())

ggsave("staff_ccwf.svg", staff_ccwf, width = 10, height = 8)

# Vaccines 
p <- latest %>% 
    filter(State == "California") %>% 
    filter(Jurisdiction == "state") %>% 
    mutate(Name = case_when(
        Name %in% c(
            "CENTRAL CALIFORNIA WOMENS FACILITY", 
            "CALIFORNIA INSTITUTION FOR WOMEN") ~ Name, 
        TRUE ~ "ALL OTHER CDCR FACILITIES")) %>% 
    group_by(Name) %>% 
    summarise(Staff.Initiated = sum_na_rm(Staff.Initiated), 
              Residents.Initiated = sum_na_rm(Residents.Initiated), 
              Staff.Population = sum_na_rm(Staff.Population), 
              Residents.Population = sum_na_rm(Residents.Population)) %>% 
    mutate(staff_pct = Staff.Initiated / Staff.Population, 
           res_pct = Residents.Initiated / Residents.Population) %>% 
    filter(!is.na(res_pct) & !is.na(staff_pct)) %>% 
    ggplot() +
    geom_dumbbell(aes(x = staff_pct, 
                      xend = res_pct, 
                      y = str_to_title(Name)), 
                  size_x = 3, 
                  size_xend = 3,
                  colour = "#c1c4b9",
                  size = 1.0,
                  colour_x = "#D7790F", 
                  alpha = 0.8,
                  colour_xend = "#4C6788") + 
    scale_x_continuous(label = scales::percent, limits = c(0, 1)) + 
    theme_classic(base_family = base_family, base_size = base_size) + 
    theme(
        text =                element_text(color = base_color),
        strip.text =          element_text(color = base_color),
        axis.text =           element_text(color = base_color),
        panel.grid.major.x =  element_line(color = "#92926C", linetype = "dotted"),
        panel.grid.major.y =  element_line(color = "#92926C", linetype = "dotted"),
        plot.title.position = "plot",
        plot.tag.position =   "bottomright",
        axis.line.y =         element_blank(),
        axis.ticks.y =        element_blank(),
        axis.title.x =        element_blank(),
        axis.line =           element_line(color = base_color),
        axis.ticks =          element_line(color = base_color),
        plot.caption =        element_text(margin = margin(t = 1.2 * base_size)),
        plot.subtitle =       element_text(margin = margin(b = 1.2 * base_size)),
        axis.title.y =        element_blank()
    ) 

ggsave("vax.svg", p, width = 8, height = 4)

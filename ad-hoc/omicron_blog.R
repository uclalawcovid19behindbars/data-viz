library(tidyverse)
library(behindbarstools)
library(scales)

df <- read_scrape_data(all_dates = TRUE)

## california
ca_df <- df %>% 
    filter(Facility.ID %in% c(
        83,
        171,
        141,
        120,
        135,
        1725
    )) %>% 
    filter(!is.na(Residents.Active)) %>%
    mutate(Staff.Active = replace_na(Staff.Active, 0)) %>% 
    mutate(Total.Active = Residents.Active + Staff.Active) 

## california plot
ca_plot <- ca_df %>%
    mutate(Label = str_to_title(Name), 
           # Label = str_replace(Label, "Federal Correctional Institution", "FCI"),
           # Label = str_replace(Label, "United States Penitentiary", "USP"),
           # Label = str_replace(Label, "Federal Correctional Complex", "FCC"), 
           # Label = str_replace(Label, "Metropolitan Correctional Center", "MCC"), 
           Label = str_c(Label, 
                         " (", State, ")"), 
           outbreak_ = case_when(
               Residents.Active / Residents.Population > 0.05 ~ "Worst outbreak (5+% actively infected)",
               Total.Active >= 5 ~ "Outbreak (5+ active cases)", 
               TRUE ~ "No outbreak (<5 active cases)")) %>% 
    ggplot(aes(x = Date, y = Residents.Active)) + 
    geom_line(size = 0.6, color = "#fee6ce") + 
    geom_point(aes(color = outbreak_), size = 0.6) + 
    facet_wrap(~Label, nrow = 3, scales = "free") + 
    scale_x_date(breaks = pretty_breaks(n = 3), label = date_format(format = "%b '%y")) + 
    theme_behindbars(base_size = 12) + 
    labs(y = "Active cases among incarcerated people") + 
    theme(strip.background = element_blank(), 
          legend.title = element_blank(), 
          legend.position = "top") + 
    scale_color_manual(values = c("#fee6ce", "#fdae6b", "#e6550d"))
ggsave("ca_omicron.png", ca_plot, width = 7, height = 5)


##illinois



## bop
bop_df <- df %>% 
    filter(Facility.ID %in% c(
        2433,
        2450,
        2330,
        2379,
        2369,
        2445
    )) %>% 
    filter(!is.na(Residents.Active)) %>%
    mutate(Staff.Active = replace_na(Staff.Active, 0)) %>% 
    mutate(Total.Active = Residents.Active + Staff.Active) 

##  bop plot
bop_plot <- bop_df %>%
    mutate(Label = str_to_title(Name), 
           Label = str_replace(Label, "Federal Correctional Institution", "FCI"),
           Label = str_replace(Label, "United States Penitentiary", "USP"),
           Label = str_replace(Label, "Federal Correctional Complex", "FCC"),
           Label = str_replace(Label, "Metropolitan Correctional Center", "MCC"),
           Label = str_c(Label, 
                         " (", State, ")"), 
           outbreak_ = case_when(
               Residents.Active / Residents.Population > 0.05 ~ "Worst outbreak (5+% actively infected)",
               Total.Active >= 5 ~ "Outbreak (5+ active cases)", 
               TRUE ~ "No outbreak (<5 active cases)")) %>% 
    ggplot(aes(x = Date, y = Residents.Active)) + 
    geom_line(size = 0.6, color = "#fee6ce") + 
    geom_point(aes(color = outbreak_), size = 0.6) + 
    facet_wrap(~Label, nrow = 3, scales = "free") + 
    scale_x_date(breaks = pretty_breaks(n = 3), label = date_format(format = "%b '%y")) + 
    theme_behindbars(base_size = 12) + 
    labs(y = "Active cases among incarcerated people") + 
    theme(strip.background = element_blank(), 
          legend.title = element_blank(), 
          legend.position = "top") + 
    scale_color_manual(values = c("#fee6ce", "#fdae6b", "#e6550d"))
ggsave("bop_omicron.png", bop_plot, width = 9, height = 5)

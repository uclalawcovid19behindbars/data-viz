library(tidyverse)
library(behindbarstools)
library(scales)

## read in data
df <- read_scrape_data(all_dates = TRUE)
hist_state <- read_csv("https://raw.githubusercontent.com/uclalawcovid19behindbars/data/master/historical-data/historical_state_counts.csv")


## for data look-up purposes, only keep facs with non-NA values for res.active
active_df <- df %>%
    filter(!is.na(Residents.Active)) %>%
    mutate(Total.Active = vector_sum_na_rm(Residents.Active, Staff.Active)) 

# ------------------------------------------------------------------------------

# How many prisons have experienced at least one outbreak after december 1st?
active_df %>% 
    select(Date, Facility.ID, Name, State, Jurisdiction, Residents.Active, Staff.Active, Residents.Population, Total.Active) %>% 
    filter(Date > "2021-12-01") %>% 
    group_by(Facility.ID, Jurisdiction) %>% 
    summarise(max_ = max(Total.Active, na.rm = TRUE)) %>% 
    filter(max_ >= 5, 
           Jurisdiction != "county") %>% ## rm jails from this statement
    nrow()

## How many facilities have been in constant outbreak since omicron was first detected?
outbreak_duration <- active_df  %>% 
    filter(Date > "2021-12-01") %>% 
    group_by(Facility.ID, Jurisdiction) %>% 
    mutate(outbreak_ = ifelse(Total.Active >= 5, 1, 0)) %>% 
    summarise(n_outbreak = sum(outbreak_), 
              n = n(), 
              pct = n_outbreak / n) 

outbreak_duration %>%
    filter(pct == 1,
           Jurisdiction != "county") %>%
    nrow()

## what percentage of facilities are experiencing an outbreak right now?
pct_outbreak <- active_df %>% 
    group_by(Date) %>% 
    mutate(outbreak_ = ifelse(Total.Active >= 5, 1, 0)) %>% 
    summarise(n_outbreak = sum(outbreak_), 
              n = n(), 
              pct = n_outbreak / n)
View(pct_outbreak)

# ------------------------------------------------------------------------------

# Share of facilities with outbreaks over time 
outbreaks_overtime <- pct_outbreak %>%
    filter(n > 150) %>%
    ggplot(aes(x = Date, y = pct)) + 
    geom_line(size = 1.0, color = "#4C6788") + 
    scale_y_continuous(limits = c(0, 1), labels = percent) +
    scale_x_date(breaks = pretty_breaks(n = 6), label = date_format(format = "%b '%y")) +
    theme_behindbars(base_size = 16) + 
    labs(y = "Percent of facilities with COVID outbreak")
ggsave("outbreaks_overtime.png", outbreaks_overtime, width = 7, height = 5)

# ------------------------------------------------------------------------------

## state-specific graphs

# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# california
ca_df <- df %>% 
    filter(Facility.ID %in% c(
        83,
        171,
        141,
        120
        # 135, # LA county jails
        # 1725
    )) %>% 
    filter(!is.na(Residents.Active)) %>%
    mutate(Staff.Active = replace_na(Staff.Active, 0)) %>% 
    mutate(Total.Active = Residents.Active + Staff.Active) 

## california plot
ca_plot <- ca_df %>%
    mutate(Label = str_to_title(Name), 
           outbreak_ = case_when(
               Residents.Active / Residents.Population > 0.05 ~ "Worst outbreak (5+% actively infected)",
               Total.Active >= 5 ~ "Outbreak (5+ active cases)", 
               TRUE ~ "No outbreak (<5 active cases)")) %>% 
    ggplot(aes(x = Date, y = Residents.Active)) + 
    geom_line(size = 0.6, color = "#fee6ce") + 
    geom_point(aes(color = outbreak_), size = 0.6) + 
    facet_wrap(~Label, nrow = 2, scales = "free") + 
    scale_x_date(breaks = pretty_breaks(n = 3), label = date_format(format = "%b '%y")) + 
    theme_behindbars(base_size = 12) + 
    labs(y = "Active cases among incarcerated people") + 
    theme(strip.background = element_blank(), 
          legend.title = element_blank(), 
          legend.position = "top") + 
    scale_color_manual(values = c("#fee6ce", "#fdae6b", "#e6550d"))
ggsave("ca_omicron.png", ca_plot, width = 7, height = 5)


# ------------------------------------------------------------------------------
# illinois

daily_il_plot <- hist_state %>% 
    filter(State == "Illinois") %>%
    ## uncomment line below to show all dates
    filter(Date > as.Date("2021-11-01")) %>%
    ggplot() + 
    geom_bar(aes(x = Date, y = Residents.Active), stat = "identity") + 
    theme_behindbars(base_size = 14, base_color = "black") + 
    scale_y_continuous(label = scales::comma) +
    theme(legend.position = "right", legend.title = element_blank()) + 
    scale_fill_manual(values = c("#9DC183", "#664d60", "#D7790F")) + 
    scale_x_date(breaks = scales::pretty_breaks(n = 12)) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    labs(title = "COVID Daily Cases in Illinois Prisons", 
         y = "Reported Active Cases")
ggsave("illinois_daily.png", daily_il_plot, width = 7, height = 5)

# ------------------------------------------------------------------------------
# BOP
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

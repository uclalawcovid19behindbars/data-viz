library(behindbarstools)
library(tidyverse)

scrape_df <- read_scrape_data(T, T, state = "California")

# Recent spikes - active cases 
plt1 <- plot_recent_fac_increases(scrape_df = scrape_df,
                          plot_days = 30, 
                          num_fac = 4, 
                          annotate = TRUE, 
                          metric = "Residents.Active") + 
    labs(color = "Facility", 
         y = "Reported Active Cases", 
         title = "Facilities with Recent Spikes in Active COVID-19 Cases")

ggsave("spikes_CA_active_1.12.20.svg", plt1, width = 16, height = 10)
ggsave("spikes_CA_active_1.12.20.png", plt1, width = 16, height = 10)

plt2 <- plot_recent_fac_increases(scrape_df = scrape_df,
                                  plot_days = 30, 
                                  num_fac = 5, 
                                  annotate = TRUE, 
                                  metric = "Residents.Confirmed") + 
    labs(color = "Facility", 
         y = "Reported Cumulative Cases", 
         title = "Facilities with Recent Spikes in Cumulative COVID-19 Cases")

ggsave("spikes_CA_cumulative_1.12.20.svg", plt2, width = 16, height = 10)
ggsave("spikes_CA_cumulative_1.12.20.png", plt2, width = 16, height = 10)


# CMC CA Men's Colony - active 
plt3 <- scrape_df %>% 
    filter(Name == "CALIFORNIA MENS COLONY") %>% 
    filter(Date > "2020-12-25") %>% 
    mutate(last_value = if_else(Date == max(Date), as.character(Residents.Active), NA_character_)) %>%
    ggplot(aes(x = Date, y = Residents.Active, label = last_value)) + 
    geom_line(size = 2.0, color = "#D7790F") + 
    geom_area(aes(x = Date, y = Residents.Active), fill = "#D7790F", alpha = 0.5) + 
    scale_x_date(date_labels = "%b %d") +
    ggrepel::geom_text_repel(na.rm = T, show.legend = F, size = 6, nudge_x = 1, point.padding = 0.1) +
    theme_behindbars() + 
    scale_y_continuous(labels = scales::comma) + 
    labs(title = "30% of People Incarcerated \nat California's Men Colony (CMC) \nin San Luis Obispo County Have COVID-19", 
         y = "Reported Active Cases") + 
    theme(axis.text.y = element_text(vjust = -0.6, hjust = 0, margin = margin(r = -45)))
         
ggsave("cmc_active_1.12.20.svg", plt3, width = 14, height = 10)
ggsave("cmc_active_1.12.20.png", plt3, width = 14, height = 10)


# CMC CA Men's Colony - cumulative 
historical_CA <- read.csv("https://raw.githubusercontent.com/uclalawcovid19behindbars/historical-data/main/data/CA_adult_facility_covid_counts_historical.csv")

cmc <- historical_CA %>% 
    filter(Name == "CMC CALIFORNIA MENS COLONY") %>%
    mutate(Date = lubridate::ymd(Date)) %>% 
    bind_rows(scrape_df %>% filter(Name == "CALIFORNIA MENS COLONY"))

plt4 <- cmc %>% 
    filter(!is.na(Residents.Confirmed)) %>% 
    select(Date, Residents.Confirmed) %>% 
    distinct() %>% 
    mutate(last_value = if_else(Date == max(Date), as.character(Residents.Confirmed), NA_character_)) %>%
    ggplot(aes(x = Date, y = Residents.Confirmed, label = last_value)) + 
    geom_line(size = 2.0, color = "#D7790F") + 
    geom_area(fill = "#D7790F", alpha = 0.5) +
    scale_x_date(date_labels = "%b %d", breaks = "2 month") +
    ggrepel::geom_text_repel(na.rm = T, show.legend = F, size = 6, nudge_x = 1, point.padding = 0.1) +
    theme_behindbars() + 
    scale_y_continuous(labels = scales::comma, limits = c(0, 2000)) + 
    labs(title = "Cumulative COVID-19 Cases \nin California's Men Colony (CMC) in San Luis Obispo County", 
         y = "Cumulative Cases") + 
    theme(axis.text.y = element_text(vjust = -0.6, hjust = 0, margin = margin(r = -45)))

ggsave("cmc_cumulative_1.12.20.svg", plt4, width = 14, height = 10)
ggsave("cmc_cumulative_1.12.20.png", plt4, width = 14, height = 10)

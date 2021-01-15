library(tidyverse)
library(behindbarstools)
library(ggrepel)

# Load data 
scrape_ca <- read_scrape_data(T, T, state = "California") 

# Update facility spikes plot to show county 
plot_recent_fac_increases <- function(
    scrape_df = NULL, metric = "Residents.Confirmed", 
    plot_days = NULL, delta_days = 7, num_fac = 4, annotate = F, auto_label = F) {
    
    if (is.null(scrape_df)) {
        scrape_df <- read_scrape_data(T, T)
    }
    
    plot_end_date <-  max(scrape_df$Date)
    delta_start_date <- plot_end_date - lubridate::days(delta_days)
    if (is.null(plot_days)) {
        plot_start_date <- min(scrape_df$Date)
    }
    else {
        plot_start_date <- plot_end_date - lubridate::days(plot_days)
    }
    
    fac_delta_df <- scrape_df %>%
        filter(!(str_detect(Name, "(?i)state") & str_detect(Name, "(?i)wide"))) %>%
        filter(Date >= delta_start_date) %>%
        group_by(Name, State) %>%
        mutate(delta = last(!!sym(metric)) - first(!!sym(metric))) %>%
        ungroup() %>%
        filter(delta %in% head(sort(unique(delta), decreasing = T), n = num_fac)) %>%
        select(State, Name) %>%
        unique() %>%
        left_join(scrape_df, by = c("State", "Name")) %>% 
        mutate(Title = str_c(Name, "\n", str_to_title(County), " County", "\n")) %>%
        mutate(last_value = if_else(Date == max(Date), as.character(!!sym(metric)), NA_character_))
    
    fac_delta_df %>%
        filter(Date >= plot_start_date) %>%
        filter(!is.na(!!sym(metric))) %>%
        ggplot(aes(x = Date, y = !!sym(metric), color = Title, label = last_value)) +
        geom_line(size = 2.0) +
        geom_point(size = 3.0, show.legend = F) +
        {if (annotate) 
            geom_text_repel(na.rm = T, show.legend = F, size = 6, nudge_x = 1, point.padding = 0.1)} + 
        {if (auto_label) 
            labs(title = str_c("Facilities with Recent Spikes in ", get_metric_description(metric, short = T)),
                 subtitle = get_metric_description(metric),
                 x = "Date",
                 y = get_metric_description(metric, short = T),
                 color = "Facility",
                 tag = str_to_upper("UCLA Law COVID-19\nBehind Bars Data Project\ncovid19behindbars.org"))} +
        scale_x_date(date_labels = "%b %d", 
                     date_breaks = "2 weeks", 
                     limits = c(plot_start_date, plot_end_date),  
                     expand = c(0.15, 0)) + 
        scale_color_bbdiscrete() + 
        theme_behindbars() +
        theme(axis.text.y = element_text(vjust = -0.6, hjust = 0, margin = margin(r = -45))) 
}

# Cumulative cases faceted plot 
out <- ggplot(data = scrape_ca %>%
                  mutate(Name = str_replace(Name, "CALIFORNIA", "CA")) %>% 
                  filter(Date > as.Date("2020-10-20")) %>% 
                  filter(!is.na(Residents.Confirmed)), 
              aes(x = Date, y = Residents.Confirmed)) + 
    geom_line(color = "#D7790F", size = 1.5) + 
    facet_wrap(~ Name, scales = "free_y", ncol = 5, labeller = label_wrap_gen(width = 25)) + 
    theme_behindbars() + 
    scale_y_continuous(breaks = scales::extended_breaks(n = 4)) + 
    theme(axis.text.y = element_text(size = 14), 
          axis.text.x = element_text(size = 14, angle = 45, hjust = 1), 
          panel.grid.major.x = element_line(color = "#92926C", linetype = "dotted")) + 
    labs(title = "Cumulative COVID-19 Cases Among Residents in California Jails and Prisons", 
         subtitle = "October 20 - December 23, 2020", 
         y = "Cumulative COVID-19 Cases", 
         caption = "UCLA LAW COVID-19 BEHIND BARS DATA PROJECT\nUCLACOVIDBEHINDBARS.ORG")

ggsave("ca_residents_confirmed.png", out, width = 20, height = 22)

# Recent spikes plot 
out_spikes <- plot_recent_fac_increases(
    scrape_ca %>% 
        filter(Date > as.Date("2020-10-20")) %>% 
        mutate(County = case_when(Name == "ORANGE COUNTY JAILS" ~ "ORANGE",
                                  TRUE ~ County)), 
    num_fac = 5, delta_days = 7, annotate = T) + 
    labs(title = "California Jails and Prisons with Spikes in COVID-19 Cases", 
         subtitle = "October 20 - December 23, 2020", 
         y = "Cumulative Cases Among Incarcerated People", 
         color = "Facility", 
         tag = str_to_upper("UCLA Law COVID-19\nBehind Bars Data Project\ncovid19behindbars.org")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
          plot.tag.position = c(0.85, 0.02), 
          axis.title.x = element_blank()) 

ggsave("ca_recent_spikes.png", out_spikes, width = 16, height = 10)

# Los Angeles plot 
out_laj <- plot_fac_trend(fac_name = "Los Angeles Jails", 
                          metric = "Residents.Active", 
                          plot_days = 30, 
                          state = "California", 
                          annotate = T, 
                          auto_label = T) 

ggsave("la_jails_active.png", out_laj, width = 14, height = 10)

# Orange County plot 
out_oc <- plot_fac_trend(fac_name = "ORANGE COUNTY JAILS", 
                          metric = "Residents.Confirmed", 
                          plot_days = 30, 
                          state = "California", 
                          annotate = T, 
                          auto_label = T) 

ggsave("oc_jails_alt.png", out_oc, width = 14, height = 10)

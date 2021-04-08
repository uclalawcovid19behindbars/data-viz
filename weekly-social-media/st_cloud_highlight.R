scrape_mn <- read_scrape_data(all_dates = TRUE, state = "Minnesota")

all <- scrape_mn %>% 
    mutate(pct_vac = Residents.Initiated / Residents.Population) %>% 
    filter(!is.na(pct_vac)) %>% 
    filter(Date > as.Date("2021-03-01"))

st_cloud <- all %>% 
    filter(Name == "SAINT CLOUD CORRECTIONAL FACILITY") 

vac <- ggplot() + 
    geom_line(all, 
              mapping = aes(x = Date, y = pct_vac, group = Name), 
              color = "#C6C6C6", size = 1.5) + 
    geom_line(st_cloud, 
              mapping = aes(x = Date, y = pct_vac), 
              color = "#D7790F", size = 2.0) +
    scale_y_continuous(label = scales::percent, limits = c(0, 1)) + 
    scale_x_date(date_labels = "%b %d", 
                 breaks = "10 days", 
                 limits = c(as.Date("2021-03-01"), as.Date("2021-04-05"))) + 
    theme_behindbars() + 
    theme(axis.text.y = element_text(vjust = -0.6, hjust = 0, margin = margin(r = -65))) +  
    labs(title = "Vaccinations in Minnesota State Prisons", 
         y = "Percentage Vaccinated") 

ggsave("st_cloud_vac.png", vac, width = 14, height = 10)
ggsave("st_cloud_vac.svg", vac, width = 14, height = 10)

active <- plot_fac_trend(fac_name = "SAINT CLOUD CORRECTIONAL FACILITY", 
               state = "Minnesota", 
               metric = "Residents.Active",
               scrape_df = scrape_mn %>% 
                   filter(Date > as.Date("2021-03-01")), 
               auto_label = TRUE, 
               area_plot = TRUE) + 
    scale_y_continuous(limits = c(0, 80)) + 
    scale_x_date(date_labels = "%b %d", 
                 breaks = "10 days", 
                 limits = c(as.Date("2021-03-01"), as.Date("2021-04-05"))) + 
    theme(axis.text.y = element_text(vjust = -0.6, hjust = 0, margin = margin(r = -35))) + 
    labs(title = "Active COVID-19 Cases in St. Cloud Correctional Facility", 
         caption = NULL, 
         subtitle = NULL)

ggsave("st_cloud_active.png", active, width = 14, height = 10)
ggsave("st_cloud_active.svg", active, width = 14, height = 10)

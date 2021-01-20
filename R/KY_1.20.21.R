library(behindbarstools)

scrape_KY <- read_scrape_data(T, T, state = "Kentucky")

out <- plot_recent_fac_increases(scrape_df = scrape_KY, 
                          metric = "Residents.Active", 
                          plot_days = 30, 
                          num_fac = 2, 
                          auto_label = TRUE, 
                          annotate = TRUE) + 
    scale_color_manual(values = c("#D7790F", "#4C6788")) + 
    labs(tag = "")

ggsave("KY_1.20.21.png", out, width = 16, height = 10)
ggsave("KY_1.20.21.svg", out, width = 16, height = 10)

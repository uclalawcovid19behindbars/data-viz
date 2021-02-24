library(behindbarstools)
library(tidyverse)

scrape_az <- read_csv("https://raw.githubusercontent.com/uclalawcovid19behindbars/historical-data/main/data/AZ-historical-data.csv")

plt1 <- scrape_az %>%
  group_by(Name, Jurisdiction) %>%
  # calculate values by name and jurisdication for all of state of NC
  mutate(Res.Act.Est = diff_roll_sum(Residents.Confirmed, Date)) %>%
  # filter here only for the example plot
  filter(Name == "SAFFORD STATE PRISON") %>%
  ggplot(aes(
    x = Date, y = Res.Act.Est, color = Name, fill = Name)) +
  geom_line(size = 1.5) +
  geom_area(alpha = .5) +
  theme_behindbars() +
  scale_color_bbdiscrete() +
  scale_fill_bbdiscrete() +
  theme(legend.position = "none") +
  labs(y = "Estimated Active Cases Among Incarcerated People") +
  ggtitle("Monitoring Facility Outbreaks at Safford State Prison")

ggsave( "safford-active.png", plt1, width = 14, height = 10)
ggsave( "safford-active.svg", plt1, width = 14, height = 10)

plt2 <- plot_fac_trend("SAFFORD STATE PRISON", 
                      state = "Arizona", 
                      metric = "Residents.Confirmed", 
                      scrape_df = scrape_az, 
                      plot_days = 60, 
                      annotate = T, 
                      area_plot = T)  +
  labs(title = "Cumulative COVID-19 Cases at Safford State Prison",
       y = "Reported Cases Among Incarcerated People")
ggsave("safford-cumulative.png", plt2, width = 14, height = 10)
ggsave("safford-cumulative.svg", plt2, width = 14, height = 10)

out2 <- scrape_az %>% 
  filter(Name == "BELLAMY CREEK CORRECTIONAL FACILITY") %>% 
  mutate(Staff.Act.Est = diff_roll_sum(Staff.Confirmed, Date)) %>% 
  filter(!is.na(Date)) %>% 
  filter(Date > "2021-01-01") %>% 
  ggplot(aes(x = Date)) + 
  geom_line(aes(y = Residents.Active), color = "#D7790F", size = 1.5) +
  geom_line(aes(y = Staff.Act.Est), color = "#4C6788", size = 1.5) +
  theme_behindbars() + 
  labs(y = "Estimated Active Cases", 
       title = "Comparison of outbreaks beween staff and incarcerated people", 
       subtitle = "Bellamy Creek Correctional Facility, Michigan")

ggsave("bellamy_staff_comparison.png", out2, width = 14, height = 10)

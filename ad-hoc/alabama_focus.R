library(behindbarstools)
library(tidyverse)
library(skimr)
library(plotly)
library(lubridate)

al <- read_scrape_data(all_dates = TRUE, state = "Alabama") 
skim(al) ## not much testing data to speak of !

al <- al %>%
  arrange(Facility.ID, Date) %>% 
  group_by(Facility.ID) %>% 
  mutate(Res.Act.Est = diff_roll_sum(Residents.Confirmed, Date)) %>% 
  fill(Residents.Population) %>% 
  ungroup() %>%
  filter(Jurisdiction == "state") %>%
  mutate(active_rate = Res.Act.Est / Residents.Population) %>%
  mutate(death_rate = Residents.Deaths / Residents.Population,
         test_rate = Residents.Tested / Residents.Population) 

## Plot all facilities together 
facs <- al %>%
  ggplot( aes(x=Date, y=Res.Act.Est, group=Name, color=Name)) +
  geom_line() +
  # scale_color_viridis(discrete = TRUE) +
  # ggtitle("Popularity of American names in the previous 30 years") +
  # theme_ipsum() +
  ylab("Estimated Active Cases") + 
  # scale_color_bbdiscrete() + 
  theme_minimal() + 
  theme(legend.position = "none")
ggplotly(facs)

## BIBB correctional facility - multiple outbreaks
bibb_plot <- al %>%
  filter(Facility.ID == 5) %>% 
  ggplot( aes(x=Date, 
              y=active_rate)) +
  geom_line(size = 1.5, color = "#D7790F") +
  geom_area(alpha = .5, fill = "#D7790F") +
  ylab("Estimated Active Case Rate") + 
  scale_color_bbdiscrete() +
  # scale_fill_manual(values = "#D7790F") +
  scale_y_continuous(labels = scales::percent) + 
  theme_behindbars() +
  theme(legend.position = "none") +
  ggtitle("Multiple COVID-19 Outbreaks in Bibb Correctional Facility") 
ggsave("bibb_historical.png", bibb_plot, width = 14, height = 10)
ggplotly(bibb_plot)

## tutwiler
tutwiler_plot <- al %>%
  filter(Facility.ID == 39) %>% 
  ggplot( aes(x=Date, 
              y=active_rate)) +
  geom_line(size = 1.5, color = "#D7790F") +
  geom_area(alpha = .5, fill = "#D7790F") +
  ylab("Estimated Active Case Rate") + 
  scale_color_bbdiscrete() +
  # scale_fill_manual(values = "#D7790F") +
  scale_y_continuous(labels = scales::percent) + 
  theme_behindbars() +
  theme(legend.position = "none") +
  ggtitle("Ongoing COVID-19 Outbreaks in Tutwiler Prison For Women") 
ggsave("tutwiler_historical.png", tutwiler_plot, width = 14, height = 10)


## Check out prisons with the worst death rates
al %>% 
  arrange(desc(death_rate)) %>% 
  relocate(death_rate, Residents.Deaths) %>% 
  View()

hamilton <- al %>%
  filter(Facility.ID == 18) %>% 
  ggplot( aes(x=Date, 
              y=Residents.Deaths)) +
  geom_line(size = 1.5, color = "#D7790F") +
  ylab("COVID-19 Deaths") + 
  scale_color_bbdiscrete() +
  # scale_fill_manual(values = "#D7790F") +
  theme_behindbars() +
  scale_y_continuous(limits = c(0, 17)) + 
  theme(legend.position = "none") +
  ggtitle("Deaths of Incarcerated Individuals in \nHamilton Aged and Infirmed Facility") 
ggsave("hamilton_deaths.png", hamilton, width = 14, height = 10)

al %>%
  filter(Facility.ID == 18) %>%
  select(Date, Residents.Deaths) %>%
  mutate(Month = floor_date(Date, unit = "month")) %>%
  group_by(Month) %>%
  summarise(Residents.Deaths = first(max(Residents.Deaths, na.rm= TRUE))) %>%
  mutate(Deaths = Residents.Deaths - lag(Residents.Deaths, default = 0)) %>%
  mutate(DeathTxt = ifelse(Deaths == 0, NA, Deaths)) %>%
  ggplot(aes(x=Month, y=Deaths, label = DeathTxt)) +
  geom_col(color = "#D7790F", fill = "#D7790F") +
  geom_text(nudge_y = .75, size = 8) +
  ylab("COVID-19 Deaths\nPer Month") + 
  ggtitle("Deaths of Incarcerated Individuals in \nHamilton Aged and Infirmed Facility")


st_clair <- al %>%
  filter(Name == "ST CLAIR CORRECTIONAL FACILITY") %>% 
  ggplot( aes(x=Date, 
              y=Residents.Deaths)) +
  geom_line(size = 1.5, color = "#D7790F") +
  ylab("COVID-19 Deaths") + 
  scale_color_bbdiscrete() +
  # scale_fill_manual(values = "#D7790F") +
  theme_behindbars() +
  scale_y_continuous(limits = c(0, 17)) + 
  theme(legend.position = "none") +
  ggtitle("Deaths of Incarcerated Individuals in \nSt. Clair Correctional Facility") 
ggsave("stclair_deaths.png", st_clair, width = 14, height = 10)


st_clair_monthly <- al %>%
  filter(Name == "ST CLAIR CORRECTIONAL FACILITY") %>% 
  select(Date, Residents.Deaths) %>%
  mutate(Month = floor_date(Date, unit = "month")) %>%
  group_by(Month) %>%
  summarise(Residents.Deaths = first(max(Residents.Deaths, na.rm= TRUE))) %>%
  mutate(Deaths = Residents.Deaths - lag(Residents.Deaths, default = 0)) %>%
  mutate(DeathTxt = ifelse(Deaths == 0, NA, Deaths)) %>%
  ggplot(aes(x=Month, y=Deaths, label = DeathTxt)) +
  geom_col(color = "#D7790F", fill = "#D7790F") +
  geom_text(nudge_y = .25, size = 8) +
  ylab("COVID-19 Deaths\nPer Month") + 
  theme_behindbars() +
  ggtitle("New Monthly Deaths of Incarcerated Individuals\nat St. Clair Correctional Facility")
ggsave("stclair_monthlydeaths.png", st_clair_monthly, width = 14, height = 10)



library(tidyverse)
library(behindbarstools)
library(here)

latest <- read_scrape_data()

statewide <- read_csv("https://raw.githubusercontent.com/uclalawcovid19behindbars/data/master/latest-data/state_aggregate_counts.csv")

base_size <- 18
base_family <- "Helvetica"
base_color <- "#555526"

## state-aggregate
statewide_scatter <- statewide %>% 
  mutate(state_abbrev = translate_state(State, reverse = TRUE),
         vax_pct = Residents.Initiated / Residents.Population, 
         case_pct = Residents.Confirmed / Residents.Population) %>% 
  filter(!is.na(vax_pct) & !is.na(case_pct)) %>% 
  ggplot(aes(x = vax_pct, 
             y = case_pct,
             label = state_abbrev)) +
  geom_point(aes(
                 size = Residents.Population),
             alpha = 1/3) + 
  scale_x_continuous(label = scales::percent, limits = c(0, 1)) + 
  scale_y_continuous(label = scales::percent, limits = c(0, 1)) + 
  theme_classic(base_family = base_family, base_size = base_size) + 
  geom_text(check_overlap = TRUE, nudge_y = 0.015, size = 4) + 
  theme(legend.position = "none")
ggsave(filename = here("statewide-vax-scatter.png"), plot = statewide_scatter)

## facility-level 
## MN 
input_state <- "California"
vax_scatter <- latest %>% 
  filter(State == input_state) %>%
  filter(Jurisdiction == "state") %>% 
  mutate(vax_pct = Residents.Initiated / Residents.Population, 
         case_pct = Residents.Confirmed / Residents.Population) %>% 
  filter(!is.na(vax_pct) & !is.na(case_pct)) %>% 
  ggplot(aes(x = vax_pct, 
             y = case_pct,
             label = Name)) +
  geom_point(aes(size = Residents.Population),
             alpha = 1/3) + 
  scale_x_continuous(label = scales::percent, limits = c(0, 1)) + 
  scale_y_continuous(label = scales::percent, limits = c(0, 2)) + 
  theme_classic(base_family = base_family, base_size = base_size) + 
  labs(title = input_state)
ggsave(filename = here("ca-vax-scatter.png"), plot = vax_scatter)
ggsave(filename = here("mn-vax-scatter.png"), plot = vax_scatter)



  # geom_text(check_overlap = TRUE, nudge_y = 0.015, size = 5)

  # theme(
  #   text =                element_text(color = base_color),
  #   strip.text =          element_text(color = base_color),
  #   axis.text =           element_text(color = base_color),
  #   panel.grid.major.x =  element_line(color = "#92926C", linetype = "dotted"),
  #   panel.grid.major.y =  element_line(color = "#92926C", linetype = "dotted"),
  #   plot.title.position = "plot",
  #   plot.tag.position =   "bottomright",
  #   axis.line.y =         element_blank(),
  #   axis.ticks.y =        element_blank(),
  #   axis.title.x =        element_blank(),
  #   axis.line =           element_line(color = base_color),
  #   axis.ticks =          element_line(color = base_color),
  #   plot.caption =        element_text(margin = margin(t = 1.2 * base_size)),
  #   plot.subtitle =       element_text(margin = margin(b = 1.2 * base_size)),
  #   axis.title.y =        element_blank()
  # ) 


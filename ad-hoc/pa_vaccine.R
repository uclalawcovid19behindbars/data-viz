library(tidyverse)
library(behindbarstools)
library(ggalt)

latest <- read_scrape_data()

pa_vaccine <- latest %>% 
    filter(State == "Pennsylvania") %>% 
    filter(Jurisdiction == "state") %>% 
    mutate(Name_clean = str_remove(Name, "CORRECTIONAL FACILITY")) %>% 
    mutate(Name_clean = str_remove(Name_clean, "CORRECTIONAL INSTITUTION")) %>% 
    mutate(Name_clean = str_remove(Name_clean, "CORRECTIONAL INSTIUTION")) %>% 
    mutate(Name_clean = stringr::str_to_title(Name_clean)) 

base_size <- 18
base_family <- "Helvetica"
base_color <- "#555526"

theme_custom <- theme(
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

p1 <- pa_vaccine %>% 
    mutate(res_pct = Residents.Initiated / Residents.Population, 
           staff_pct = Staff.Initiated / Staff.Population) %>% 
    filter(!is.na(res_pct) & !is.na(staff_pct)) %>% 
    ggplot() +
    geom_dumbbell(aes(x = staff_pct, 
                      xend = res_pct, 
                      y = reorder(Name_clean, -staff_pct)), 
                  size_x = 3, 
                  size_xend = 3,
                  colour = "#c1c4b9",
                  size = 1.0,
                  colour_x = "#D7790F", 
                  alpha = 0.8,
                  colour_xend = "#4C6788") + 
    scale_x_continuous(label = scales::percent, limits = c(0, 1)) + 
    theme_classic(base_family = base_family, base_size = base_size) + 
    theme_custom

p2 <- pa_vaccine %>% 
    mutate(pct_vaccinated = Staff.Initiated / Staff.Population, 
           pct_not = 1 - pct_vaccinated) %>% 
    select(Name_clean, pct_vaccinated, pct_not) %>% 
    filter(!is.na(pct_vaccinated)) %>% 
    pivot_longer(!Name_clean, names_to = "condition", values_to = "value") %>% 
    ggplot(aes(fill = condition, y = value, x = fct_rev(Name_clean))) + 
    geom_bar(position = "stack", stat = "identity", width = 0.5) + 
    coord_flip() + 
    scale_fill_manual(values = c("#cdcfbe", "#E7963C")) + 
    scale_y_continuous(label = scales::percent) + 
    theme_classic(base_family = base_family, base_size = base_size) + 
    theme_custom + 
    theme(legend.position = "none")
    
p3 <- pa_vaccine %>% 
    mutate(res_pct = Residents.Initiated / Residents.Population, 
           staff_pct = Staff.Initiated / Staff.Population) %>% 
    filter(!is.na(staff_pct)) %>% 
    ggplot(aes(x = fct_rev(Name_clean), y = staff_pct, 
               label = percent(staff_pct, accuracy = 1))) + 
    geom_point(size = 3.0, color = "#D7790F") + 
    geom_segment(aes(x = reorder(Name_clean, -staff_pct), 
                     xend = reorder(Name_clean, -staff_pct), 
                     y = 0, 
                     yend = staff_pct), 
                 size = 1.0, color = "#D7790F") + 
    geom_text(size = 4, position = position_nudge(y = 0.06), color = base_color, family = base_family) +
    coord_flip() + 
    scale_y_continuous(label = scales::percent, limits = c(0, 1)) + 
    theme_classic(base_family = base_family, base_size = base_size) + 
    theme_custom + 
    theme(panel.grid.major.x = element_line(color = "#cdcfbe", linetype = "dotted"),
          panel.grid.major.y = element_line(color = "#cdcfbe", linetype = "dotted"))

ggsave("p1.svg", p1, width = 10, height = 12)
ggsave("p2.svg", p2, width = 10, height = 12)
ggsave("p3.svg", p3, width = 10, height = 12)

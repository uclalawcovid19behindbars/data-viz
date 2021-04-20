library(tidyverse)
library(behindbarstools)
library(ggalt)

latest <- read_scrape_data()

base_size <- 18
base_family <- "Helvetica"
base_color <- "#555526"

p <- latest %>% 
    filter(State == "California") %>% 
    filter(Jurisdiction == "state") %>% 
    mutate(staff_pct = Staff.Initiated / Staff.Population, 
           res_pct = Residents.Initiated / Residents.Population) %>% 
    filter(!is.na(res_pct) & !is.na(staff_pct)) %>% 
    filter(staff_pct < 0.44) %>% 
    ggplot() +
    geom_dumbbell(aes(x = staff_pct, 
                      xend = res_pct, 
                      y = reorder(stringr::str_to_title(Name), -staff_pct)), 
                  size_x = 3, 
                  size_xend = 3,
                  colour = "#c1c4b9",
                  size = 1.0,
                  colour_x = "#D7790F", 
                  alpha = 0.8,
                  colour_xend = "#4C6788") + 
    scale_x_continuous(label = scales::percent, limits = c(0, 1)) + 
    theme_classic(base_family = base_family, base_size = base_size) + 
    theme(
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

ggsave("vax_rates.svg", width = 8, height = 7)    

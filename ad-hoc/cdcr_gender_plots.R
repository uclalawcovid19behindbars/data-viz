library(tidyverse)
library(behindbarstools)

all_dates <- read_scrape_data(all_dates = TRUE)
latest <- read_scrape_data()

staff_vax <- latest %>% 
    filter(Jurisdiction == "state") %>% 
    filter(State == "California") %>% 
    filter(!is.na(Residents.Confirmed)) %>% 
    mutate(Gender = ifelse(Gender == "Female", "Female", "Mixed")) %>% 
    arrange(desc(Gender)) %>% 
    ggplot(aes(x = Staff.Population, y = Staff.Initiated)) +
    geom_abline(slope = 0.2, linetype = "dashed", color = "#5c5859") + 
    geom_abline(slope = 0.4, linetype = "dashed", color = "#5c5859") + 
    geom_abline(slope = 0.6, linetype = "dashed", color = "#5c5859") + 
    geom_point(aes(color = Gender, fill = Gender), 
               shape = 21, size = 4, color = "white") +
    theme_behindbars(base_color = "black", base_size = 18) + 
    scale_x_continuous(label = scales::comma, limits = c(0, 4000)) + 
    scale_y_continuous(label = scales::comma, limits = c(0, 2500), 
                       sec.axis = dup_axis(
                           breaks = c(850, 1700, 2500), 
                           labels = c("20% vaccinated", "40% vaccianted", "60% vaccinated"), 
                           name = element_blank()
                        )) + 
    scale_fill_bbdiscrete() +
    labs(x = "Staff Population", 
         y = "Staff With 1+ Vaccination Dose", 
         title = "Staff Vaccination Rates Across California State Prisons") + 
    theme(legend.position = "none", 
          panel.grid.major.y = element_blank(), 
          axis.line.y = element_line(), 
          axis.ticks.y = element_line(), 
          axis.ticks.y.right = element_blank(), 
          axis.line.y.right = element_blank(), 
          axis.title.x = element_text(margin = margin(t = 1.2 * 18)),
          axis.text.y.right = element_text(color = "#5c5859"))

ggsave("ca_staff_vax.png", staff_vax, width = 9, height = 5)

ccwf <- all_dates %>% 
    filter(Name == "CENTRAL CALIFORNIA WOMENS FACILITY") %>% 
    ggplot(aes(x = Date, y = Residents.Population)) + 
    geom_line(size = 1.0, color = "#4C6788") + 
    geom_hline(yintercept = 2004, linetype = "dashed", size = 1.0, color = "#D7790F") +
    scale_y_continuous(label = scales::comma, limits = c(0, 3000), 
                       sec.axis = dup_axis(
                           breaks = c(2004), 
                           labels = c("Prison Capacity"), 
                           name = element_blank()
                       )) + 
    theme_behindbars(base_color = "black", base_size = 18) +
    theme(panel.grid.major.y = element_blank()) +
    labs(title = "Total Incarcerated Population",
         subtitle = "Central California Women's Facility (Chowchilla)", 
         y = "Incarcerated Population")

ggsave("ciw.png", ciw, width = 9, height = 5)
ggsave("ccwf.png", ccwf, width = 9, height = 5)


library(tidyverse)
library(behindbarstools)
library(glue)

######## GDC (statewide) death metrics

# deaths by suicide and homicide -------------------------------------------------------
sw_deaths <- tibble(
    Year = c(2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021),
    Suicides = c(5, 7, 8, 19, 17, 16, 24, 14),
    Homicides = c(5, 2, 5, 8, 9, 8, 29, 18),
    ) %>%
    pivot_longer(!Year, names_to = "variable", values_to = "count") %>%
    group_by(Year) %>%
    mutate(label_y = cumsum(count)) %>%
    ungroup() %>%
    mutate(Date = as.Date(glue("{Year}-01-01"))) %>%
    mutate(partial_dat_alpha = ifelse(Year == 2021, .6, 1))

#PLOT 
statewide_homicides_suicides <- sw_deaths %>%
    ggplot(aes(
        x = Date, y = count)) +
    geom_col(aes(color = variable, fill = variable, alpha = partial_dat_alpha), 
             position = position_stack()) +
    theme_behindbars() +
    scale_color_bbdiscrete() +
    scale_fill_bbdiscrete() +
    theme(
        axis.ticks.y = element_line(color = "#555526"), 
        axis.title.y = element_blank(), 
        axis.line.y = element_line(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_blank()) + 
    scale_x_date(date_breaks = "1 year", date_labels =  "%Y") +
    labs(y = "N deaths",
         title = "Deaths in Georgia State Prisons",
         tag = "G") + 
    geom_text(aes(y = label_y, label = count), size = 6, vjust = 1.5) 
    
ggsave("~/Desktop/ga_viz/statewide_homicides_suicides.png", statewide_homicides_suicides, width = 10, height = 8)
ggsave("~/Desktop/ga_viz/statewide_homicides_suicides.svg", statewide_homicides_suicides, width = 7, height = 5)

# staff vacancy rates -----------------------------------------------------



# deaths by homicide ------------------------------------------------------


######## FACILITY_LEVEL 
# staff vacancy rates -----------------------------------------------------
fac_staff_vac_dat <- tibble(
    facility = c("Smith SP", "Telfair SP", "Georgia SP", "Lee Arrendale SP", "Ware SP",
                 "Calhoun SP", "Rogers SP", "Wilcox SP", "Autry SP", "Dodge SP"),
    vac_rate = c(.74, .727, .703, .685, .636, .706, .697, .685, .636, .625),
    security_lvl = c(rep("close", 5), rep("medium", 5))
)

# deaths by suicide -------------------------------------------------------
fac_suicides <- tibble(
    facility = c("Lee Arrendale SP", "Augusta SMP", "Autry SP", "Baldwin SP",
                 "Central SP", "Georgia Diagnostic and Classification Prison",
                 "SMU", "Georgia SP", "Jefferson County CI", "Johnson SP",
                 "Lee SP", "Macon SP", "Phillips SP", "Pulaski SP", "Rogers SP",
                 "Rutledge SP", "Smith SP", "Smith Transitional Center", "Terrell County CI",
                 "Valdosta SP", "Wheeler SP", "Wilcox SP"),
    n_suicides = c(1, 1, 1, 1, 1, 3, 1, 10, 1, 2, 1, 1, 1, 1, 2, 1, 2, 1, 1, 3, 1, 1)
)

# deaths by homicide ------------------------------------------------------

fac_homicides <- tibble(
    facility = c("Augusta SMP", "Baldwin SP",
                 "Central SP", "Coffee CF", "Georgia Diagnostic and Classification Prison",
                 "Hancock SP", "Hays SP", "Johnson SP",
                 "Macon SP", 
                 "Rutledge SP", "Smith SP", "Telfair SP", 
                 "Valdosta SP", "Wilcox SP"),
    n_homicides = c(3, 4, 1, 1, 4, 3, 2, 1, 9, 1, 7, 6, 3, 3)
)

# combined data ------------------------------------------------------
fac_hom_suicides <- fac_suicides %>%
    full_join(fac_homicides, by = "facility") %>%
    full_join(fac_staff_vac_dat, by = "facility") %>%
    mutate(n_homicide_plus_suicide = n_suicides + n_homicides) ## na rm treatment here?

#PLOT 
fac_staff_vac_plot <- fac_hom_suicides %>%
    filter(!is.na(vac_rate)) %>%
    ggplot(aes(
        x = reorder(facility, vac_rate), y = vac_rate)) +
    coord_flip() + 
    geom_col(aes(color = security_lvl, fill = security_lvl)) +
    theme_behindbars() +
    scale_color_bbdiscrete() +
    scale_fill_bbdiscrete() +
    theme(
        axis.ticks.y = element_line(color = "#555526"), 
        axis.title.y = element_blank(), 
        axis.line.y = element_line(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_blank()) + 
    labs(title = "Staff Vacancy Rates in Georgia State Prisons",
         tag = "H") + 
    scale_y_continuous(labels = scales::percent, limits = c(0, 1)) + 
    geom_text(aes(label = (round(vac_rate*100, 0))), 
                  size = 6, hjust = 1)# + 
    # geom_text(aes(label = glue("Suicides: {n_suicides}, Homicides: {n_suicides}")))

ggsave("~/Desktop/ga_viz/fac_staff_vac_plot.png", fac_staff_vac_plot, width = 15, height = 8)
ggsave("~/Desktop/ga_viz/fac_staff_vac_plot.svg", fac_staff_vac_plot, width = 7, height = 5)



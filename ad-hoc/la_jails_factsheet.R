library(tidyverse)
library(behindbarstools)

raw_dat <- behindbarstools::read_scrape_data(all_dates = TRUE, state = "California")

laj <- raw_dat %>%
    filter(Name == "LOS ANGELES JAILS")

fac_data <- behindbarstools::read_fac_info()
laj_info <- fac_data %>%
    filter(Name == "LOS ANGELES JAILS")

## plot of population over time
laj_pop <- laj %>% 
    filter(!is.na(Residents.Population),
           Date != "2021-08-31",
           Date != "2021-04-09",
           Date != "2021-04-11") %>%
    ggplot() + 
    geom_line(aes(x = Date, y = Residents.Population), size = 1.0, color = "#D7790F") + 
    theme_behindbars() + 
    scale_y_continuous(limits = c(12000, 15500), label = scales::comma) +
    theme(
        axis.ticks.y = element_line(color = "#555526"), 
        axis.title.y = element_blank(), 
        axis.line.y = element_line(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_blank()) + 
    labs(title = "Total Incarcerated Population",
         subtitle = "Los Angeles Jails") + 
    ylab("Incarcerated population") + 
    ## capacity source: 
    ## http://www.la-sheriff.org/s2/static_content/info/documents/Custody%20Division%20Population%202019%20Third%20Quarter%20Report.pdf
    geom_hline(yintercept = 12404, size = 2.0, color = "#4C6788") + 
    scale_x_date(date_breaks = "2 month", date_labels =  "%b %y") 

ggsave("laj_pop.png", laj_pop, width = 9, height = 5)
ggsave("laj_pop.svg", laj_pop, width = 10, height = 8)

## tests administered 
laj_tadmin <- laj %>% 
    filter(!is.na(Residents.Tadmin),
           Date != "2021-06-02",
           Date != "2021-08-27",
           Date != "2021-04-09",
           Date != "2021-04-11",
           Date != "2021-03-19",
           Date != "2021-04-14") %>%
    mutate(new_tests_over_cases =  diff_roll_sum(Residents.Confirmed) / diff_roll_sum(Residents.Tadmin)) %>%
    ggplot() + 
    geom_line(aes(x = Date, y = new_tests_over_cases), size = 1.0, color = "#D7790F") + 
    theme_behindbars() + 
    theme(
        axis.ticks.y = element_line(color = "#555526"), 
        axis.title.y = element_blank(), 
        axis.line.y = element_line(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_blank()) + 
    # labs(title = "Staff Cumulative Cases",
    #      subtitle = "Los Angeles Jails") + 
    scale_x_date(date_breaks = "2 month", date_labels =  "%b %y") 
ggsave("laj_tadmin.png", laj_staff, width = 9, height = 5)

## table of monthly population numbers
laj %>% 
    filter(Date != "2021-08-31",
           Date != "2021-04-09",
           Date != "2021-04-11") %>%
    filter(!(is.na(Residents.Confirmed) & (is.na(Residents.Deaths)) & (is.na(Residents.Population)))) %>%
    select(Date, 
           `Cumulative Cases Among Incarcerated Individuals` = Residents.Confirmed,
           `Deaths Among Incarcerated Individuals` = Residents.Deaths,
           `Tests Adminstered to Incarcerated Individuals` = Residents.Tadmin, 
           `Total Incarcerated Population` =Residents.Population) %>%
    write_csv("~/Downloads/laj_dat.csv", na = "")

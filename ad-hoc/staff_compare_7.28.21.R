library(tidyverse)
library(behindbarstools)

# Overall state denominators 
census_pop <- "https://www2.census.gov/programs-surveys/popest/datasets/2010-2020/state/totals/nst-est2020.csv" %>% 
    read_csv(col_types = cols()) %>% 
    select(state = NAME, Population = POPESTIMATE2019)

# Overall new case rates (from NYT)
nyt_covid <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv" %>%
    read_csv(col_types = cols()) %>%
    rename(Date = date) %>%
    group_by(state) %>% 
    mutate(New.Cases = diff_roll_sum(cases, Date)) %>% 
    filter(Date == max(.$Date)) %>% 
    left_join(census_pop, by = "state") %>% 
    mutate(NCR = New.Cases / Population * 10000) %>% 
    rename(NYT.Date = Date)

# Latest state agg 
agg_df <- "https://raw.githubusercontent.com/uclalawcovid19behindbars/data/master/latest-data/latest_state_counts.csv" %>% 
    read_csv(col_types = cols())

active_df <- agg_df %>% 
    mutate(Staff.Active.Pct = Staff.Active / Staff.Population * 10000, 
           Res.Active.Pct = Residents.Active / Residents.Population * 10000) %>% 
    select(Staff.Active.Pct, Res.Active.Pct, State, Staff.Active, Residents.Active) 

# Historical state agg 
diff_agg_df <- calc_aggregate_counts(all_dates = TRUE, state = TRUE)

diff_roll_df <- diff_agg_df %>% 
    filter(Measure %in% c("Residents.Confirmed", "Staff.Confirmed")) %>% 
    group_by(State, Measure) %>% 
    filter(!is.na(Val)) %>% 
    mutate(New.Cases = diff_roll_sum(Val, Date)) %>% 
    filter(Date == max(.$Date)) %>% 
    select(State, Date, Measure, Val, Pop.Anchor, New.Cases) %>% 
    pivot_wider(names_from = "Measure", values_from = c("Val", "Pop.Anchor", "New.Cases")) %>% 
    mutate(Staff.NCR = New.Cases_Staff.Confirmed / Pop.Anchor_Staff.Confirmed * 10000, 
           Res.NCR = New.Cases_Staff.Confirmed / Pop.Anchor_Residents.Confirmed * 10000) %>% 
    left_join(nyt_covid, by = c("State" = "state")) %>%
    left_join(active_df, by = "State") %>% 
    select(State, NCR, Staff.NCR, Res.NCR, Staff.Active.Pct, Res.Active.Pct, Staff.Active, Residents.Active)
    
# Make plot 
base_size <- 20
base_family <- "Helvetica"
base_color <- "#555526"

dumbell_theme <- theme_classic(base_size = base_size) + 
    theme(
        text =                element_text(color = base_color),
        strip.text =          element_text(color = base_color),
        axis.text =           element_text(color = base_color),
        panel.grid.major.x =  element_line(color = "#cfcfbe", linetype = "dotted"),
        panel.grid.major.y =  element_line(color = "#cfcfbe", linetype = "dotted"),
        plot.title.position = "plot",
        plot.tag.position =   "bottomright",
        axis.line.y =         element_blank(),
        axis.ticks.y =        element_blank(),
        axis.line =           element_line(color = base_color),
        axis.ticks =          element_line(color = base_color),
        plot.caption =        element_text(margin = margin(t = 1.2 * base_size)),
        plot.subtitle =       element_text(margin = margin(b = 1.2 * base_size)),
        axis.title.y =        element_blank())

p <- diff_roll_df %>% 
    filter(State %in% c(
        "Pennsylvania", "Illinois", "Indiana", "California", "South Carolina", "Texas")) %>% 
    mutate(label_text = str_c(round(Staff.Active.Pct / NCR, 1), "x"), 
           label_position = Staff.Active.Pct + 5) %>%  
    ggplot() +
    ggalt::geom_dumbbell(
        aes(x = NCR, 
            xend = Staff.Active.Pct, 
            y = reorder(stringr::str_to_title(State), -NCR)), 
        size_x = 3, 
        size_xend = 3,
        colour = "#C1C4b9",
        size = 1.0,
        colour_x = "#D7790F", 
        alpha = 0.8,
        colour_xend = "#4C6788") + 
    geom_text(
        aes(x = label_position, 
            y = reorder(stringr::str_to_title(State), -NCR), 
            label = label_text), 
        color = "#4C6788", 
        size = 6) + 
    scale_x_continuous(limits = c(0, 85))  + 
    dumbell_theme + 
    labs(x = "New cases per 10k", 
         title = "Overall population (orange) and prison staff (blue)")

ggsave("staff_comparison.svg", p, width = 9, height = 4)

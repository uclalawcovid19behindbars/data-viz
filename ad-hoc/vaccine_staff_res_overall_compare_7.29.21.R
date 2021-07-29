library(behindbarstools)
library(tidyverse)
library(jsonlite)

# Plots for vaccine op-ed

# ------------------------------------------------------------------------------
# overall: vaccination rates for staff/incarcerated people/overall pop
# ------------------------------------------------------------------------------

# Latest state agg 
agg_df <- "https://raw.githubusercontent.com/uclalawcovid19behindbars/data/master/latest-data/latest_state_counts.csv" %>% 
    read_csv(col_types = cols())

vaccination_df <- agg_df %>% 
    mutate(Staff.Initiated.Pct = Staff.Initiated / Staff.Population, 
           Res.Initiated.Pct = Residents.Initiated / Residents.Population) %>% 
    select(State, Staff.Initiated.Pct, Res.Initiated.Pct, Staff.Initiated, Residents.Initiated) 

# Overall state-level vaccine numbers 
raw_cdc <- str_c(
    "https://covid.cdc.gov/covid-data-tracker/COVIDData/", 
    "getAjaxData?id=vaccination_data") %>% 
    jsonlite::read_json(simplifyVector = TRUE) 

cdc_joined <- as_tibble(raw_cdc$vaccination_data) %>% 
    mutate(pct_overall = Administered_Dose1_Recip_18PlusPop_Pct / 100)  %>% 
    select(LongName, pct_overall) %>% 
    full_join(vaccination_df, by = c("LongName" = "State")) %>% 
    mutate(staff_diff = Staff.Initiated.Pct - pct_overall)

vax_dat_out <- cdc_joined %>%
    filter(!is.na(pct_overall) & !is.na(Staff.Initiated.Pct) & !is.na(Res.Initiated.Pct)) %>%
    select(LongName, pct_overall, Staff.Initiated.Pct, Res.Initiated.Pct)

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

cdc_joined %>% 
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

ggplot() +
    geom_segment(
        data = gather(vax_dat_out, measure, val, -LongName) %>% 
            group_by(LongName) %>% 
            top_n(-1) %>% 
            slice(1) %>%
            ungroup(),
        aes(x = 0, xend = val, y = LongName, yend = LongName),
        linetype = "dotted", size = 0.5, color = "gray80"
    ) +
    geom_segment(
        data = gather(vax_dat_out, measure, val, -LongName) %>% 
            group_by(LongName) %>% 
            summarise(start = range(val)[1], end = range(val)[2]) %>% 
            ungroup(),
        aes(x = start, xend = end, y = LongName, yend = LongName),
        color = "gray80", size = 2
    ) +
    geom_point(
        data = gather(vax_dat_out, measure, value, -LongName),
        aes(value, LongName, color = measure), 
        size = 4
    ) +
    labs(
        x = "", y = "",
        title = "Vaccination Rate Comparison"
    ) + 
    scale_x_continuous(labels = scales::percent) + 
    dumbell_theme #+ 
    # scale_fill_discrete(labels = c("Overall population", 
    #                                "Incarcerated individuals",
    #                                "Prison staff members"))

# ggsave("vaccine_comparison.svg", p, width = 9, height = 4)

# ------------------------------------------------------------------------------
# overall: active case rates for staff/incarcerated people/overall pop
# ------------------------------------------------------------------------------


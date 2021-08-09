rm(list=ls())

library(tidyverse)
library(behindbarstools)
library(scales)

# ------------------------------------------------------------------------------
# LOAD AND PROCESS DATA 
# ------------------------------------------------------------------------------

# Read general population vaccine data 
genstate_vax_df <- get_genstate_vax()

# Read general population COVID data 
genstate_covid_raw_df <- get_genstate_covid()

genstate_covid_df <- genstate_covid_raw_df %>% 
    group_by(State) %>% 
    mutate(General.Active = diff_roll_sum(General.Confirmed, Date)) %>% 
    filter(Date == max(.$Date))

# Read state-aggregated COVID data 
latest_state_df <- calc_aggregate_counts(state = TRUE, all_dates = FALSE) 
aggregate_pop <- read_aggregate_pop_data() 

COVID_SUFFIXES <- c(
    ".Confirmed", ".Deaths", ".Tadmin", ".Tested", ".Active",
    ".Initiated", ".Completed", ".Vadmin"
)

covid_df <- latest_state_df %>%
    filter(!is.na(Val)) %>%
    select(State, Measure, Val) %>%
    pivot_wider(names_from = "Measure", values_from = "Val") %>%
    select(State, ends_with(COVID_SUFFIXES)) %>% 
    left_join(aggregate_pop, by = "State") 

# Alt: read from latest csv since barstools is slow 
# covid_df <- str_c("https://raw.githubusercontent.com/uclalawcovid19behindbars/", 
#                   "data/master/latest-data/latest_state_counts.csv") %>% 
#     read_csv(col_types = cols())

# Read state-aggregated vaccine data with media sources 
vaccine_df <- "1tc3p3FHG0tCHnlfb8YAABno_oh9JSNL4MNfhn_clyX0" %>% 
    googlesheets4::read_sheet(sheet = "Full Data_August")

# Join all datasets 
joined_df <- genstate_vax_df %>% 
    full_join(genstate_covid_df, by = "State") %>%
    full_join(covid_df, by = "State") %>% 
    full_join(vaccine_df, by = "State") %>% 
    mutate(Staff.Initiated = coalesce(Staff.Initiated, (Staff.Pct * Staff.Population)), 
           Residents.Initiated = coalesce(Residents.Initiated, (Res.Pct * Residents.Population))) %>% 
    select(-starts_with("Date"), -ends_with("Source"), -ends_with("Pop"), -ends_with("Vaccine"), -Notes) 

# ------------------------------------------------------------------------------
# SUMMARY NUMBERS  
# ------------------------------------------------------------------------------

# Estimated national staff vaccination rate    
joined_df %>% 
    filter(!is.na(Staff.Initiated) & !is.na(Staff.Population)) %>% 
    summarise(sum(Staff.Initiated) / sum(Staff.Population))

# Estimated national incarcerated vaccination rate
joined_df %>% 
    filter(!is.na(Residents.Initiated) & !is.na(Residents.Population)) %>% 
    summarise(sum(Residents.Initiated) / sum(Residents.Population))

# Estimated general population adult vaccination 
joined_df %>% 
    filter(State == "National") %>% 
    summarise(General.Initiated / General.Adult.Population)

# ------------------------------------------------------------------------------
# STAFF / OVERALL DUMBBELLS 
# ------------------------------------------------------------------------------
    
staff_bars <- joined_df %>% 
    mutate(General.Pct = General.Initiated / General.Adult.Population) %>% 
    filter(!is.na(Staff.Pct) & !is.na(General.Pct)) %>%
    mutate(staff_label = Staff.Pct - 0.05) %>% 
    mutate(overall_label = General.Pct + 0.05) %>% 
    rowwise() %>% 
    mutate(sort_ = min(General.Pct, Staff.Pct)) %>% 
    ggplot() +
    ggalt::geom_dumbbell(aes(x = Staff.Pct, 
                      xend = General.Pct, 
                      y = reorder(State, -sort_)), 
                  size_x = 2, 
                  size_xend = 2,
                  colour = "#C1C4B9",
                  size = 1.0,
                  colour_x = "#D7790F", 
                  alpha = 0.8,
                  colour_xend = "#4C6788") + 
    geom_text(aes(x = staff_label, 
                  y = reorder(State, -sort_), 
                  label = percent(Staff.Pct, accuracy = 1)), 
              color = "#D7790F") + 
    geom_text(aes(x = overall_label, 
                  y = reorder(State, -Staff.Pct), 
                  label = percent(General.Pct, accuracy = 1)), 
              color = "#4C6788") + 
    scale_x_continuous(label = percent, limits = c(0, 1)) +
    theme_minimal(base_size = 15) + 
    theme(axis.title.x = element_blank(), 
          axis.title.y = element_blank(), 
          axis.text.x = element_blank(), 
          axis.text.y = element_text(color = "black"))

ggsave("staff_bars.svg", staff_bars, width = 8, height = 6)

# ------------------------------------------------------------------------------
# SIDE-BY-SIDE VACCINATION / ACTIVE BARS 
# ------------------------------------------------------------------------------

# Cases per 10k 
SCALE <- 10000

# Active cases national 
active_nat_df <- joined_df %>% 
    select(State, ends_with("Active"), ends_with("Population")) %>% 
    drop_na() %>% 
    summarise_if(is.numeric, sum) %>% 
    gather(key, value) %>%
    filter(!key == "General.Adult.Population") %>% 
    separate(key, into = c("Group", "Metric")) %>%
    spread(Metric, value) %>% 
    bind_cols(as_tibble(Hmisc::binconf(.$Active, .$Population))) %>%
    mutate(Active.Pct = PointEst * SCALE, 
           Active.Lower = Lower * SCALE, 
           Active.Upper = Upper * SCALE) %>% 
    select(-PointEst, -Lower, -Upper) %>% 
    rename(Active.Population = Population)

# Vaccination rates national 
vax_nat_df <- joined_df %>% 
    select(State, ends_with("Initiated"), ends_with("Population")) %>% 
    drop_na() %>% 
    summarise_if(is.numeric, sum) %>% 
    gather(key, value) %>%
    filter(!key == "General.Population") %>% 
    mutate(key = ifelse(key == "General.Adult.Population", "General.Population", key)) %>% 
    separate(key, into = c("Group", "Metric")) %>%
    spread(Metric, value) %>% 
    bind_cols(as_tibble(Hmisc::binconf(.$Initiated, .$Population))) %>%
    mutate(Initiated.Pct = PointEst, 
           Initiated.Lower = Lower, 
           Initiated.Upper = Upper) %>% 
    select(-PointEst, -Lower, -Upper) %>% 
    rename(Initiated.Population = Population)

joined_nat_df <- active_nat_df %>% 
    left_join(vax_nat_df, by = "Group") 

# Plots 
active <- joined_nat_df %>% 
    ggplot(aes(x = Group, y = Active.Pct, ymin = Active.Lower, ymax = Active.Upper)) + 
    geom_bar(stat = "identity", width = 0.5) + 
    geom_errorbar(width = 0.1) +
    theme_behindbars(base_size = 14, base_color = "black") + 
    labs(title = "New cases per 10k") + 
    scale_y_continuous(limits = c(0, 80)) +
    theme(axis.title.y = element_blank())

vax <- joined_nat_df %>% 
    ggplot(aes(x = Group, y = Initiated.Pct, ymin = Initiated.Lower, ymax = Initiated.Upper)) + 
    geom_bar(stat = "identity", width = 0.5) + 
    geom_errorbar(width = 0.1) + 
    theme_minimal() + 
    theme_behindbars(base_size = 14, base_color = "black") + 
    scale_y_continuous(labels = percent, limits = c(0, 1)) + 
    labs(title = "Vaccination rate") +
    theme(axis.title.y = element_blank())
        
joined_plot <- ggpubr::ggarrange(vax, active)

ggsave("joined_plot.svg", joined_plot, width = 7, height = 3)

# ------------------------------------------------------------------------------
# ACTIVE STAFF / OVERALL COMPARISON DUMBBELLS  
# ------------------------------------------------------------------------------

base_size <- 20
base_family <- "Helvetica"
base_color <- "black"

active_compare_plot <- joined_df %>% 
    filter(Staff.Active > 15) %>% 
    mutate(Staff.Active.Pct = Staff.Active / Staff.Population * SCALE, 
           Res.Active.Pct = Residents.Active / Residents.Population * SCALE, 
           General.Active.Pct = General.Active / General.Population * SCALE) %>% 
    select(State, ends_with("Active"), ends_with("Pct")) %>% 
    filter(Staff.Active.Pct / General.Active.Pct > 1.1) %>%
    drop_na() %>% 
    mutate(label_text = str_c(round(Staff.Active.Pct / General.Active.Pct, 1), "x"), 
           label_position = Staff.Active.Pct + 9) %>%  
    ggplot() +
    ggalt::geom_dumbbell(
        aes(x = General.Active.Pct, 
            xend = Staff.Active.Pct, 
            y = reorder(stringr::str_to_title(State), -General.Active.Pct)), 
        size_x = 3, 
        size_xend = 3,
        colour = "#C1C4b9",
        size = 1.0,
        colour_x = "#4C6788", 
        alpha = 0.8,
        colour_xend = "#D7790F") + 
    geom_text(
        aes(x = label_position, 
            y = reorder(stringr::str_to_title(State), -General.Active.Pct), 
            label = label_text), 
        color = "#D7790F", 
        size = 6) + 
    labs(x = "New cases per 10k") + 
    scale_x_continuous(limits = c(0, 150)) +
    theme_classic(base_size = base_size) + 
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

ggsave("active_compare.svg", active_compare_plot, width = 9, height = 3)

# ------------------------------------------------------------------------------
# WEEKLY ACTIVE CASES 
# ------------------------------------------------------------------------------

agg_df <- calc_aggregate_counts(all_dates = TRUE, state = TRUE)

active_est_df <- agg_df %>% 
    filter(Measure %in% c("Residents.Confirmed", "Staff.Confirmed")) %>% 
    select(State, Date, Measure, UCLA, MP) %>% 
    pivot_wider(names_from = Measure, values_from = c("UCLA", "MP")) %>% 
    group_by(State) %>% 
    mutate(UCLA.Res.Est = diff_roll_sum(UCLA_Residents.Confirmed, Date), 
           UCLA.Staff.Est = diff_roll_sum(UCLA_Staff.Confirmed, Date), 
           MP.Res.Est = diff_roll_sum(MP_Residents.Confirmed, Date), 
           MP.Staff.Est = diff_roll_sum(MP_Staff.Confirmed, Date)) %>% 
    left_join(
        agg_df %>% 
            filter(Measure %in% c("Residents.Active", "Staff.Active")) %>% 
            select(State, Date, Measure, UCLA) %>% 
            pivot_wider(names_from = "Measure", values_from = "UCLA"), 
        by = c("State", "Date")) %>% 
    mutate(Residents = coalesce(Residents.Active, MP.Res.Est, UCLA.Res.Est), 
           Staff = coalesce(Staff.Active, MP.Staff.Est, UCLA.Staff.Est)) %>% 
    select(State, Date, Residents, Staff) %>% 
    pivot_longer(cols = c("Residents", "Staff"), names_to = "Measure", values_to = "Val") %>% 
    filter(!is.na(Val)) 

keep_states <- active_est_df %>% 
    filter(Date == "2021-08-01") %>% 
    group_by(State, Date) %>% 
    filter(n() == 2) %>% 
    distinct(State) %>% 
    unlist()

plot_df <- active_est_df %>% 
    filter(State %in% keep_states) %>% 
    group_by(Date, Measure) %>% 
    mutate(Val = ifelse(Val < 0, 0, Val)) %>% 
    summarise(cases = sum_na_rm(Val)) %>% 
    filter(Date < "2021-08-07")

stacked_pct_plot <- plot_df %>% 
    ggplot(aes(x = Date, y = cases, fill = Measure)) + 
    geom_bar(stat = "identity", position = "fill") + 
    theme_behindbars(base_size = 16, base_color = "black") +
    scale_y_continuous(label = percent, breaks = pretty_breaks(n = 2)) +
    scale_x_date(label = date_format("%b '%y"), breaks = pretty_breaks(n = 10)) + 
    scale_fill_manual(values = c("#CDCFBE", "#D7790F")) + 
    labs(y = "Share of active cases") + 
    theme(legend.position = "none", 
          legend.title = element_blank())

stacked_plot <- plot_df %>% 
    ggplot(aes(x = Date, y = cases, fill = Measure)) + 
    geom_bar(stat = "identity") + 
    theme_behindbars(base_size = 16, base_color = "black") +
    scale_y_continuous(label = comma, breaks = pretty_breaks()) + 
    scale_x_date(label = date_format("%b '%y"), breaks = pretty_breaks(n = 10)) + 
    scale_fill_manual(values = c("#CDCFBE", "#D7790F")) + 
    labs(y = "Estimated active cases") + 
    theme(legend.position = "none", 
          legend.title = element_blank())

weekly_active <- ggpubr::ggarrange(stacked_plot, stacked_pct_plot, 
                                   heights = c(2, 1),
                                   ncol = 1, nrow = 2, align = "v")

ggsave("weekly_active.svg", weekly_active, width = 8, height = 6)

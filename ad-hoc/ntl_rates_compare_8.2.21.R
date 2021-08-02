library(behindbarstools)
library(tidyverse)
library(jsonlite)

'%!in%' <- function(x,y)!('%in%'(x,y))

## LOAD DATA
agg_df <- "https://raw.githubusercontent.com/uclalawcovid19behindbars/data/master/latest-data/latest_national_counts.csv" %>% 
    read_csv(col_types = cols())
state_df <- "https://raw.githubusercontent.com/uclalawcovid19behindbars/data/master/latest-data/latest_state_counts.csv" %>% 
    read_csv(col_types = cols())
anchored_denoms <- "https://raw.githubusercontent.com/uclalawcovid19behindbars/data/master/anchored-data/state_aggregate_denominators.csv" %>%
    read_csv(col_types = cols())
genpop_df <- "https://www2.census.gov/programs-surveys/popest/datasets/" %>%
    stringr::str_c("2010-2020/state/totals/nst-est2020.csv") %>%
    readr::read_csv(col_types = readr::cols()) %>%
    select(State = NAME, General.Population = POPESTIMATE2020)
state_infections <- "https://data.cdc.gov/api/views/9mfq-cb36/rows.csv" %>%
    stringr::str_c("?accessType=DOWNLOAD") %>%
    readr::read_csv(col_types = readr::cols())
gen_covid_df <- state_infections %>%
    rename(state_abv = state) %>%
    mutate(State = translate_state(state_abv)) %>%
    mutate(Date = lubridate::mdy(submission_date)) %>%
    filter(Date == max(Date)) %>%
    select(
        State, Date, 
        General.Confirmed = tot_cases,
        General.Death = tot_death,
        General.Active = new_case) %>%
    left_join(genpop_df, by = "State") 
raw_cdc_vax <- str_c(
    "https://covid.cdc.gov/covid-data-tracker/COVIDData/", 
    "getAjaxData?id=vaccination_data") %>% 
    jsonlite::read_json(simplifyVector = TRUE) 
gen_vax_df <- as_tibble(raw_cdc_vax$vaccination_data) %>% 
    mutate(General.Initiated = Administered_Dose1_Recip_18Plus)  %>% 
    mutate(State = translate_state(Location)) %>%
    mutate(Date = lubridate::ymd(Date)) %>%
    filter(Date == max(Date)) %>%
    left_join(genpop_df, by = "State") %>%
    select(State, Date, General.Initiated, General.Population) 

# ------------------------------------------------------------------------------
# national: vaccination rate for prison staff
# ------------------------------------------------------------------------------

missing_staff_vax_states <- agg_df %>%
    filter(Measure == "Staff.Initiated") %>%
    select(Missing) %>%
    str_split(", |\n ") %>%
    .[[1]]

staff_vax_num <- agg_df %>%
    filter(Measure == "Staff.Initiated") %>%
    pull(Count)

staff_vax_denom <- anchored_denoms %>%
    ## ! NB: missing denoms for DE, GA, NJ
    filter(State %!in% missing_staff_vax_states) %>%
    summarise(ntl_staff_estimate = sum_na_rm(Staff.Population)) %>%
    pull(ntl_staff_estimate)

prison_staff_vax_rate <- staff_vax_num / staff_vax_denom
print(paste0("Estimated staff vaccination rate: ", prison_staff_vax_rate))

# ------------------------------------------------------------------------------
# national: active case rate for prison staff
# ------------------------------------------------------------------------------

missing_staff_active_states <- state_df %>%
    filter(is.na(Staff.Active)) %>%
    pull(State)

staff_active_num <- state_df %>%
    filter(!is.na(Staff.Active)) %>%
    summarise(staff_active = sum(Staff.Active)) %>%
    pull(staff_active)

staff_active_denom <- anchored_denoms %>%
    ## NB: missing denoms for DE, LA
    filter(State %!in% missing_staff_active_states) %>%
    summarise(ntl_staff_estimate = sum_na_rm(Staff.Population)) %>%
    pull(ntl_staff_estimate)

prison_staff_active_rate <- staff_active_num / staff_active_denom
print(paste0("Estimated staff active case rate: ", prison_staff_active_rate))

# ------------------------------------------------------------------------------
# national: vaccination rate for incarcerated people
# ------------------------------------------------------------------------------

missing_residents_vax_states <- agg_df %>%
    filter(Measure == "Residents.Initiated") %>%
    select(Missing) %>%
    str_split(", |\n ") %>%
    .[[1]]

residents_vax_num <- agg_df %>%
    filter(Measure == "Residents.Initiated") %>%
    pull(Count)

residents_vax_denom <- anchored_denoms %>%
    filter(State %!in% missing_residents_vax_states) %>%
    summarise(ntl_residents_estimate = sum_na_rm(Residents.Population)) %>%
    pull(ntl_residents_estimate)

prison_residents_vax_rate <- residents_vax_num / residents_vax_denom
print(paste0("Estimated residents vaccination rate: ", prison_residents_vax_rate))

# ------------------------------------------------------------------------------
# national: active case rate for incarcerated people
# ------------------------------------------------------------------------------

missing_residents_active_states <- state_df %>%
    filter(is.na(Residents.Active)) %>%
    pull(State)

residents_active_num <- state_df %>%
    filter(!is.na(Residents.Active)) %>%
    summarise(residents_active = sum(Residents.Active)) %>%
    pull(residents_active)

residents_active_denom <- anchored_denoms %>%
    filter(State %!in% missing_residents_active_states) %>%
    summarise(ntl_residents_estimate = sum_na_rm(Residents.Population)) %>%
    pull(ntl_residents_estimate)

prison_residents_active_rate <- residents_active_num / residents_active_denom
print(paste0("Estimated residents active case rate: ", prison_residents_active_rate))

# ------------------------------------------------------------------------------
# national: vaccination rate for general, non-incarcerated population
# ------------------------------------------------------------------------------

general_vax_df <- gen_vax_df %>%
    filter(!is.na(State)) %>%
    summarise(general_vax_num = sum(General.Initiated),
              general_vax_denom = sum(General.Population)) %>%
    mutate(rate = general_vax_num / general_vax_denom)

print(paste0("Estimated general vaccination rate: ", general_vax_df$rate))

# ------------------------------------------------------------------------------
# national: active case rate for general, non-incarcerated population
# ------------------------------------------------------------------------------

general_active_df <- gen_covid_df %>%
    filter(!is.na(State)) %>%
    summarise(general_active_num = sum(General.Active),
              general_active_denom = sum(General.Population)) %>%
    mutate(rate = general_active_num / general_active_denom)

print(paste0("Estimated general active case rate: ", general_active_df$rate))

# ------------------------------------------------------------------------------
# combine national nums into one df
# ------------------------------------------------------------------------------

national_vax_active_rates_out <- tibble(
    gen_active_rate = general_active_df$rate,
    gen_initiated_rate = general_vax_df$rate,
    staff_active_rate = prison_staff_active_rate,
    staff_initiated_rate = prison_staff_vax_rate,
    residents_active_rate = prison_residents_active_rate,
    residents_initiated_rate = prison_residents_vax_rate
    ) %>%
    pivot_longer(everything(), names_to = "Measure", values_to = "rate") %>%
    mutate(
        pop = case_when(
          str_detect(Measure, "gen") ~ "General, non-incarcerated population",
          str_detect(Measure, "staff") ~ "Prison staff",
          TRUE ~ "Incarcerated population"
          ),
        metric = case_when(
            str_detect(Measure, "active") ~ "Active case rate",
            TRUE ~ "Vaccination rate (initiated)"
        ),
    ) 



# ------------------------------------------------------------------------------
# state: vaccination rates for staff/incarcerated people/overall pop
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


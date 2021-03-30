library(tidyverse)
library(magrittr)
library(lubridate)
library(glue)
library(readxl)
library(janitor)
library(skimr)
library(behindbarstools)

first_row_dates <- read_xlsx(file.path("~", "UCLA", "misc-data", "NYC-BOC", 
                                       "Daily-Report-Population-Data-Consolidated-Summaries-February-2021.xlsx"),
                             # range = cell_rows(1),
                             col_types = c("skip", rep("date", 348))
)

dat <- read_xlsx(file.path("~", "UCLA", "misc-data", "NYC-BOC", 
                           "Daily-Report-Population-Data-Consolidated-Summaries-February-2021.xlsx"),
                 col_names = FALSE
)

first_row_dates <- as.numeric(dat[1, -1])
first_row_dates_formatted <- as.Date(first_row_dates, origin = '1899-12-30')

population <- dat %>%
  slice(12:34) %>%
  tibble::rownames_to_column() %>%  
  pivot_longer(-rowname) %>% 
  pivot_wider(names_from=rowname, values_from=value) %>%
  row_to_names(row_number = 1) %>%
  select(-`...1`) %>%
  mutate(Date = first_row_dates_formatted) %>%
  select(Date, `Total Population in Custody`,
         Female:`Unknown Gender`) %>%
  pivot_longer(
    cols = Female:`Unknown Gender`,
    names_to = "gender",
    values_to = "population"
  ) %>%
  dplyr::rename(total_population = `Total Population in Custody`) %>%
  mutate(population_numeric = ifelse(population == "≤10", NA, population),
         population_numeric = as.numeric(population_numeric)) %>%
  select(-population) %>%
  pivot_wider(names_from = gender, values_from = population_numeric) %>%
  mutate(pop_sum_gender = vector_sum_na_rm(Female, Male, `Transgender Female`, `Transgender Male`, `Gender Non-Conforming`, Intersex, `Unknown Gender`),
         pop_mutually_exclusive = ifelse(pop_sum_gender == total_population, TRUE, FALSE),
         cisgender_aggregated = vector_sum_na_rm(Female, Male),
         not_cis_male_aggregated = vector_sum_na_rm(Female, `Transgender Female`, `Transgender Male`, `Gender Non-Conforming`, Intersex, `Unknown Gender`),
         gnc_aggregated = vector_sum_na_rm(`Transgender Female`, `Transgender Male`, `Gender Non-Conforming`, Intersex, `Unknown Gender`)) 

pop_to_compare <- population %>%
  select(Date, total_population, Female, Male, cisgender_aggregated, not_cis_male_aggregated, gnc_aggregated) %>%
  pivot_longer(
    cols = Female:gnc_aggregated,
    names_to = "gender",
    values_to = "population"
  )

exposed <- dat %>%
  slice(36:46) %>%
  tibble::rownames_to_column() %>%  
  pivot_longer(-rowname) %>% 
  pivot_wider(names_from=rowname, values_from=value) %>%
  row_to_names(row_number = 1) %>%
  select(-`...1`) %>%
  mutate(Date = first_row_dates_formatted) %>%
  dplyr::rename(total_exposed = `Total People in Exposed but Asymptomatic Units (as of 11:00am)\r\nNote: Categories below are not mutually exclusive.`) %>%
  select(Date, total_exposed,
         Female:`Unknown Gender`) %>%
  pivot_longer(
    cols = Female:`Unknown Gender`,
    names_to = "gender",
    values_to = "exposed"
  ) %>%
  mutate(exposed_numeric = ifelse(exposed == "≤10", NA, exposed),
         exposed_numeric = as.numeric(exposed_numeric)) %>%
  select(-exposed) %>%
  pivot_wider(names_from = gender, values_from = exposed_numeric) %>%
  mutate(exposed_sum_gender = vector_sum_na_rm(Female, Men, `Transgender Female`, `Transgender Male`, `Gender Non-Conforming`, Intersex, `Unknown Gender`),
         exposed_mutually_exclusive = ifelse(exposed_sum_gender == total_exposed, TRUE, FALSE),
         cisgender_aggregated = vector_sum_na_rm(Female, Men),
         not_cis_male_aggregated = vector_sum_na_rm(Female, `Transgender Female`, `Transgender Male`, `Gender Non-Conforming`, Intersex, `Unknown Gender`),
         gnc_aggregated = vector_sum_na_rm(`Transgender Female`, `Transgender Male`, `Gender Non-Conforming`, Intersex, `Unknown Gender`)) 

exposed_to_compare <- exposed %>%
  select(Date, total_exposed, Female, Men, cisgender_aggregated, not_cis_male_aggregated, gnc_aggregated) %>%
  pivot_longer(
    cols = Female:gnc_aggregated,
    names_to = "gender",
    values_to = "exposed"
  )

confirmed <- dat %>%
  slice(48:58) %>%
  tibble::rownames_to_column() %>%  
  pivot_longer(-rowname) %>% 
  pivot_wider(names_from=rowname, values_from=value) %>%
  row_to_names(row_number = 1) %>%
  select(-`...1`) %>%
  mutate(Date = first_row_dates_formatted) %>%
  dplyr::rename(total_confirmed = `Total Patients in Housing Areas Used for COVID-19 Patients and Symptomatic Patients (as of 11:00am)\r\nNote: Categories below are not mutually exclusive.`) %>%
  select(Date, total_confirmed,
         Female:`Unknown Gender`) %>%
  pivot_longer(
    cols = Female:`Unknown Gender`,
    names_to = "gender",
    values_to = "confirmed"
  ) %>%
  mutate(confirmed_numeric = ifelse(confirmed == "≤10", NA, confirmed),
         confirmed_numeric = as.numeric(confirmed_numeric)) %>%
  select(-confirmed) %>%
  pivot_wider(names_from = gender, values_from = confirmed_numeric) %>%
  mutate(confirmed_sum_gender = vector_sum_na_rm(Female, Male, `Transgender Female`, `Transgender Male`, `Gender Non-Conforming`, Intersex, `Unknown Gender`),
         cisgender_aggregated = vector_sum_na_rm(Female, Male),
         both_cis_real = ifelse((!is.na(Female) & !is.na(Male)), TRUE, FALSE),
         total_confirmed = as.numeric(total_confirmed),
         gnc_aggregated = ifelse(both_cis_real, 
                                 total_confirmed - cisgender_aggregated,
                                 NA),
         not_cis_male_aggregated = ifelse(!is.na(Male),
                                          total_confirmed - Male,
                                          NA)
  )

confirmed_to_compare <- confirmed %>%
  select(Date, total_confirmed, Female, Male, cisgender_aggregated, not_cis_male_aggregated, gnc_aggregated) %>%
  pivot_longer(
    cols = Female:gnc_aggregated,
    names_to = "gender",
    values_to = "confirmed"
  )

exposed_confirmed <- left_join(confirmed_to_compare, exposed_to_compare, by = c("gender", "Date"))
merged <- left_join(pop_to_compare, exposed_confirmed, by = c("gender", "Date"))
skim(merged)

out <- merged %>%
  mutate(conf_rate = ((confirmed)  / population) * 100) %>%
  select(Date, gender, 
         starts_with("confirmed"), starts_with("population"),
         starts_with("exposed"), conf_rate)

## make a plot ! 
plt <- out %>%
  ggplot( aes(x = Date, y = conf_rate, group = gender, color = gender)) +
  geom_line()
ggsave("nyc-boc-covid_aggregated.png", plt, width = 12, height = 10)

## another plt
## cis male and not-cis male
plt2 <- out %>%
  filter((gender == "Male") | (gender == "not_cis_male_aggregated")) %>%
  ggplot( aes(x = Date, y = conf_rate, group = gender, color = gender)) +
  geom_line()
ggsave("nyc-boc-covid_aggregated_collapsed.png", plt2, width = 12, height = 10)



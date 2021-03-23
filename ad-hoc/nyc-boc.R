library(tidyverse)
library(lubridate)
library(glue)
library(readxl)
library(janitor)
library(skimr)

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
         Female:Intersex) %>%
  pivot_longer(
    cols = Female:Intersex,
    names_to = "gender",
    values_to = "population"
  ) %>%
  dplyr::rename(total_pop = `Total Population in Custody`)
  
exposed <- dat %>%
  slice(36:46) %>%
  tibble::rownames_to_column() %>%  
  pivot_longer(-rowname) %>% 
  pivot_wider(names_from=rowname, values_from=value) %>%
  row_to_names(row_number = 1) %>%
  select(-`...1`) %>%
  mutate(Date = first_row_dates_formatted) %>%
  select(Date, 
         Female:Intersex) %>%
  pivot_longer(
    cols = Female:Intersex,
    names_to = "gender",
    values_to = "exposed_asymp"
  ) 

confirmed <- dat %>%
  slice(48:58) %>%
  tibble::rownames_to_column() %>%  
  pivot_longer(-rowname) %>% 
  pivot_wider(names_from=rowname, values_from=value) %>%
  row_to_names(row_number = 1) %>%
  select(-`...1`) %>%
  mutate(Date = first_row_dates_formatted) %>%
  select(Date, 
         Female:Intersex) %>%
  pivot_longer(
    cols = Female:Intersex,
    names_to = "gender",
    values_to = "confirmed"
  ) 

exposed_confirmed <- left_join(confirmed, exposed, by = c("gender", "Date"))
merged <- left_join(population, exposed_confirmed, by = c("gender", "Date"))
skim(merged)

out <- merged %>%
  mutate(confirmed_numeric = ifelse(confirmed == "≤10", 1, confirmed),
         population_numeric = ifelse(population == "≤10", 1, population),
         exposed_asymp_numeric = ifelse(exposed_asymp == "≤10", 1, exposed_asymp),
         ) %>%
  mutate(confirmed_numeric = as.numeric(confirmed_numeric),
         population_numeric = as.numeric(population_numeric),
         exposed_asymp_numeric = as.numeric(exposed_asymp_numeric)) %>%
  mutate(conf_rate = ((confirmed_numeric +.000001)  / population_numeric) * 100) %>%
  select(Date, gender, 
         starts_with("confirmed"), starts_with("population"),
         starts_with("exposed"), conf_rate,
         total_pop)

## make a plot ! 
plt <- out %>%
  ggplot( aes(x = Date, y = conf_rate, group = gender, color = gender)) +
  geom_line()
ggsave("nyc-boc-covid.png", plt, width = 12, height = 10)




rm(list = ls())

library(tidyverse)
library(readxl)
library(xlsx)

# Read and merge data
data <- read_excel("data/raw/Vaccination Questionnaire Data 1.6 - Sharon.xlsx", 
                   sheet = "Data", guess_max = 5000)

# Read CDC crosswalk 
xwalk_cdc <- read.csv("data/interim/xwalk_cdc.csv")

# Join datasets and reshape reasons long 
joined <- data %>% 
    select(`Index global`, `Accept?`, `j/p`, `age`, `primary race`, `gender`, 
           `1st Reason Code`, `2nd Reason Code`, `3rd Reason Code`) %>% 
    as.data.frame() %>% 
    reshape::melt(id = c("Index global", "Accept?", "j/p", "age", "primary race", "gender")) %>% 
    filter(!is.na(value)) %>% 
    left_join(xwalk_cdc, by = c("value" = "Code")) %>% 
    mutate(age = as.numeric(age)) 

# Function to generate most common reasons 
process_crosstab <- function(x) {
    x %>% 
        group_by(Category) %>% 
        summarise(Responses = n()) %>% 
        mutate(Percent = scales::percent(Responses / sum(Responses), accuracy = 0.1L)) %>% 
        arrange(-Responses) %>% 
        ungroup() %>% 
        as.data.frame()
}

# ------------------------------------------------------------------------------

# Overall 
overall <- joined %>% 
    process_crosstab() %>% 
    rename("Responses_overall" = "Responses", 
           "Percent_overall" = "Percent")

# By response 
no <- joined %>% 
    filter(`Accept?` == "n") %>% 
    process_crosstab() %>% 
    rename("Responses_no" = "Responses", 
           "Percent_no" = "Percent")

maybe <- joined %>% 
    filter(`Accept?` == "m") %>% 
    process_crosstab() %>% 
    rename("Responses_maybe" = "Responses", 
           "Percent_maybe" = "Percent")

response <- overall %>% 
    left_join(no, by = "Category") %>% 
    left_join(maybe, by = "Category")

write.xlsx(response, "data/out/crosstabs.xlsx", sheetName = "response", row.names = FALSE)

# By facility type
jail <- joined %>% 
    filter(`j/p` == "jail") %>% 
    process_crosstab() %>% 
    rename("Responses_jail" = "Responses", 
           "Percent_jail" = "Percent")

prison <- joined %>% 
    filter(`j/p` == "prison") %>% 
    process_crosstab() %>%
    rename("Responses_prison" = "Responses", 
           "Percent_prison" = "Percent")

facility <- overall %>% 
    left_join(jail, by = "Category") %>% 
    left_join(prison, by = "Category")

write.xlsx(facility, "data/out/crosstabs.xlsx", sheetName = "facility", row.names = FALSE, append = TRUE)

# By gender
male <- joined %>% 
    filter(gender == "m") %>% 
    process_crosstab() %>%
    rename("Responses_male" = "Responses", 
           "Percent_male" = "Percent")

female <- joined %>% 
    filter(gender == "f") %>% 
    process_crosstab() %>%
    rename("Responses_female" = "Responses", 
           "Percent_female" = "Percent")

gender <- overall %>% 
    left_join(male, by = "Category") %>% 
    left_join(female, by = "Category")

write.xlsx(gender, "data/out/crosstabs.xlsx", sheetName = "gender", row.names = FALSE, append = TRUE)

# By race
white <- joined %>% 
    filter(`primary race` == "w") %>% 
    process_crosstab() %>%
    rename("Responses_white" = "Responses", 
           "Percent_white" = "Percent")

black <- joined %>% 
    filter(`primary race` == "b") %>% 
    process_crosstab() %>%
    rename("Responses_black" = "Responses", 
           "Percent_black" = "Percent")

hispanic <- joined %>% 
    filter(`primary race` == "h") %>% 
    process_crosstab() %>%
    rename("Responses_hispanic" = "Responses", 
           "Percent_hispanic" = "Percent")

race <- overall %>% 
    left_join(white, by = "Category") %>% 
    left_join(black, by = "Category") %>% 
    left_join(hispanic, by = "Category")

write.xlsx(race, "data/out/crosstabs.xlsx", sheetName = "race", row.names = FALSE, append = TRUE)

# By age 
young <- joined %>% 
    filter(age < 35) %>% 
    process_crosstab() %>%
    rename("Responses_under35" = "Responses", 
           "Percent_under35" = "Percent")

old <- joined %>% 
    filter(age >= 35) %>% 
    process_crosstab() %>%
    rename("Responses_over35" = "Responses", 
           "Percent_over35" = "Percent")

age <- overall %>% 
    left_join(young, by = "Category") %>% 
    left_join(old, by = "Category") 

write.xlsx(age, "data/out/crosstabs.xlsx", sheetName = "age", row.names = FALSE, append = TRUE)

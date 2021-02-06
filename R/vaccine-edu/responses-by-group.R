rm(list = ls())

library(tidyverse)
library(readxl)
library(xlsx)

# Read and merge data
data <- read_excel("data/raw/Vaccination Questionnaire Data 2.6.xlsx", 
                   sheet = "Data", guess_max = 5000)

# Read CDC crosswalk 
xwalk_cdc <- read.csv("data/interim/crosswalk.csv")

# Join datasets and reshape reasons long 
joined <- data %>% 
    left_join(xwalk_cdc, by = c("1st Reason Code" = "Code")) %>% 
    filter(`Accept?` %in% c("n", "m")) %>% 
    filter(!is.na(Category)) %>% 
    filter(!Category == "No response or null response") %>% 
    mutate(age = as.numeric(age)) %>% 
    mutate("under35" = ifelse(age < 35, 1, 0)) 

# Get total number of unique individuals 
overall_n <- joined %>% select("Index global") %>% 
    distinct() %>% 
    nrow()

# Generate summary table 
process_crosstab <- function(x) {
    # Number of unique responses 
    denom <- x %>% 
        select("Index global") %>% 
        distinct() %>% 
        nrow()

    x %>% 
        group_by(Category) %>% 
        summarise(n = n()) %>% 
        mutate(pct = scales::percent(n / denom, accuracy = 0.1L), 
               output = paste0(n, " (", pct, ")")) %>% 
        arrange(-n) %>% 
        select(Category, output) %>% 
        ungroup() %>% 
        as.data.frame() %>% 
        add_row(Category = "Total", output = paste0(
            denom, " (", scales::percent(denom / overall_n, accuracy = 0.1L), ")"))
}

# ------------------------------------------------------------------------------

# Overall 
overall <- joined %>% 
    process_crosstab() %>% 
    rename("Overall" = "output")

# By response 
no <- joined %>% 
    filter(`Accept?` == "n") %>% 
    process_crosstab() %>% 
    rename("No" = "output")

maybe <- joined %>% 
    filter(`Accept?` == "m") %>% 
    process_crosstab() %>% 
    rename("Maybe" = "output")

# By facility type
jail <- joined %>% 
    filter(`j/p` == "jail") %>% 
    process_crosstab() %>% 
    rename("Jail" = "output")

prison <- joined %>% 
    filter(`j/p` == "prison") %>% 
    process_crosstab() %>%
    rename("Prison" = "output")

# By gender
male <- joined %>% 
    filter(gender == "m") %>% 
    process_crosstab() %>%
    rename("Male" = "output")

female <- joined %>% 
    filter(gender == "f") %>% 
    process_crosstab() %>%
    rename("Female" = "output") 

# By race
white <- joined %>% 
    filter(`primary race` == "w") %>% 
    process_crosstab() %>%
    rename("White" = "output")

black <- joined %>% 
    filter(`primary race` == "b") %>% 
    process_crosstab() %>%
    rename("Black" = "output") 

hispanic <- joined %>% 
    filter(`primary race` == "h") %>% 
    process_crosstab() %>%
    rename("Hispanic" = "output") 

# By age 
young <- joined %>% 
    filter(under35 == 1) %>% 
    process_crosstab() %>%
    rename("Under 35" = "output")

old <- joined %>% 
    filter(under35 == 0) %>% 
    process_crosstab() %>%
    rename("Over 35" = "output")

# Combine into one wide table 
out <- overall %>% 
    left_join(no) %>% 
    left_join(maybe) %>% 
    left_join(jail) %>% 
    left_join(prison) %>% 
    left_join(male) %>% 
    left_join(female) %>% 
    left_join(white) %>% 
    left_join(black) %>% 
    left_join(hispanic) %>% 
    left_join(young) %>% 
    left_join(old) 

write.xlsx(out, "data/out/crosstabs.xlsx", sheetName = "table-3", row.names = FALSE)

# ------------------------------------------------------------------------------

# Chi-squared tests 
chisq.test(table(joined$`Accept?`, joined$Category))
chisq.test(table(joined$`j/p`, joined$Category))
chisq.test(table(joined$gender, joined$Category))
chisq.test(table(joined$`primary race`, joined$Category))
chisq.test(table(joined$under35, joined$Category))

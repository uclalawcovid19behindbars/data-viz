rm(list = ls())

library(behindbarstools)
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
    filter(!is.na(Category)) %>% 
    filter(!Category == "No response or null response") 

# Get total number of unique individuals 
overall_n <- joined %>% select("Index global") %>% 
    distinct() %>% 
    nrow()

# Generate summary table 
process_crosstab <- function(x, combine_cols = TRUE) {
    # Number of unique responses 
    denom <- x %>% 
        select("Index global") %>% 
        distinct() %>% 
        nrow()

    if (!combine_cols){
        x %>% 
            group_by(Category) %>% 
            summarise(n = n()) %>% 
            mutate(pct = n / denom) 
        
    } else if (combine_cols){
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
}

# ------------------------------------------------------------------------------

# For blog plots 
overall <- joined %>% 
    process_crosstab(combine_cols = F) %>% 
    mutate(group = "overall")

no <- joined %>% 
    filter(`Accept?` == "n") %>% 
    process_crosstab(combine_cols = F) %>% 
    mutate(group = "no")
    
maybe <- joined %>% 
    filter(`Accept?` == "m") %>% 
    process_crosstab(combine_cols = F) %>% 
    mutate(group = "maybe")

overall <- bind_rows(overall, no, maybe)

level_order <- c(
    "Other", 
    "Believing in a virus- or vaccine-related conspiracy to harm incarcerated or detained persons", 
    "Against vaccination in general", 
    "Not perceiving themselves at risk for COVID-19 or perceiving vaccination as unnecessary",
    "Distrust of health care, correctional, or governmental personnel or institutions", 
    "Awaiting more information or to see others vaccinated", 
    "Efficacy or safety concerns"
    )

p <- overall %>% 
    filter(group != "overall") %>% 
    ggplot(aes(x = factor(Category, level = level_order), 
               y = pct, 
               label = scales::percent(pct, accuracy = 1))) +
    geom_point(color = "#4C6788", size = 2.5) +
    geom_segment(aes(x = Category, xend = Category, y = 0, yend = pct), size = 1.0, color = "#4C6788") +
    geom_text(size = 3.5, position = position_nudge(y = 0.06, x = 0.25), family = "Helvetica") + 
    coord_flip() + 
    facet_wrap(~ factor(group, 
                        levels = c("maybe", "no"), 
                        labels = c("Maybe (hesitancy)\n(n = 458 respondents)", 
                                   "No (refusal)\n(n = 1,823 respondents)"))) + 
    scale_x_discrete(labels = function(x) str_wrap(x, width = 51)) + 
    scale_y_continuous(label = scales::percent, limits = c(0, 0.7), breaks = c(0, 0.3, 0.6)) + 
    labs(title = "Primary reason for COVID-19 refusal or hesitancy among incarcerated people", 
         subtitle = "By willingness to receive a COVID-19 vaccination when it is authorized", 
         x = "Primary reason") + 
    theme_behindbars(base_color = "black", base_size = 14) + 
    theme(panel.spacing = unit(2, "lines"), 
          axis.text.y = element_text(hjust = 0))

ggsave("v4.svg", p, width = 10, height = 5)


# ------------------------------------------------------------------------------

# For MMWR analysis 
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

native_am <- joined %>% 
    filter(`primary race` == "n") %>% 
    process_crosstab() %>% 
    rename("Native American" = "output")

other <- joined %>% 
    filter(!`primary race` %in% c("w", "b", "h", "n")) %>% 
    process_crosstab() %>% 
    rename("Other Race" = "output")

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
    left_join(native_am) %>% 
    left_join(other) %>% 
    left_join(young) %>% 
    left_join(old) 

write.xlsx(out, "data/out/crosstabs.xlsx", sheetName = "yes", row.names = FALSE)

# ------------------------------------------------------------------------------

# Chi-squared tests 
chisq.test(table(joined$`Accept?`, joined$Category))
chisq.test(table(joined$`j/p`, joined$Category))
chisq.test(table(joined$gender, joined$Category))
chisq.test(table(joined$race_aggregated, joined$Category))
chisq.test(table(joined$under35, joined$Category))

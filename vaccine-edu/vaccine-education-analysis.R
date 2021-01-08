library(tidyverse)
library(readxl)

# Load data 
data <- read_excel("Vaccination Questionnaire Data 1.6 - Sharon.xlsx", 
                   sheet = "Data", guess_max = 5000)

# Clean strings
data <- data %>% 
    filter(!is.na(`Accept?`)) %>% 
    mutate(Reason_clean = str_squish(Reason), 
           Reason_clean = str_remove_all(Reason_clean, "[^[:space:]|^[:alnum:]]"), 
           Reason_clean = str_to_lower(Reason_clean), 
           Reason_clean = str_remove(Reason_clean, "he "), 
           Reason_clean = str_remove(Reason_clean, "she "), 
           Reason_clean = str_remove(Reason_clean, "i "), 
           Reason_clean = str_remove(Reason_clean, "am "), 
           Reason_clean = str_remove(Reason_clean, "is "), 
           Reason_clean = str_remove(Reason_clean, " it"), 
           Reason_clean = str_replace(Reason_clean, "doesnt", "does not"), 
           Reason_clean = str_replace(Reason_clean, "dont", "does not"), 
           Reason_clean = str_replace(Reason_clean, "do not", "does not"), 
           Reason_clean = str_replace(Reason_clean, "wants", "want"))

# Sort by frequency 
value_counts <- data %>%
    filter(`Accept?` %in% c("m", "n")) %>%
    filter(!is.na(Reason_clean)) %>% 
    group_by(Reason_clean) %>%
    count() %>% 
    arrange(-n, Reason_clean) %>% 
    rename("Responses" = n) 

write.csv(value_counts, "responses-by-frequency_v2.csv", row.names = FALSE)

# Get basic summary stats 
data %>% 
    group_by(`j/p`) %>% 
    count() %>% 
    mutate(pct = n / nrow(data))

# Responses by jails vs. prisons 
viz1 <- data %>% 
    group_by(`j/p`, `Accept?`) %>% 
    summarise(n = n()) %>% 
    mutate(pct = n / sum(n), 
           label = paste0(scales::percent(pct, accuracy = 1), "\n", "n=", n)) %>% 
    ggplot(aes(x = `Accept?`, y = pct, label = label)) + 
    geom_text(nudge_y = 0.03) + 
    geom_bar(stat = "identity") + 
    facet_wrap(~`j/p`) + 
    theme_bw(base_size = 14) + 
    coord_flip() + 
    labs(title = "Responses by type of facility", 
         subtitle = "People in jails tended to be more likely to respond no (48% vs. 32%)")

ggsave("response-by-jp.png", viz1)

# Response by age 
viz2 <- data %>% 
    mutate(age = as.numeric(age)) %>% 
    ggplot(aes(x = `age`, y = `Accept?`)) +  
    geom_boxplot() + 
    theme_bw(base_size = 14) + 
    labs(title = "Responses by age", 
         subtitle = "People who responded yes tended to be older")

ggsave("response-by-age.png", viz2)

# Jails/prisons by age 
viz3 <- data %>% 
    mutate(age = as.numeric(age)) %>% 
    ggplot(aes(x = `age`, y = `j/p`)) +  
    geom_boxplot() + 
    theme_bw(base_size = 14) + 
    labs(title = "Age by type of facility", 
         subtitle = "People in prisons tended to be older")

ggsave("jp-by-age.png", viz3)

# Response by previous response 
viz4 <- data %>% 
    mutate(accept_lag = paste("Previous respondent:", lag(`Accept?`)), 
           accept_lag = factor(
               accept_lag, 
               levels = c(
                   "Previous respondent: y", "Previous respondent: n", "Previous respondent: m"))) %>% 
    filter(!is.na(lag(`Accept?`))) %>% 
    group_by(accept_lag, `Accept?`) %>% 
    summarise(n = n()) %>% 
    mutate(pct = n / sum(n), 
           label = paste0(scales::percent(pct, accuracy = 1), "\n", "n=", n)) %>% 
    ggplot(aes(x = `Accept?`, y = pct, label = label)) + 
    geom_text(nudge_y = 0.04) + 
    scale_y_continuous(limits = c(0, 0.65)) + 
    geom_bar(stat = "identity") + 
    facet_wrap(~accept_lag) + 
    theme_bw(base_size = 14) + 
    coord_flip() + 
    labs(title = "Responses by previous response", 
         subtitle = "Responses tended to correlate to the previous response")

ggsave("response-by-previous.png", viz4)

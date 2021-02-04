rm(list = ls())

library(tidyverse)
library(readxl)
library(xlsx)

# Read and merge data
data <- read_excel("data/raw/Vaccination Questionnaire Data 1.6 - Sharon.xlsx", 
                   sheet = "Data", guess_max = 5000)

xwalk <- read.csv("data/interim/xwalk.csv")

joined <- data %>% 
    mutate("Group" = case_when(`Accept?` == "m" ~ "maybe",
                               `Accept?` == "n" ~ "no",
                               `Accept?` == "y" ~ "yes", 
                               TRUE ~ `Accept?`)) %>% 
    left_join(xwalk, by = c("Group" = "group", 
                            "1st Reason Code" = "code")) %>% 
    filter(!is.na(subcategory)) %>% 
    filter(subcategory != "") %>% 
    rename("Code" = `1st Reason Code`, 
           "Category" = `category`, 
           "Subcategory" = `subcategory`) %>% 
    mutate(age = as.numeric(age))
    
# Function to generate most common reasons 
process_crosstab <- function(x, n = 20) {
    x %>% 
        group_by(Group, Code, Category, Subcategory) %>% 
        summarise(Responses = n()) %>% 
        arrange(-Responses) %>% 
        head(n) %>% 
        ungroup() %>% 
        as.data.frame()
}

# ------------------------------------------------------------------------------

# Overall 
overall <- joined %>% 
    process_crosstab()

write.xlsx(overall, "data/out/crosstabs.xlsx", sheetName = "overall", row.names = FALSE)

# By response 
no <- joined %>% 
    filter(Group == "no") %>% 
    process_crosstab()

maybe <- joined %>% 
    filter(Group == "maybe") %>% 
    process_crosstab()

write.xlsx(no, "data/out/crosstabs.xlsx", sheetName = "no", row.names = FALSE, append = TRUE)
write.xlsx(maybe, "data/out/crosstabs.xlsx", sheetName = "maybe", row.names = FALSE, append = TRUE)

# By facility type
jail <- joined %>% 
    filter(`j/p` == "jail") %>% 
    process_crosstab()

prison <- joined %>% 
    filter(`j/p` == "prison") %>% 
    process_crosstab()

write.xlsx(jail, "data/out/crosstabs.xlsx", sheetName = "jail", row.names = FALSE, append = TRUE)
write.xlsx(prison, "data/out/crosstabs.xlsx", sheetName = "prison", row.names = FALSE, append = TRUE)

# By gender
male <- joined %>% 
    filter(gender == "m") %>% 
    process_crosstab()

female <- joined %>% 
    filter(gender == "f") %>% 
    process_crosstab()

write.xlsx(male, "data/out/crosstabs.xlsx", sheetName = "male", row.names = FALSE, append = TRUE)
write.xlsx(female, "data/out/crosstabs.xlsx", sheetName = "female", row.names = FALSE, append = TRUE)

# By race
white <- joined %>% 
    filter(`primary race` == "w") %>% 
    process_crosstab()

black <- joined %>% 
    filter(`primary race` == "b") %>% 
    process_crosstab()

hispanic <- joined %>% 
    filter(`primary race` == "h") %>% 
    process_crosstab()

write.xlsx(white, "data/out/crosstabs.xlsx", sheetName = "white", row.names = FALSE, append = TRUE)
write.xlsx(black, "data/out/crosstabs.xlsx", sheetName = "black", row.names = FALSE, append = TRUE)
write.xlsx(hispanic, "data/out/crosstabs.xlsx", sheetName = "hispanic", row.names = FALSE, append = TRUE)

# By age 
young <- joined %>% 
    filter(age < 35) %>% 
    process_crosstab()

old <- joined %>% 
    filter(age >= 35) %>% 
    process_crosstab()

write.xlsx(young, "data/out/crosstabs.xlsx", sheetName = "below_35", row.names = FALSE, append = TRUE)
write.xlsx(old, "data/out/crosstabs.xlsx", sheetName = "above_35", row.names = FALSE, append = TRUE)


library(tidyverse)
library(behindbarstools)

# Load data 
scorecard <- read.csv("data/interim/scorecard-data.csv", skip = 1)

# Assign scores 
scored <- scorecard %>% 
    mutate(
        Machine.Readable_score = case_when(
            Machine.Readable == "Yes" ~ 2, Machine.Readable == "No" ~ 0), 
        Regularly.Updated_score = case_when(
            Regularly.Updated == "Yes" ~ 2, Regularly.Updated == "No" ~ 0), 
        Historical.Data_score = case_when(
            Historical.Data == "Yes" ~ 2, Historical.Data == "No" ~ 0), 
        Documented_score = case_when(
            Documented == "Yes" ~ 2, Documented == "No" ~ 0), 
        Reliable_score = case_when(
            Reliable != "No" ~ 2, Reliable == "No" ~ 0), 
        Residents.Cumulative.Cases_score = case_when(
            Residents.Cumulative.Cases == "Facility-Level" ~ 2, 
            Residents.Cumulative.Cases == "Statewide" ~ 1, 
            Residents.Cumulative.Cases == "" ~ 0), 
        Residents.Cumulative.Deaths_score = case_when(
            Residents.Cumulative.Deaths == "Facility-Level" ~ 2, 
            Residents.Cumulative.Deaths == "Statewide" ~ 1, 
            Residents.Cumulative.Deaths == "" ~ 0), 
        Residents.Active.Cases_score = case_when(
            Residents.Active.Cases == "Facility-Level" ~ 2, 
            Residents.Active.Cases == "Statewide" ~ 1, 
            Residents.Active.Cases == "" ~ 0), 
        Residents.Tests_score = case_when(
            Residents.Tests == "Facility-Level" ~ 2, 
            Residents.Tests == "Statewide" ~ 1, 
            Residents.Tests == "" ~ 0), 
        Residents.Population_score = case_when(
            Residents.Population == "Facility-Level" ~ 2, 
            Residents.Population == "Statewide" ~ 1, 
            Residents.Population == "" ~ 0), 
        Staff.Cumulative.Cases_score = case_when(
            Staff.Cumulative.Cases == "Facility-Level" ~ 2, 
            Staff.Cumulative.Cases == "Statewide" ~ 1, 
            Staff.Cumulative.Cases == "" ~ 0), 
        Staff.Cumulative.Deaths_score = case_when(
            Staff.Cumulative.Deaths == "Facility-Level" ~ 2, 
            Staff.Cumulative.Deaths == "Statewide" ~ 1, 
            Staff.Cumulative.Deaths == "" ~ 0), 
        Staff.Tests_score = case_when(
            Staff.Tests == "Facility-Level" ~ 2, 
            Staff.Tests == "Statewide" ~ 1, 
            Staff.Tests == "" ~ 0)
        ) 

# Sum scores 
out <- scored %>% 
    mutate(
        Quality_total = Machine.Readable_score + Regularly.Updated_score + Historical.Data_score + Documented_score + Reliable_score,
        Residents_total = Residents.Cumulative.Cases_score + Residents.Cumulative.Deaths_score + Residents.Active.Cases_score + Residents.Tests_score + Residents.Population_score, 
        Staff_total = Staff.Cumulative.Cases_score + Staff.Cumulative.Deaths_score + Staff.Tests_score, 
        Total = scored %>% select(ends_with("_score")) %>% rowSums(), 
        Percentage = Total / 26 * 100) %>% 
    mutate(
        Score = case_when(Percentage >= 90 ~ "A", 
                          Percentage >= 80 & Percentage < 90 ~ "B", 
                          Percentage >= 70 & Percentage < 80 ~ "C", 
                          Percentage >= 60 & Percentage < 70 ~ "D", 
                          Percentage < 60 ~ "F"))

write.csv(out, "data/out/scored.csv")

# Plot distribution 
ggplot(out, aes(x = Total)) + 
    geom_histogram(bins = 26, color = "white") + 
    theme_minimal()

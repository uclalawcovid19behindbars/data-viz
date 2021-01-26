library(behindbarstools)
library(tidyverse)

scrape_df <- read_scrape_data() %>% 
    filter(Date > Sys.Date() - 14) 

# State
facility <- scrape_df %>% 
    filter(Jurisdiction == "state") %>% 
    filter(Name != "STATEWIDE") %>% 
    group_by(State) %>% 
    summarise_all(funs(sum(!is.na(.)))) %>% 
    select(State, Residents.Confirmed:Residents.Tested) %>% 
    mutate_if(is.numeric, funs(if_else(. > 0, "Facility-Level", NA_character_)))

statewide <- scrape_df %>% 
    filter(Jurisdiction == "state") %>% 
    filter(Name == "STATEWIDE") %>% 
    group_by(State) %>% 
    summarise_all(funs(sum(!is.na(.)))) %>% 
    select(State, Residents.Confirmed:Residents.Tested) %>% 
    mutate_if(is.numeric, funs(if_else(. > 0, "Statewide", NA_character_)))

coalesce_by_column <- function(df) {
    return(dplyr::coalesce(!!! as.list(df)))
}

state <- rbind(facility, statewide) %>%
    group_by(State) %>%
    summarise_all(coalesce_by_column) %>% 
    mutate(Jurisdiction = "state") %>% 
    mutate(Name = paste(toupper(State), "DOC")) %>% 
    select(Name, Jurisdiction, State, Residents.Confirmed:Residents.Tested) %>% 
    add_row(Name = "OKLAHOMA DOC", Jurisdiction = "state", State = "Oklahoma")

# County
county <- scrape_df %>% 
    filter(Jurisdiction == "county") %>% 
    filter(State != "Texas") %>% 
    group_by(Name, State, source, Jurisdiction) %>% 
    summarise_all(funs(sum(!is.na(.)))) %>% 
    select(Name, Jurisdiction, State, source, Residents.Confirmed:Residents.Tested) %>% 
    mutate_if(is.numeric, funs(if_else(. > 0, "Facility-Level", NA_character_)))

tx_jails <- scrape_df %>% 
    filter(Jurisdiction == "county") %>% 
    filter(State == "Texas") %>% 
    group_by(State, source, Jurisdiction) %>% 
    summarise_all(funs(sum(!is.na(.)))) %>% 
    mutate(Name = "TEXAS JAILS") %>% 
    select(Name, Jurisdiction, State, source, Residents.Confirmed:Residents.Tested) %>% 
    mutate_if(is.numeric, funs(if_else(. > 0, "Facility-Level", NA_character_)))
    
# Federal 
federal <- scrape_df %>% 
    filter(Jurisdiction == "federal") %>% 
    filter(source == "https://www.bop.gov/coronavirus/") %>% 
    group_by(source, Jurisdiction) %>% 
    summarise_all(funs(sum(!is.na(.)))) %>% 
    mutate(Name = "FEDERAL / BOP") %>% 
    select(Name, Jurisdiction, Residents.Confirmed:Residents.Tested) %>% 
    mutate_if(is.numeric, funs(if_else(. > 0, "Facility-Level", NA_character_)))

# Immigration
immigration <- scrape_df %>% 
    filter(Jurisdiction == "immigration") %>% 
    group_by(source, Jurisdiction) %>% 
    summarise_all(funs(sum(!is.na(.)))) %>% 
    mutate(Name = "IMMIGRATION / ICE") %>% 
    select(Name, Jurisdiction, source, Residents.Confirmed:Residents.Tested) %>% 
    mutate_if(is.numeric, funs(if_else(. > 0, "Facility-Level", NA_character_)))

# Combine
combined <- bind_rows(
    county, 
    tx_jails, 
    federal, 
    immigration, 
    state) %>% 
    ungroup %>% 
    select(-source, -Population.Feb20) %>% 
    mutate(Jurisdiction = factor(Jurisdiction, levels = c("state", "federal", "immigration", "county"))) %>% 
    arrange(Jurisdiction, State, Name)

write.table(combined, "data-availability-by-jurisdiction.csv", 
            sep = ",", col.names = TRUE, row.names = FALSE, na = "")

library(tidyverse)
library(behindbarstools)

# Latest state agg 
state_agg <- "https://raw.githubusercontent.com/uclalawcovid19behindbars/data/master/latest-data/latest_state_counts.csv" %>% 
    read_csv(col_types = cols())

# Overall new cases (from NYT)
nyt_covid <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv" %>%
    read_csv(col_types = cols()) %>%
    rename(NYT.Date = date) %>%
    group_by(state) %>% 
    mutate(Gen.Active = diff_roll_sum(cases, NYT.Date)) %>% 
    filter(NYT.Date == max(.$NYT.Date)) %>% 
    select(State = state, 
           Gen.Active)

# Overall vaccinations (from CDC)
cdc <- str_c(
    "https://covid.cdc.gov/covid-data-tracker/COVIDData/", 
    "getAjaxData?id=vaccination_data") %>% 
    jsonlite::read_json(simplifyVector = TRUE) 

joined <- as_tibble(cdc$vaccination_data) %>% 
    select(State = LongName, 
           Gen.Vax = Administered_Dose1_Recip_18Plus, 
           Gen.Pop = Census2019) %>% 
    full_join(nyt_covid, by = "State") %>% 
    full_join(state_agg %>% 
                  select(State, 
                         Res.Vax = Residents.Initiated, 
                         Res.Active = Residents.Active, 
                         Res.Pop = Residents.Population, 
                         Staff.Vax = Staff.Initiated, 
                         Staff.Active = Staff.Active,
                         Staff.Pop = Staff.Population), 
              by = "State") %>% 
    drop_na() %>% 
    summarise_if(is.numeric, sum) %>% 
    gather(key, value) %>%
    separate(key, into = c("Population", "Metric")) %>%
    spread(Metric, value) %>% 
    bind_cols(as_tibble(Hmisc::binconf(.$Active, .$Pop))) %>%
    rename(Active.Pct = PointEst, 
           Active.Lower = Lower, 
           Active.Upper = Upper) %>% 
    bind_cols(as_tibble(Hmisc::binconf(.$Vax, .$Pop))) %>%
    rename(Vax.Pct = PointEst, 
           Vax.Lower = Lower, 
           Vax.Upper = Upper) 
    
active <- joined %>% 
    ggplot(aes(x = Population, y = Active.Pct, ymin = Active.Lower, ymax = Active.Upper)) + 
    geom_bar(stat = "identity") + 
    geom_errorbar(width = 0.25) +
    theme_minimal()
    
vax <- joined %>% 
    ggplot(aes(x = Population, y = Vax.Pct, ymin = Vax.Lower, ymax = Vax.Upper)) + 
    geom_bar(stat = "identity") + 
    geom_errorbar(width = 0.25) + 
    theme_minimal()
    
ggpubr::ggarrange(active, vax)
    
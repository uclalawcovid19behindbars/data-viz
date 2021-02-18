library(behindbarstools)
library(tidyverse)
library(remotes)
remotes::install_github("JohnCoene/echarts4r")
library(echarts4r)
library(glue)
library(lubridate)

# read snapshot data (only last date)
scrape_df <- read_scrape_data(T) 
vera <- read_vera_pop(updated = TRUE)

# filter it and summarise to state-wide
mydata <- scrape_df %>%
  filter(!is.na(Residents.Initiated)) %>%
  mutate(StateShort = translate_state(State, reverse = TRUE)) %>% 
  group_by(StateShort, Date) %>%
  summarise(allInit = sum(Residents.Initiated)) %>%
  # filter(Date != "2021-02-07") %>% # skip day where CA disappeared (why did this happen?)
  left_join(vera, by = c("StateShort" = "State")) %>%
  mutate(percInit = round((allInit / Population) * 100))

mydata %>%
  group_by(Date) %>% #<<
  e_charts(StateShort, timeline = TRUE) %>% #<<
  e_bar(percInit, 
        realtimeSort = TRUE, 
        itemStyle = list(
            borderColor = "black", 
            borderWidth = '1')
  ) %>%
  e_legend(show = FALSE) %>%
  e_flip_coords() %>%
  e_y_axis(inverse = TRUE)  %>%
  e_labels(position = "insideRight", 
           formatter = htmlwidgets::JS("
          function(params){
            return(params.value[0] + '%')
          }
      "),
      ) %>%
  e_timeline_opts(
    autoPlay = TRUE, 
    top = "55",
    label = list(
      formatter = htmlwidgets::JS("
        function(s){
          const d = new Date(s);
          const mo = new Intl.DateTimeFormat('en', { month: 'short' }).format(d);
          const da = new Intl.DateTimeFormat('en', { day: '2-digit' }).format(d);
          return (`${mo}-${da}`);
        }
          ")
        )
      )  %>%
  e_grid(top = 100) %>%
  e_title(paste0("Percent Initiated Covid-19 Vaccine Doses in Prison"), 
          # subtext = "Source: us ! ", 
          # sublink = "https://covid.cdc.gov/covid-data-tracker/#vaccinations", 
          left = "center", 
          top = 10)

rm(list=ls())
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(behindbarstools))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(argparse))

parser <- ArgumentParser()

# specify our desired options
# by default ArgumentParser will add an help option
parser$add_argument(
    "-st", "--start", default = "2021-04-01",
    help="Date to start the active case analysis should be YYYY-MM-DD format")
parser$add_argument(
    "-en", "--end",
    default = as.character(floor_date(Sys.Date(), unit = "month")),
    help="Date to start the active case analysis should be YYYY-MM-DD format")

args <- parser$parse_args()

all_data <- read_scrape_data(all_dates = T)

all_active <- all_data %>%
    # look at all jurisdictions
    # only look at these three variables
    select(Facility.ID, Date, Residents.Active) %>%
    # remove anything with missing data
    na.omit() %>%
    # for each date floor to the nearest month
    mutate(Month = floor_date(Date, unit = "month")) %>%
    # group by each facility and each month
    group_by(Facility.ID, Month) %>%
    # only get the first monthly observation for each facility
    arrange(Facility.ID, Date) %>%
    filter(1:n() == 1) %>%
    # only get data that fall within these date ranges
    filter(Month >= ymd(args$start)) %>%
    filter(Month <= ymd(args$end)) %>%
    # only keep facilities that have 10 months of observations
    group_by(Facility.ID) %>%
    mutate(nmonth = n()) %>%
    ungroup() %>%
    filter(nmonth == max(nmonth))

cat(str_c(
    "There are ", length(unique(all_active$Facility.ID)),
    " facilities in this analysis."))

# of the facilities which have been continuously reporting data on active cases
# here are the number of active cases around the start of the month
plt1 <- all_active %>%
    group_by(Month) %>%
    summarize(Residents.Active = sum(Residents.Active)) %>%
    ggplot(aes(x=Month, y = Residents.Active)) +
    geom_col(fill = "#D7790F", alpha = .95) +
    theme_behindbars() +
    labs(y="Incarcerated People with\nActive Covid Cases")

ggsave("active_case_trends.svg", plt1, width = 10, height = 8)
ggsave("active_case_trends.png", plt1, width = 10, height = 8)

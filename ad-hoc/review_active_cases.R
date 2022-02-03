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
    default = as.character(round_date(Sys.Date(), unit = "day")),
    help="Date to start the active case analysis should be YYYY-MM-DD format")
parser$add_argument(
    "-un", "--unit",
    default = "month",
    help="How to round the data. Should be either 'week' or 'month'")

args <- parser$parse_args()

all_data <- read_scrape_data(all_dates = T)

all_active <- all_data %>%
    # look at all jurisdictions
    # only look at these three variables
    select(Facility.ID, Date, Residents.Active) %>%
    # remove anything with missing data
    na.omit() %>%
    # for each date floor to the nearest month
    mutate(Month = floor_date(Date, unit = args$unit)) %>%
    # group by each facility and each month
    group_by(Facility.ID, Month) %>%
    # only get the first monthly observation for each facility
    arrange(Facility.ID, Date) %>%
    filter(1:n() == 1) %>%
    # only get data that fall within these date ranges
    filter(Date >= ymd(args$start)) %>%
    filter(Date <= ymd(args$end)) %>%
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
    geom_line(color = "#D7790F", size = 1.5) +
    geom_point(color = "#D7790F", size = 3) +
    theme_behindbars() +
    labs(y="Incarcerated People with\nActive Covid Cases") +
    ylim(c(0,NA))

ggsave("active_case_trends.svg", plt1, width = 10, height = 8)
ggsave("active_case_trends.png", plt1, width = 10, height = 8)

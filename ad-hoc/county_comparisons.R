rm(list=ls())
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(behindbarstools))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(argparse))

# get carceral data for the state of Illinois
all_df <- read_scrape_data(TRUE)

parser <- ArgumentParser()

# specify our desired options
# by default ArgumentParser will add an help option
parser$add_argument(
    "-id", "--facid",
    help="The facility id to make for the plot")
parser$add_argument(
    "-alt", "--alttext",
    default = NULL,
    help="The name of the geography to use in the plot.")
parser$add_argument(
    "-st", "--start", default = "2021-10-01",
    help="Date to start the active case analysis should be YYYY-MM-DD format")
parser$add_argument(
    "-en", "--end",
    default = as.character(round_date(Sys.Date(), unit = "day")),
    help="Date to start the active case analysis should be YYYY-MM-DD format")

args <- parser$parse_args()

make_county_plot <- function(fac_id, adf = all_df, sub_name = NULL){
    sub_df <- adf %>%
        filter(Facility.ID == fac_id)

    cname <- ifelse(
        is.null(sub_name),
        str_c(str_to_title(first(sub_df$County), " County")), sub_name)
    cid <- as.numeric(first(sub_df$County.FIPS))
    gen_df <- get_genpop_covid(cid)

    final_plot <- sub_df %>%
        # trim the data down
        select(
            Date, Active = Residents.Active, Pop = Population.Feb20) %>%
        mutate(Group = str_c(cname, "\nJail Population")) %>%
        # pull in the gen population data
        bind_rows(
            gen_df %>%
                arrange(Date) %>%
                mutate(Active = diff_roll_sum(General.Confirmed, Date)) %>%
                select(Date, Active, Pop = General.Population) %>%
                mutate(Group = str_c(cname, "\nOverall Population"))
        ) %>%
        # remove missing values
        arrange(Group, Date) %>%
        na.omit() %>%
        # only look at recent dates
        filter(Date >= ymd(args$start)) %>%
        filter(Date <= ymd(args$end)) %>%
        # calculate rate
        mutate(Rate = Active/Pop * 100000) %>%
        # make a purrty plot
        ggplot(aes(x = Date, y = Rate, color = Group)) +
        geom_line(size = 2) +
        theme_behindbars() +
        scale_color_bbdiscrete() +
        labs(color = "", y = "Active Cases\nPer 100,000")

    ggsave(
        str_c(cname, "_compare_plot.png"),
        final_plot,
        width = 10, height = 6)
}

make_county_plot(as.integer(args$facid), sub_name = args$alttext)

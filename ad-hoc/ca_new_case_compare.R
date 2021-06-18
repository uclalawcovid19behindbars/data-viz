library(tidyverse)
library(behindbarstools)
library(lubridate)
library(plotly)

ca_raw_files <- list_remote_data("raw_files", scraper_name = "california_staff")

ca_staff_case_df <- bind_rows(lapply(ca_raw_files, function(cf){
    page_html <- cf %>%
        xml2::read_html()

    p_date <- page_html %>%
        rvest::html_nodes("p") %>% rvest::html_text() %>%
        str_remove("\\s*\\([^\\)]+\\)") %>%
        .[str_detect(.,"(?i)updated")] %>%
        str_remove("(?i)updated as of ") %>%
        str_remove("(?i)updated ") %>%
        mdy()

    tibble(
        Date = p_date,
        Cases = cf %>%
            xml2::read_html() %>%
            rvest::html_node("table") %>%
            rvest::html_table(trim = 2) %>%
            pull(X2) %>%
            tail(n=1) %>%
            str_remove_all(",") %>%
            as.numeric()
    )})) %>%
    distinct(Date, .keep_all = TRUE) %>%
    mutate(New.Cases = diff_roll_sum(Cases, Date)) %>%
    # total staff population pulled from Vera
    mutate(NCR = New.Cases / 65491)

ca_plot1 <- ca_staff_case_df %>%
    #mutate(NCR = rollmean(NCR, 5, fill = NA)) %>%
    mutate(Name = "CDCR Staff") %>%
    bind_rows(

    "~/Downloads/data_table_for_total_cases__california.csv" %>%
        read_csv(skip = 2) %>%
        mutate(Date = mdy(Date)) %>%
        arrange(Date) %>%
        mutate(New.Cases = diff_roll_sum(`Total Cases`, Date)) %>%
        # total pop from google search of CA
        mutate(NCR = New.Cases / 39510000) %>%
        mutate(Name = "CA Total Population")
    ) %>%
    filter(Date >= ymd("2021-03-15")) %>%
    mutate(Name = fct_rev(Name)) %>%
    ggplot(aes(x=Date, y= NCR*100000, color= Name)) +
    geom_line(size = 2) +
    labs(y="New Case Rate\nPer100,000", color = "") +
    theme_behindbars() +
    scale_color_bbdiscrete()

ca_plot2 <- ca_staff_case_df %>%
    mutate(Name = "CDCR Staff") %>%
    bind_rows(
        # file downloaded from CDC
        "~/Downloads/data_table_for_total_cases__california.csv" %>%
            read_csv(skip = 2) %>%
            mutate(Date = mdy(Date)) %>%
            arrange(Date) %>%
            mutate(New.Cases = diff_roll_sum(`Total Cases`, Date)) %>%
            # total pop from google search of CA
            mutate(NCR = New.Cases / 39510000) %>%
            mutate(Name = "CA Total Population")
    ) %>%
    select(Date, Name, NCR) %>%
    pivot_wider(names_from = "Name", values_from = NCR) %>%
    mutate(Ratio = `CDCR Staff`/`CA Total Population`) %>%
    filter(!is.na(Ratio)) %>%
    mutate(Ratio = rollmean(Ratio, 3, fill = NA)) %>%
    ggplot(aes(x=Date, y= Ratio)) +
    geom_line(size = 2, color = "#D7790F") +
    geom_area(alpha=.5, fill = "#D7790F") +
    theme_behindbars() +
    labs(y="CDCR Staff COVID\nRelative Risk")

ca_plot1
ca_plot2

ggplotly(ca_plot1)

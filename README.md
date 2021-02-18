# Data Viz 

This repository contains the code used by the [UCLA Law COVID-19 Behind Bars Team](https://uclacovidbehindbars.org/)
to create data visualization for social media and other ad-hoc requests. 

**Note**: Only code should be stored here – not the image files themselves. 

## Accessing Data 

#### Latest Data
Our latest data on COVID-19 in carceral facilities is maintained in [our `data` repository](https://github.com/uclalawcovid19behindbars/data), along with a detailed data dictionary. 

#### Post-November Historical Data 
We updated our scraper ETL pipelines in November 2020. As a result, all data post-November is readily available – but data pre-November is only available for certain states at this time. 

We recommend accessing our post-November time series data through an R package that we are developing called [`behindbarstools`](https://github.com/uclalawcovid19behindbars/behindbarstools): 
```
devtools::install_github("uclalawcovid19behindbars/behindbarstools")

data <- behindbarstools::read_scrape_data(all_dates = TRUE, coalesce = TRUE)
```

To access out post-November time series data in Python: 
```
import pandas as pd 

data = pd.read_csv("http://104.131.72.50:3838/scraper_data/summary_data/scraped_time_series.csv")
```

#### Pre-November Historical Data 
Our historical data (pre-November 2020) is available for several states in [our `historical-data` repository](https://github.com/uclalawcovid19behindbars/historical-data/tree/main/data). We are in the process of cleaning this data and will be adding additional states as this data becomes available. 

## Style Guide 

Our R package `behindbarstools` makes it easy to adhere to our visualization style guide through our custom ggplot theme called `theme_behindbars` and custom color scales (`scale_color_bbdiscrete`, `scale_fill_bbdiscrete`, `scale_color_bbcontinous`, and `scale_fill_bbcontinous`). These can be added to an existing ggplot object: 

```
datasets::iris %>%
    ggplot(aes(x = Petal.Width, y = Petal.Length, color = Species)) +
    geom_point() +
    theme_behindbars() +
    scale_color_bbdiscrete()
```

[Our documentation](https://github.com/uclalawcovid19behindbars/behindbarstools) in `behindbarstools` provides additional visualization examples. 


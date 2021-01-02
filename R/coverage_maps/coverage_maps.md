
[![logo](logo.svg)](https://uclacovidbehindbars.org/)

# UCLA Law COVID-19 Behind Bars Data

## Background

The [UCLA Law COVID-19 Behind Bars Data
Project](https://uclacovidbehindbars.org/), launched in March 2020,
tracks the spread and impact of COVID-19 in American carceral facilities
and advocates for greater transparency and accountability around the
pandemic response of the carceral system. Since March, we have been
collecting and reporting facility-level data on COVID-19 in prisons,
jails, and other correctional centers. We also collect information about
pandemic-related prison and jail releases, legal filings and court
orders bearing on the safety of incarcerated people, and grassroots
organizing campaigns and fundraisers – available
[here](https://docs.google.com/spreadsheets/u/2/d/1X6uJkXXS-O6eePLxw2e4JeRtM41uPZ2eRcOA_HkPVTk/edit#gid=1641553906).

## Our Process

Our core dataset includes information on COVID-19 cases, deaths, and
tests across more than 1,500 state, federal, and county facilities. As
of January 2021, we scrape and standardize data from more than 80
sources. We scrape this data 3-4 times each week, although correctional
agencies vary in how often they update their data. Our [scraper
production
code](https://github.com/uclalawcovid19behindbars/covid19_behind_bars_scrapers)
and [more detailed
documentation](https://github.com/uclalawcovid19behindbars/covid19_behind_bars_scrapers)
are available on GitHub.

The majority of the facilities that we collect data on fall under state
jurisdiction, where COVID-19 data is reported on state Department of
Correction websites. We also collect data from federal prisons reported
by the [Federal Bureau of Prisons](https://www.bop.gov/coronavirus/) and
from several large county jail systems – including [Los
Angeles](https://lasd.org/covid19updates/), [New York
City](https://doccs.ny.gov/doccs-covid-19-report),
[Philadelphia](https://www.phila.gov/programs/coronavirus-disease-2019-covid-19/testing-and-data/#/philadelphia-prisons-covid-19-data),
[Maricopa
County](https://www.maricopa.gov/5574/COVID-19-in-County-Jails), [Orange
County](https://ocsheriff.gov/about-ocsd/covid-19/covid-19-oc-jails),
[Cook
County](https://www.cookcountysheriff.org/covid-19-cases-at-ccdoc/), and
[Hennepin
County](https://www.hennepinsheriff.org/jail-warrants/jail-information/COVID-19).

## Number of Facilities by Jurisdiction

<img src="coverage_maps_files/figure-gfm/fac-by-jurisdiction-1.png" style="display: block; margin: auto;" />

We are continuously adding to and refining our scrapers. Where possible,
we have also retrospectively added COVID-19 data for facilities using
digital archives. We are currently in the process of cleaning our
historical scraped data and integrating population data to more readily
compute COVID-19 rates across facilities over the course of the
pandemic. This data is available for several states [on GitHub
here](https://github.com/uclalawcovid19behindbars/historical-data/tree/main/data).

## Our Data

Our core dataset includes the following metrics – reported separately
for incarcerated people and staff at the facility level:

  - Cumulative COVID-19 cases
  - Cumulative COVID-19 deaths
  - Active COVID-19 cases
  - COVID-19 tests administered

Not all jurisdictions report COVID-19 metrics at the facility level.
Some Department of Corrections only report statewide totals, and others
do not report any data for certain metrics. Authorities also vary
dramatically in how they define the metrics that they report. We do our
best to standardize these variables, but comparing data across
jurisdictions and over time should be done with caution.

**Note**: Jurisdictions are continuously updating how, where, and
whether they update their data. We do our best to collect as much data
as possible, but our data availability is subject to change.

### Data Availability for Incarcerated Residents in State Facilities

The maps below summarize the data availability for our four core metrics
among incarcerated people in state facilities. For example,
[California’s Department of
Corrections](https://www.cdcr.ca.gov/covid19/population-status-tracking/)
reports facility-level data on cumulative COVID-19 cases for residents,
while [Florida’s Department of
Corrections](http://www.dc.state.fl.us/comm/covid-19.html) only reports
statewide totals. The [Federal Bureau of
Prisons](https://www.bop.gov/coronavirus/) reports facility-level data
on cumulative cases and deaths, but not active cases or tests
administered.

<img src="coverage_maps_files/figure-gfm/resident-maps-1.png" style="display: block; margin: auto;" />

### Data Availability for Staff in State Facilities

The maps below summarize the data availability for cumulative COVID-19
cases and deaths among staff in state facilities. We try to include data
only for correctional staff who work within facility walls and exclude
administrative staff, who typically do not work on site in prisons and
jails. However, few agencies distinguish between different types of
staff in their reporting. In these cases, we include data for all staff
reported by the agency.

<img src="coverage_maps_files/figure-gfm/staff-maps-1.png" style="display: block; margin: auto;" />

## Directory Structure

## Data Dictionary

## Citations

## License

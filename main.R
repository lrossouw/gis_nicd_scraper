# pull data from json endpoints

# libraries
library(tidyverse)

# clear mem
rm(list = ls())

# fetch data
source("fetch_data_covid_map4.R", echo = TRUE)

# repo
repo_path <- "../covid19za/"
repo <- "origin"
repo_branch <- "master"

# files
file_cases <-
  paste0(repo_path,
         "data/covid19za_provincial_cumulative_timeline_confirmed.csv")
file_deaths <-
  paste0(repo_path,
         "data/covid19za_provincial_cumulative_timeline_deaths.csv")
file_recoveries <-
  paste0(repo_path,
         "data/covid19za_provincial_cumulative_timeline_recoveries.csv")

# get git tools
source("git_tools.R")

print("Updating repo...")
# Checkout the right branch
git_checkout(repo_path, repo_branch)

# Pull latest changes on covid19za
git_pull(repo_path, repo, repo_branch)

print("Loading CSVs...")
# Read in the relevant CSVs
data_cases <- read_csv(
  file_cases,
  col_types = cols(
    date = col_date(format = "%d-%m-%Y"),
    YYYYMMDD = col_date(format = "%Y%m%d"),
    EC = col_integer(),
    FS = col_integer(),
    GP = col_integer(),
    KZN = col_integer(),
    LP = col_integer(),
    MP = col_integer(),
    NC = col_integer(),
    NW = col_integer(),
    WC = col_integer(),
    UNKNOWN = col_integer(),
    total = col_integer(),
    source = col_character()
  )
)

data_deaths <- read_csv(
  file_deaths,
  col_types = cols(
    date = col_date(format = "%d-%m-%Y"),
    YYYYMMDD = col_date(format = "%Y%m%d"),
    EC = col_integer(),
    FS = col_integer(),
    GP = col_integer(),
    KZN = col_integer(),
    LP = col_integer(),
    MP = col_integer(),
    NC = col_integer(),
    NW = col_integer(),
    WC = col_integer(),
    UNKNOWN = col_integer(),
    total = col_integer(),
    source = col_character()
  )
)

data_recoveries <-
  read_csv(
    file_recoveries,
    col_types =
      cols(
        date = col_date(format = "%d-%m-%Y"),
        YYYYMMDD = col_date(format = "%Y%m%d"),
        EC = col_integer(),
        FS = col_integer(),
        GP = col_integer(),
        KZN = col_integer(),
        LP = col_integer(),
        MP = col_integer(),
        NC = col_integer(),
        NW = col_integer(),
        WC = col_integer(),
        UNKNOWN = col_integer(),
        total = col_integer(),
        source = col_character()
      )
  )

# create province mapping
province_map <- data.frame(
  code = c("EC", "FS", "GT", "KZN", "LIM", "MP", "NW", "NC", "WC"),
  province_code = c("EC", "FS", "GP", "KZN", "LP", "MP", "NW", "NC", "WC")
)

# stats totals
totals <-
  statistics_json_data$features$attributes %>%
  rename(
    cases = Confirmed,
    tests = Tests,
    deaths = Death,
    recoveries = Recovered,
    active = Active
  ) %>%
  select(cases, tests, deaths, recoveries, active) %>%
  pivot_longer(
    cols = c("cases", "tests", "deaths", "recoveries", "active"),
    values_to = "total",
    names_to = "type"
  )

# get province data
province_data <-
  province_json_data$features$attributes %>%
  select(CODE, Positive, Recovered, Death) %>%
  rename(
    code = CODE,
    cases = Positive,
    recoveries = Recovered,
    deaths = Death
  ) %>%
  inner_join(province_map, by = "code") %>%
  select(province_code, cases, deaths, recoveries) %>%
  pivot_longer(
    cols = c("cases", "recoveries", "deaths"),
    names_to = "type",
    values_to = "count"
  ) %>%
  pivot_wider(names_from = "province_code", values_from = "count") %>%
  inner_join(totals, by = "type") %>%
  mutate(
    date = report_date,
    YYYYMMDD = report_date,
    UNKNOWN = total - (EC + FS + GP + KZN + LP + MP + NW + NC + WC),
    source = "gis_nicd_scraper"
  )

data_cases <-
  data_cases %>%
  filter(date < report_date) %>%
  bind_rows(province_data %>% filter(type == "cases") %>% select(-type))

data_deaths <-
  data_deaths %>%
  filter(date < report_date) %>%
  bind_rows(province_data %>% filter(type == "deaths") %>% select(-type))

data_recoveries <-
  data_recoveries %>%
  filter(date < report_date) %>%
  bind_rows(province_data %>% filter(type == "recoveries") %>% select(-type))

source("data_checks.R", echo = TRUE)

# run checks
stopifnot(checks(data_cases))
stopifnot(checks(data_deaths))
stopifnot(checks(data_recoveries))

format_dates <- function(data) {
  return(data %>%
           mutate(
             date = format(date, "%d-%m-%Y"),
             YYYYMMDD = format(YYYYMMDD, "%Y%m%d")
           ))
}

# Pull latest changes on covid19za
git_pull(repo_path, repo, repo_branch)

# stop for no
stopifnot(TRUE == FALSE)

# write output
write_csv(
  format_dates(data_cases),
  file = file_cases,
  quote = FALSE,
  na = "",
  col_names = TRUE
)

write_csv(
  format_dates(data_deaths),
  file = file_deaths,
  quote = FALSE,
  na = "",
  col_names = TRUE
)

write_csv(
  format_dates(data_recoveries),
  file = file_recoveries,
  quote = FALSE,
  na = "",
  col_names = TRUE
)

print("Update & push git...")
# git add
git_add(repo_path,
        "data/covid19za_provincial_cumulative_timeline_confirmed.csv")
git_add(repo_path,
        "data/covid19za_provincial_cumulative_timeline_deaths.csv")
git_add(repo_path,
        "data/covid19za_provincial_cumulative_timeline_recoveries.csv")
git_add(repo_path,
        "data/covid19za_timeline_testing.csv")
git_add(repo_path,
        "data/covid19za_timeline_vaccination.csv")

# git commit
git_commit(repo_path,
           paste0(
             "Scrape & update cumulative provincial data for ",
             format(report_date, "%Y-%m-%d"),
             "."
           ))

# Push changes
git_push(repo_path, repo, repo_branch)

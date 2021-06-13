# pull data from json endpoints

# libraries
library(tidyverse)

# clear mem
rm(list = ls())

# fetch data
source("fetch_data_covid_map4.R", echo = TRUE)
source("fetch_data_nmclist.R", echo = TRUE)

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
file_testing <-
  paste0(repo_path,
         "data/covid19za_timeline_testing.csv")

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

data_testing <-
  read_csv(
    file_testing,
    col_types = cols(
      date = col_date(format = "%d-%m-%Y"),
      YYYYMMDD = col_date(format = "%Y%m%d"),
      cumulative_tests = col_integer(),
      cumulative_tests_private = col_integer(),
      cumulative_tests_public = col_integer(),
      recovered = col_integer(),
      hospitalisation = col_integer(),
      critical_icu = col_integer(),
      ventilation = col_integer(),
      deaths = col_integer(),
      contacts_identified = col_integer(),
      contacts_traced = col_integer(),
      scanned_travellers = col_integer(),
      passengers_elevated_temperature = col_integer(),
      covid_suspected_criteria = col_integer(),
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

test_data <-
  data.frame(type = tests_by_sector_json_data$series$name,
             count = tests_by_sector_json_data$series$y) %>%
  pivot_wider(names_from = "type", values_from = "count") %>%
  mutate(date = tests_report_date,
         YYYYMMDD = tests_report_date, ) %>%
  rename(cumulative_tests_private = Private,
         cumulative_tests_public = Public) %>%
  mutate(cumulative_tests = cumulative_tests_private + cumulative_tests_public,
         source = "gis_nicd_scraper")

if (report_date == tests_report_date) {
  test_data$recovered = province_data %>% filter(type == "recoveries") %>% pull(total)
  test_data$deaths = province_data %>% filter(type == "deaths") %>% pull(total)
}

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

data_testing <-
  data_testing %>%
  filter(date < tests_report_date) %>%
  bind_rows(test_data)

source("data_checks.R", echo = TRUE)

# format dates function
format_dates <- function(data) {
  return(data %>%
           mutate(
             date = format(date, "%d-%m-%Y"),
             YYYYMMDD = format(YYYYMMDD, "%Y%m%d")
           ))
}

# Pull latest changes on covid19za
git_pull(repo_path, repo, repo_branch)


# write output (if checks passed)
if (checks(data_cases)) {
  print("data_cases OK!")
} else {
  print("Error in data_cases.")
}
if (checks(data_deaths)) {
  print("data_deaths OK!")
} else {
  print("Error in data_deaths.")
}
if (checks(data_recoveries)) {
  print("data_recoveries OK!")
} else {
  print("Error in data_recoveries.")
}

if (checks_testing(data_testing)) {
  print("data_testing OK")
} else {
  print("Error in data_testing.")
}

# write output (if checks passed)
if (checks(data_cases)) {
  print("Write data_cases...")
  write_csv(
    format_dates(data_cases),
    file = file_cases,
    quote = FALSE,
    na = "",
    col_names = TRUE
  )
}
if (checks(data_deaths)) {
  print("Write data_deaths...")
  write_csv(
    format_dates(data_deaths),
    file = file_deaths,
    quote = FALSE,
    na = "",
    col_names = TRUE
  )
}
if (checks(data_recoveries)) {
  print("Write data_recoveries...")
  write_csv(
    format_dates(data_recoveries),
    file = file_recoveries,
    quote = FALSE,
    na = "",
    col_names = TRUE
  )
}

if (checks_testing(data_testing)) {
  print("Write data_testing...")
  write_csv(
    format_dates(data_testing),
    file = file_testing,
    quote = FALSE,
    na = "",
    col_names = TRUE
  )
}

print("Update & push git...")

# git add data
git_add(repo_path,
        "data/covid19za_provincial_cumulative_timeline_confirmed.csv")
git_add(repo_path,
        "data/covid19za_provincial_cumulative_timeline_deaths.csv")
git_add(repo_path,
        "data/covid19za_provincial_cumulative_timeline_recoveries.csv")

# git commit data
git_commit(repo_path,
           paste0(
             "Scrape & update cumulative provincial data for ",
             format(report_date, "%Y-%m-%d"),
             "."
           ))

# git add testing data
git_add(repo_path,
        "data/covid19za_timeline_testing.csv")

# git commit testing data
git_commit(repo_path,
           paste0(
             "Scrape & update cumulative testing data for ",
             format(tests_report_date, "%Y-%m-%d"),
             "."
           ))

# # git add vaccine data
# git_add(repo_path,
#         "data/covid19za_timeline_vaccination.csv")
# 
# # git commit vaccine data
# git_commit(repo_path,
#            paste0(
#              "Scrape & update cumulative provincial data for ",
#              format(report_date, "%Y-%m-%d"),
#              "."
#            ))

# Push changes
git_push(repo_path, repo, repo_branch)

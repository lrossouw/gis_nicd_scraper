# fetch data

# pull data from json endpoints here:
# https://gis.nicd.ac.za/server/rest/services/Covid_Map4/FeatureServer/

# libraries
library(jsonlite)
library(httr)
library(tidyverse)
library(lubridate)

# get json function
get_json <- function(url, path = "", query = NULL) {
  new_url <- modify_url(url = url,
                        path = path,
                        query = query)
  r <- RETRY(
    verb = "GET",
    url = new_url,
    times = 100,
    quiet = FALSE
  )
  t <- content(r, as = "text")
  return(fromJSON(t))
}

# province data
url = "https://gis.nicd.ac.za/"
path = "server/rest/services/Covid_Map4/FeatureServer/0/query"
query = list(
  f = "json",
  where = "Positive > 0",
  returnGeometry = "false",
  spatialRel = "esriSpatialRelIntersects",
  outFields = "*",
  outSR = 0,
  resultOffset = 0,
  resultRecordCount = 1000
)
province_json_data <- get_json(url, path, query)

# features
url = "https://gis.nicd.ac.za/"
path = "server/rest/services/Covid_Map4/FeatureServer/2"
query = list(f = "json")

features_json_data <- get_json(url, path, query)

# get statistics
url = "https://gis.nicd.ac.za/"
path = "server/rest/services/Covid_Map4/FeatureServer/2/query"
query = list(
  f = "json",
  where = "1=1",
  outFields="*",
  returnGeometry="false"
  )
statistics_json_data <- get_json(url, path, query)

modified_unix_time <-
  max(statistics_json_data$features$attributes$ModifiedDate)
modified_date_time <-
  max(
    as.POSIXct(
      modified_unix_time / 1000 - 60 * 60 * 2,
      origin = '1970-01-01',
      tz = "Africa/Johannesburg"
    )
  )

# if data is released before 18h00 it's probably the previous day's data
report_date <-
  as.Date(modified_date_time, tz = "Africa/Johannesburg") +
  days(if_else(hour(modified_date_time) < 18, -1, 0))

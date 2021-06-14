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

# set v request parameter
v = day(Sys.Date()) + month(Sys.Date()) + year(Sys.Date())

# province case data
url = "https://nmclist.nicd.ac.za/"
path = "App_JSON/DashProvinceData.json"
query = list(v = v)
province_nmclist_json_data <- get_json(url, path, query)

# testing data
url = "https://nmclist.nicd.ac.za/"
path = "App_JSON/DashDistributionTestsBySector.json"
query = list(v = v)
tests_by_sector_json_data <- get_json(url, path, query)

nmclist_date_time <-
  as.POSIXct(tests_by_sector_json_data$start,
             format = "%Y-%m-%d %H:%M:%S",
             tz = "Africa/Johannesburg")

# if data is released before 18h00 it's probably the previous day's data
nmclist_report_date <-
  as.Date(nmclist_date_time, tz = "Africa/Johannesburg") +
  days(if_else(hour(nmclist_date_time) < 18,-1, 0))

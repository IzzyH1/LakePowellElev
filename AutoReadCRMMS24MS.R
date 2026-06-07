library(rvest)
library(stringr)
library(dplyr)
library(lubridate)
library(readr)
##### fAutoReadCRMMS24MS
# Reads most recent 24 month study for Lake Powell and outputs it into data frame

# Inputs: reservoir ("Lake Powell", "Lake Mead"), parameter (Storage, Inflow Volume, Pool Elevation, Release Volume)
# Outputs: dataframe: Most Recent 24MS Results
fAutoReadCRMMS24MS <- function(reservoir, parameter) {
  
  
  # Converts inputs reservoir and data_type into ID for URL
  reservoir_list <- data.frame(name = c("Lake Powell", "Lake Mead"), ID = c(919, 921))
  parameter_list <- data.frame(name = c("Storage", "Inflow Volume", "Release Volume", "Pool Elevation"), ID = c(17,30,43,49))
  
  reservoir_id <- reservoir_list$ID[reservoir == reservoir_list$name]
  parameter_id <- parameter_list$ID[parameter == parameter_list$name]
  

  # Rips all possible CRMMS urls
  nav_url <- "https://www.usbr.gov/uc/water/hydrodata/crmms/current/crmms_nav.html"
  
  hrefs <- read_html(nav_url) |>
    html_elements("a") |>
    html_attr("href")
  
  crmms_links <- hrefs[
    str_detect(hrefs, paste0("/",reservoir_id, "/csv/", parameter_id, "\\.csv$"))
  ]
  
  if(length(crmms_links) == 0)
    stop("No files found")
  
  runs <- str_match(
    crmms_links,
    paste0("([0-9]{1,2})_([0-9]{4})/",reservoir_id, "/csv/", parameter_id, "\\.csv$")
  )
  
  
  candidates <- tibble(
    link = crmms_links,
    month = as.integer(runs[,2]),
    year  = as.integer(runs[,3])
  ) |>
    mutate(
      run_date = make_date(year, month, 1)
    ) |>
    arrange(desc(run_date))
  
  latest_url <- paste0(
    "https://www.usbr.gov/uc/water/hydrodata/crmms/current/",
    candidates$link[1]
  )
  latest_url
  read_csv(latest_url)
}



#I have decided to use the API of New York Open Data
#There are lots of data to choose from here that will meet the 
#requirements of this project

library(httr)
library(jsonlite)
library(tibble)
library(dplyr)

#NYC_airquality_ID <- "https://data.cityofnewyork.us/resource/c3uy-2p5r.json"
#NYC_airquality_info <- httr::GET(NYC_airquality_ID)
#NYC_airquality_parsed <- fromJSON(rawToChar(NYC_airquality_info$content))
#NYC_airquality <- as_tibble(NYC_airquality_parsed)
#NYC_airquality

#NYC_waterquality_ID <- "https://data.cityofnewyork.us/resource/5uug-f49n.json"
#NYC_waterquality_info <- httr::GET(NYC_waterquality_ID)
#NYC_waterquality_parsed <- fromJSON(rawToChar(NYC_waterquality_info$content))
#NYC_waterquality <- as_tibble(NYC_waterquality_parsed)
#NYC_waterquality



#Going to start with Air Quality
airquality_query <- function(pollutant = c("Ozone (O3)", "Nitrogen dioxide (NO2)", "Fine particles (PM 2.5)"),
                             time_period = c("Summer 2023", "Winter 2022-23", "Summer 2022", "Winter 2021-22",
                                             "Summer 2021", "Winter 2020-21", "Summer 2020")){
  airq_url <- "https://data.cityofnewyork.us/resource/c3uy-2p5r.json"
  airq_info <- httr::GET(airq_url, query = list(
    "$where"  = paste0("name = '", pollutant, "' AND ","time_period = '", time_period, "'")
    ))
  airq_data <- fromJSON(rawToChar(airq_info$content))
  as_tibble(airq_data)
}

test1 <- airquality_query(pollutant = "Ozone (O3)", time_period = "Winter 2022-23")



#Making Groups for NYC Harbors
NYC_harbors <- list(
  "Staten Island" = c("K1", "K2", "K3", "K4", "K5", "K5A", "K6"),
  "Hudson" = c("N1", "N3B", "N4", "N5", "N6", "G2", "N7", "N8", "N9", 
               "N16", "NR1"),
  "East River" = c("E2", "E4", "E6", "E7", "E8", "E10", "E11", "E12", 
                   "E13", "E14", "E15"),
  "Jamaica Bay" = c("J1", "J2", "J3", "J5", "J7", "J8", "J9A", "J10", "J11", 
                    "J12", "JA1", "N9A", "J14", "J16"),
  "Harlem River" = "H3",
  "Triathlon" = c("TR1", "TR2", "N3C"),
  "Tributaries" = c("AC1", "AC2", "BB2", "BB4", "BR1", "BR3", "BR5", "CIC2", 
                    "CIC3", "F1", "F5", "FB1", "FLC1", "FLC2", "GB1", "GC3", 
                    "GC4", "GC5", "GC6", "HC1", "HC2", "HC3", "HR1", "HR2", 
                    "HR03", "LN1", "NC0", "NC1", "NC2", "NC3", "PB2", "PB3", 
                    "SP1", "SP2", "WC1", "WC2", "WC3")
)

#Next doing Water Quality function
waterquality_query <- function(weather = c("D", "W"), 
                               harbor = c("Staten Island", "Hudson", "East River", "Jamaica Bay",
                                          "Harlem River", "Triathlon", "Tributaries"),
                               year){
  
  water_code <- NYC_harbors[[harbor]]
  
  start_date <- paste0(year, "-01-01T00:00:00")
  end_date <- paste0(year, "-12-31T23:59:59")
  
  waterq_url <- "https://data.cityofnewyork.us/resource/5uug-f49n.json"
  waterq_info <- httr::GET(waterq_url, query = list(
    "$where" = paste0("sampling_location IN ('", paste(water_code, collapse = "','"), "')", 
                      " AND weather_condition_dry_or_wet = '", weather, "'",
                      " AND sample_date BETWEEN '", start_date, "' AND '", end_date, "'")
  ))
  waterq_data <- fromJSON(rawToChar(waterq_info$content))
  as_tibble(waterq_data) |>
    mutate(year = substr(sample_date, 1, 4))
}

test <- waterquality_query("D", "Triathlon", year = 2004)





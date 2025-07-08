
#Author: Allison McDowell
#Date: July 9, 2025
#Purpose: Code for API and summaries of data


#I have decided to use the API of New York Open Data
#There are lots of data to choose from here that will meet the 
#requirements of this project

library(httr)
library(jsonlite)
library(tibble)
library(dplyr)
library(tidyverse)
library(bslib)
library(ggplot2)
library(ggridges)


#Going to start with Air Quality
airquality_query <- function(pollutant = NULL, year = NULL){
  #We want the user to be able to not specify parameters and get full dataset back
  where_check <- character()
  if(!is.null(pollutant)){
    where_check <- c(where_check, paste0("name IN ('", paste(pollutant, collapse = "','"), "')"))
  }
  if(!is.null(year)){
    start_date <- paste0(year, "-01-01T00:00:00")
    end_date <- paste0(year, "-12-01T00:00:00.000")
    where_check <- c(where_check, paste0("start_date BETWEEN '", start_date, "' AND '", end_date, "'"))
  }
  
  airq_url <- "https://data.cityofnewyork.us/resource/c3uy-2p5r.json"
  airq_info <- httr::GET(airq_url, query = list("$where" = paste(where_check, collapse = " AND ")))
  airq_data <- fromJSON(rawToChar(airq_info$content))
  
  as_tibble(airq_data) |>
    mutate(data_value = as.numeric(data_value))
}

test1 <- airquality_query(pollutant = c("Ozone (O3)", "Nitrogen dioxide (NO2)"), year = 2022)


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
waterquality_query <- function(weather = NULL, harbor = NULL, year = NULL){
  #When investigating data, found there are multiple types of weather documentation
  weather_types <- list(
    "Wet" = c("Wet", "W", "WET"),
    "Dry" = c("Dry", "D")
  )
  
  #Want the user to be able not to specify weather, harbor or year and still obtain results
  where_check <- character()
  if (!is.null(weather)) {
    weather_vals <- unlist(weather_types[weather])
    where_check <- c(where_check, paste0("weather_condition_dry_or_wet IN ('", 
                                         paste(weather_vals, collapse = "','"), "')"))
  }
  if (!is.null(harbor)){
    water_code <- NYC_harbors[harbor] |> unlist()
    where_check <- c(where_check, paste0("sampling_location IN ('",
                                         paste(water_code, collapse = "','"), "')"))
  }
  if (!is.null(year)){
    start_date <- paste0(min(year), "-01-01T00:00:00")
    end_date <- paste0(max(year), "-12-31T23:59:59")
    where_check <- c(where_check, paste0("sample_date BETWEEN '", start_date, "' AND '", end_date, "'"))
  }
  
  waterq_url <- "https://data.cityofnewyork.us/resource/5uug-f49n.json"
  waterq_info <- httr::GET(waterq_url, query = list("$where" = paste(where_check, collapse = " AND ")))
  waterq_data <- fromJSON(rawToChar(waterq_info$content))

  print(str(waterq_data))
  print(names(waterq_data))
  
  waterq_data <- as_tibble(waterq_data) 
  
  waterq_data |>
    filter(!is.na(sample_date)) |>
    mutate(
      year = substr(sample_date, 1, 4),
      harbor = case_when(
        sampling_location %in% c("K1", "K2", "K3", "K4", "K5", "K5A", "K6") ~ "Staten Island",
        sampling_location %in% c("N1", "N3B", "N4", "N5", "N6", "G2", "N7", "N8", "N9", 
                                 "N16", "NR1") ~ "Hudson",
        sampling_location %in% c("E2", "E4", "E6", "E7", "E8", "E10", "E11", "E12", 
                                 "E13", "E14", "E15") ~ "East River",
        sampling_location %in% c("J1", "J2", "J3", "J5", "J7", "J8", "J9A", "J10", "J11", 
                                 "J12", "JA1", "N9A", "J14", "J16") ~ "Jamaica Bay",
        sampling_location %in% c("H3") ~ "Harlem River",
        sampling_location %in% c("TR1", "TR2", "N3C") ~ "Triathlon",
        sampling_location %in% c("AC1", "AC2", "BB2", "BB4", "BR1", "BR3", "BR5", "CIC2", 
                                 "CIC3", "F1", "F5", "FB1", "FLC1", "FLC2", "GB1", "GC3", 
                                 "GC4", "GC5", "GC6", "HC1", "HC2", "HC3", "HR1", "HR2", 
                                 "HR03", "LN1", "NC0", "NC1", "NC2", "NC3", "PB2", "PB3", 
                                 "SP1", "SP2", "WC1", "WC2", "WC3") ~ "Tributaries"
      
      ))
  waterq_data
}

test <- waterquality_query(harbor = c("Harlem River", "Triathlon"), year = 2004)



#Now moving on to making graphical summaries
#I need to make a couple different datasets to work with

#Lets start with some contingency tables
#Air quality
airdata1 <- airquality_query(year = 2022)
table(airdata1$name)
#Ozone is only measured in the summer
airdata2 <- airquality_query(pollutant = "Ozone (O3)", year = 2021)
table(airdata2$geo_type_name)
airdata3 <- airquality_query(pollutant = "Nitrogen dioxide (NO2)", year = 2021)
table(airdata3$geo_type_name)
#Will be useful to compare NO2 and PM 2.5 between summer and winter, or all of them over years

#Water quality
waterdata1 <- waterquality_query(weather = "W", year = 2022)
table(waterdata1$harbor)
waterdata2 <- waterquality_query(weather = "D", harbor = c("Tributaries", "Hudson", "Staten Island"), year = 2011)
table(waterdata2$harbor)
waterdata3 <- waterquality_query(harbor = "Triathlon")
table(waterdata3$year, waterdata3$weather_condition_dry_or_wet)


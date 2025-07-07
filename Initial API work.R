
#I have decided to use the API of New York Open Data
#There are lots of data to choose from here that will meet the 
#requirements of this project

library(httr)
library(jsonlite)
library(tibble)

NYC_airquality_ID <- "https://data.cityofnewyork.us/resource/c3uy-2p5r.json"
NYC_airquality_info <- httr::GET(NYC_airquality_ID)
NYC_airquality_parsed <- fromJSON(rawToChar(NYC_airquality_info$content))
NYC_airquality <- as_tibble(NYC_airquality_parsed)
NYC_airquality

NYC_waterquality_ID <- "https://data.cityofnewyork.us/resource/5uug-f49n.json"
NYC_waterquality_info <- httr::GET(NYC_waterquality_ID)
NYC_waterquality_parsed <- fromJSON(rawToChar(NYC_waterquality_info$content))
NYC_waterquality <- as_tibble(NYC_waterquality_parsed)
NYC_waterquality



#Going to start with Air Quality
airquality_query <- function(pollutant = c("Ozone (O3)", "Nitrogen dioxide (NO2)", "Fine particles (PM 2.5)"),
                             time_period = c("Summer 2023", "Winter 2022", "Summer 2022", "Winter 2021",
                                             "Summer 2021", "Winter 2020", "Summer 2020")){
  airq_url <- "https://data.cityofnewyork.us/resource/c3uy-2p5r.json"
  airq_info <- httr::GET(airq_url, query = list(
    "$where"  = paste0("name = '", pollutant, "' AND ","time_period = '", time_period, "'")
    ))
  airq_data <- fromJSON(rawToChar(airq_info$content))
  as_tibble(airq_data)
}

airquality_query("Ozone (O3)", "Summer 2022")

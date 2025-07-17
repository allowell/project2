
#Required Libraries
library(httr)
library(jsonlite)
library(tibble)
library(dplyr)
library(tidyverse)
library(bslib)
library(ggplot2)
library(ggridges)
library(purrr)
library(stringr)

#Creating shiny app

#Adding in custom functions
#Going to start with Air Quality
#Initializing as NULL to avoid errors, can use this function and select neither
airquality_query <- function(pollutant = NULL, year = NULL){
  #We want the user to be able to not specify parameters and get full dataset back
  
  #I did research into the paste and paste0 functions for this part because
  #my function is pretty complex, I needed the collapse so that each part of
  #the vector became one string as opposed to a vector.
  
  #Initializing empty character vector to avoid errors
  where_check <- character()
  if(!is.null(pollutant)){
    where_check <- c(where_check, paste0("name IN ('", paste(pollutant, collapse = "','"), "')"))
  }
  if(!is.null(year)){
    #Here, the user inputs a year but the format in this dataset is a floating
    #timestamp, so I needed to change the year that is input by the user to
    #this floating timestamp version using paste0
    start_date <- paste0(year, "-01-01T00:00:00")
    end_date <- paste0(year, "-12-01T00:00:00.000")
    where_check <- c(where_check, paste0("start_date BETWEEN '", start_date, "' AND '", end_date, "'"))
  }
  
  airq_url <- "https://data.cityofnewyork.us/resource/c3uy-2p5r.json"
  
  #From outside API research, found I can do query = list("$where") along with
  #other inputs to query the API, so I used that here.
  #Separating the different results from pollutant and year with AND
  #so that the query works properly, since this is the format required
  airq_info <- httr::GET(airq_url, query = list("$where" = paste(where_check, collapse = " AND ")))
  airq_data <- fromJSON(rawToChar(airq_info$content))
  
  as_tibble(airq_data) |>
    #Found later on when doing graphs that data_value wasn't numeric, changing that here
    mutate(data_value = as.numeric(data_value),
           #Adding easy to work with year by taking first 4 characters of floating timestamp
           year = substr(start_date, 1, 4))
}

#Next, moving on to the second dataset

#This one is water, and contains a PDF of the codes and corresponding harbors. I am
#going to make groups because I think this could be an interesting variable

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



#Next, it's time to write the Water Quality function
#Again initializing as NULL
waterquality_query <- function(weather = NULL, harbor = NULL, year = NULL){
  #When investigating data, found there are multiple types of weather documentation
  #so I'm putting them all in this list to make sure they're all included/no issues
  #and I need the general names Wet and Dry
  weather_types <- list(
    "Wet" = c("Wet", "W", "WET"),
    "Dry" = c("Dry", "D")
  )
  
  where_check <- character()
  if (!is.null(weather)) {
    #weather_vals needs to be a vector, but right now it's a list
    #ran into many errors with this, found that I need to use the unlist function
    #to keep the general names Wet and Dry but change the list to a vector.
    weather_vals <- unlist(weather_types[weather])
    where_check <- c(where_check, paste0("weather_condition_dry_or_wet IN ('", 
                                         paste(weather_vals, collapse = "','"), "')"))
  }
  if (!is.null(harbor)){
    #Allowing users to select a vector of harbors
    water_code <- NYC_harbors[harbor]
    #Need to unlist again to make it a vector
    water_code <- unlist(water_code)
    where_check <- c(where_check, paste0("sampling_location IN ('",
                                         paste(water_code, collapse = "','"), "')"))
  }
  if (!is.null(year)){
    #Once again, I need the year in floating timestamp format
    start_date <- paste0(min(year), "-01-01T00:00:00")
    end_date <- paste0(max(year), "-12-31T23:59:59")
    where_check <- c(where_check, paste0("sample_date BETWEEN '", start_date, "' AND '", end_date, "'"))
  }
  
  waterq_url <- "https://data.cityofnewyork.us/resource/5uug-f49n.json"
  #Using query = again for this
  #Again using collapse = AND so that each part (year, harbor and weather)
  #separated with AND which is what the query requires to work correctly
  waterq_info <- httr::GET(waterq_url, query = list("$where" = paste(where_check, collapse = " AND ")))
  waterq_data <- fromJSON(rawToChar(waterq_info$content))
  
  waterq_data <- as_tibble(waterq_data) 
  
  waterq_data <- waterq_data |>
    #Cleaning up the data because it's so large. removing difficult variables
    select(-c("wind_speed_mph", "wind_direction_wind_direction", "sea_state", "type",
              "current_direction_current_direction")) |>
    #Removing dates that are NA to avoid errors
    filter(!is.na(sample_date)) |>
    mutate(
      #Adding new easy-to-use variable called year by getting first 4 characters
      #of the floating timestamp date
      year = substr(sample_date, 1, 4),
      #Creating new variable called harbor, following key outlined in PDF
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
  
  #Changing all numeric data to numeric. Right now everything is in character (ugh)
  #Selecting columns I want to change to numeric
  waterq_data_numeric <- waterq_data |>
    select(-sampling_location, -sample_date, -sample_time, -weather_condition_dry_or_wet, 
           -year, -harbor) |>
    map(as.numeric) |>
    as_tibble()
  #Selecting character columns
  waterq_data_character <- waterq_data |>
    select(sampling_location, sample_date, sample_time, weather_condition_dry_or_wet,
           year, harbor)
  
  #Return final data by binding character and numeric columns
  #This is also nicer now because character is first then all numeric
  waterq_data_final <- bind_cols(waterq_data_character, waterq_data_numeric)
  waterq_data_final
  
}





#User Interface
#Doing a fluid page
ui <- page_fluid(
  titlePanel("New York City Water and Air Quality Investigation"),
  
  #Creating tabs for app
  navset_card_underline(
    
    #First tab, the about section
    nav_panel("About", markdown(glue::glue(
      "
      The purpose of this app is to explore the air quality and water quality in New York City. Users
      can select which dataset they would like to work with, select variables of interest, download
      that data, and explore numerous summary statistics and graphical summaries of the data.
      
      The data was obtained from the NYC Open Data Source, which contains numerous datasets relating to New
      York City. For more information about where this data came from, and to explore the other 
      datasets avaliable,please visit [NYC Open Source Data](https://opendata.cityofnewyork.us/)
      
      There are 3 tabs on this app. The first page, here, is the About page, containing some necessary
      information about this app. The next page, Data Download, will allows users to return data by
      specifying changes to the API functions that I have written for this project and save those datasets.
      Users can also subset the data based on variables of interest and download desired data.
      Lastly, the Data Exploration page will allow users to explore various numeric and graphical summaries.
      
      Thanks for visiting my app!
      ")),
      #Adding in the image
      tags$img(height = 400, width = 700, 
               src = "nyc_pic.jpg")),
    
    
    #Next tab, the data download
    nav_panel("Data Download", sidebarLayout(
      sidebarPanel(
        #Select which dataset to use, Air or Water
        selectInput("data_select", "Select Dataset",
                    choices = c("Air Quality", "Water Quality")),
        
        #Conditional based on Air Quality
        #Choose the year of interest
        conditionalPanel(condition = "input.data_select == 'Air Quality'",
                         numericInput("year_air", "Year", value = 2020, min = 2005, max = 2023)),
        #Choose the pollutant(s) of interest
        conditionalPanel(condition = "input.data_select == 'Air Quality'",
                         checkboxGroupInput("pollutant", "Pollutant", 
                                            choices = c("Ozone (O3)", "Nitrogen dioxide (NO2)", 
                                                        "Fine particles (PM 2.5)"))),
        
        #Conditional based on Water Quality
        #Select range of years using slider
        conditionalPanel(condition = "input.data_select == 'Water Quality'",
                         sliderInput("year_water", "Year", value = c(2010, 2020), min = 1909, max = 2023),
                         step = 1),
        #Choose weather condition
        conditionalPanel(condition = "input.data_select == 'Water Quality'",
                         checkboxGroupInput("weather", "Weather", 
                                            choices = c("Wet", "Dry"))),
        #Choose which harbor station(s)
        conditionalPanel(condition = "input.data_select == 'Water Quality'",
                         checkboxGroupInput("harbor", "Harbor Station", 
                                            choices = c("Staten Island", "Hudson", "East River", "Jamaica Bay",
                                                        "Harlem River", "Triathlon", "Tributaries"))),
        #Get the data based on above conditions
        actionButton("get", "Get Data"),
        #UI output, shows up after data retrieved allowing user to select which columns
        uiOutput("cols_select"),
        #Download button to download data
        downloadButton("download", "Download Data")
      ),
      #Only thing on the main part of this tab is a data table
      mainPanel(
        dataTableOutput("api_data")
      )
    )),
    
    
    #Next tab, the data exploration
    nav_panel("Data Exploration", sidebarLayout(
      #User selects which summary type and plot type they want
      sidebarPanel(
        selectInput("summary_type", "Summary Type",
                    choices = c("Contingency Table", "Summary Statistics")),
        selectInput("plot_type", "Plot Type",
                    choices = c("Histogram", "Scatterplot", "Boxplot", "Ridgeline")),
        #UI outputs for the variables. Different requirements depending on summary or plot
        #So I separated them. 2 available for summary, 3 for plots (x,y,color) and also
        #option for facet variable. Also including question for if they want to do facetwrap
        uiOutput("sum_var1"),
        uiOutput("sum_var2"),
        uiOutput("plot_var1"),
        uiOutput("plot_var2"),
        uiOutput("plot_var3"),
        uiOutput("facet_q"),
        uiOutput("facet_var")
      ),
      #Main panel has a table and plot
      mainPanel(
        tableOutput("tableoutput"),
        plotOutput("plotoutput")
      )
    ))
    )
)


#Now moving on to the server
server <- function(input, output, session){
  
  #Data Download tab
  #Get data based on which they want (air or water) and corresponding variables
  api_data <- eventReactive(input$get, {
    if(input$data_select == "Air Quality"){
      airquality_query(pollutant = input$pollutant, year = input$year_air)
    }
    else if(input$data_select == "Water Quality"){
      waterquality_query(weather = input$weather, harbor = input$harbor, year = input$year_water)
    }
  })
  
  #Allowing the user to select the columns that they want
  output$cols_select <- renderUI({
    #requiring data
    req(api_data())
    #no columns selected initially to avoid user having to uncheck everything
    #if they are only interested in select few
    checkboxGroupInput("select_cols", "Select Columns", 
                       choices = names(api_data()))
  })
  
  #Filter final data reactively once user has selected columns
  final_data <- reactive({
    req(api_data(), input$select_cols)
    api_data()[ , input$select_cols]
  })
  
  #Making data table to view for data download tab
  output$api_data <- renderDataTable({
    final_data()
  })
  
  #Allowing download of data
  output$download <- downloadHandler(
    filename = function(){
      #extracting first word of data selected (Air or Water) using stringr
      paste0("nyc_data_", stringr::word(input$data_select, 1), ".csv")
    },
    content = function(file){
      write_csv(final_data(), file)
    }
  )
  
  
  #Data Exploration tab
  #Selecting which variables are going to be looked at
  #All of these are reactive to final_data, so it takes a minute to load into the tab
  output$sum_var1 <- renderUI({
    req(final_data())
    selectInput("sum_var1", "Variable 1 - Summary", choices = names(final_data()))
  })
  output$sum_var2 <- renderUI({
    req(final_data())
    selectInput("sum_var2", "Variable 2 - Summary 
                (if Summary Statistics chosen, this must be a numeric variable!) ", 
                choices = names(final_data()))
  })
  output$plot_var1 <- renderUI({
    req(final_data())
    selectInput("plot_var1", "Variable 1 - Plot", choices = names(final_data()))
  })
  output$plot_var2 <- renderUI({
    req(final_data())
    #Only allowing this one to be numeric using sapply function
    numeric <- names(final_data())[sapply(final_data(), is.numeric)]
    selectInput("plot_var2", "Variable 2 - Plot (must be numeric)", choices = numeric)
  })
  output$plot_var3 <- renderUI({
    req(final_data())
    #Requiring that this variable be character so that they can be color coded
    character <- names(final_data())[sapply(final_data(), is.character)]
    selectInput("plot_var3", "Variable 3 - Plot (color)", choices = character)
  })
  #Making a button for user to select if they want to facet wrap or not, default is no
  output$facet_q <- renderUI({
    req(final_data())
    radioButtons("facet_q", "Facet this Plot?", choices = c("Yes", "No"), selected = "No")
  })
  #If they want to facet wrap, adding the code to pick which variable
  output$facet_var <- renderUI({
    req(final_data())
    character <- names(final_data())[sapply(final_data(), is.character)]
    if(input$facet_q == "Yes"){
      selectInput("facet_var", "Facet Variable", choices = character)
    }
  })
  
  #Now moving on to the visualizations
  #First the tables
  output$tableoutput <- renderTable({
    req(final_data())
    #Contingency table
    if(input$summary_type == "Contingency Table"){
      return(table(final_data()[[input$sum_var1]], 
                   final_data()[[input$sum_var2]]))
    }
    if(input$summary_type == "Summary Statistics"){
      final_data() |>
        #I was having issues with my code here and did research that I might 
        #need to do .data instead which is why I did so here and it worked
        group_by(.data[[input$sum_var1]]) |>
        summarise(
          "Mean" = mean(as.numeric(.data[[input$sum_var2]]), na.rm = TRUE),
          "Median" = median(as.numeric(.data[[input$sum_var2]]), na.rm = TRUE),
          "Standard Deviation" = sd(as.numeric(.data[[input$sum_var2]]), na.rm = TRUE),
          "N" = sum(!is.na(as.numeric(.data[[input$sum_var2]])))
        )
    }
  })
  
  #Next the plots
  output$plotoutput <- renderPlot({
    
    #Histogram
    if(input$plot_type == "Histogram"){
      plot <- ggplot(final_data(), aes(x = final_data()[[input$plot_var1]],
                                       fill = "red")) +
        geom_histogram(stat = "count") +
        labs(title = "Histogram of Selected Plot Variable 1 Counts", 
             x = input$plot_var1)
        
    }
    #Scatterplot
    else if(input$plot_type == "Scatterplot"){
      plot <- ggplot(final_data(), aes(x = final_data()[[input$plot_var1]], 
                                       y = final_data()[[input$plot_var2]],
                                       color = final_data()[[input$plot_var3]])) +
        geom_point() +
        labs(title = "Scatterplot of Selected Plot Variables 1 and 2", 
             x = input$plot_var1,
             y = input$plot_var2,
             color = input$plot_var3)
    }
    #Boxplot
    else if(input$plot_type == "Boxplot"){
      plot <- ggplot(final_data(), aes(x = final_data()[[input$plot_var1]],
                                       y = final_data()[[input$plot_var2]],
                                       fill = "red")) +
        geom_boxplot() +
        labs(title = "Boxplot of Selected Plot Variables 1 and 2", 
             x = input$plot_var1,
             y = input$plot_var2)
    }
    #Ridgeline
    else if(input$plot_type == "Ridgeline"){
      plot <- ggplot(final_data(), aes(x = final_data()[[input$plot_var2]],
                                       y = final_data()[[input$plot_var1]],
                                       fill = final_data()[[input$plot_var1]])) +
        geom_density_ridges() +
        labs(title = "Ridgeline Plot of Selected Plot Variables 1 and 2", 
             x = input$plot_var2,
             y = input$plot_var1)
    }
    
    #Adding facetwrap on to end of selected plot code if user wants to do so
    if(input$facet_q == "Yes"){
      plot <- plot + facet_wrap(~ final_data()[[input$facet_var]])
    }
    
    #Returning the plot at the end!
    plot
    
  })

  
}

#All done, now running the app!
shinyApp(ui = ui, server = server)





#Required Libraries
library(httr)
library(jsonlite)
library(tibble)
library(dplyr)
library(tidyverse)
library(bslib)

#Creating shiny app


#User Interface
ui <- page_fluid(
  titlePanel("New York City Water and Air Quality Investigation"),
  
  #Creating tabs for app
  navset_card_underline(
    
    #First tab, the about section
    nav_panel("About", markdown(glue::glue(
      "
      The purpose of this app is to explore the air quality and water quality in New York City.
      
      The data was obtained from the NYC Open Data Source, which contains numerous datasets relating to New
      York City. For more information about where this data came from, please visit 
      [NYC Open Source Data](https://opendata.cityofnewyork.us/)
      
      There are 3 pages on this app. The first page, here, is the About page, containing some necessary
      information about this app. The next page, Data Download, will allow users to return data by
      specifying changes to the API functions that I have written for this project and save those datasets.
      Lastly, the Data Exploration page will allow users to explore various numeric and graphical summaries.
      
      Thanks for visiting my app!
      ")),
      #Adding in the image
      tags$img(height = 400, width = 700, 
               src = "nyc_pic.jpg")),
    
    #Next tab, the data download
    nav_panel("Data Download", sidebarLayout(
      sidebarPanel(
        selectInput("data_select", "Select Dataset",
                    choices = c("Air Quality", "Water Quality")),
        numericInput("year", "Year", value = 2020, min = 2000, max = 2023),
        checkboxGroupInput("pollutant", "Pollutant for Air Quality", 
                    choices = c("Ozone (O3)", "Nitrogen dioxide (NO2)", "Fine particles (PM 2.5)")),
        checkboxGroupInput("weather", "Weather for Water Quality", 
                    choices = c("Wet", "Dry")),
        checkboxGroupInput("harbor", "Harbor Station for Water Quality", 
                    choices = c("Staten Island", "Hudson", "East River", "Jamaica Bay",
                                "Harlem River", "Triathlon", "Tributaries")),
        uiOutput("cols_select"),
        actionButton("get", "Get Data"),
        downloadButton("download", "Download Data")
      ),
      mainPanel(
        dataTableOutput("api_data")
      )
    )),
    
    #Next tab, the data exploration
    nav_panel("Data Exploration", sidebarLayout(
      sidebarPanel(
        selectInput("plot_type", "Plot Type", 
                    choices = c("Scatterplot", "Histogram", "Boxplot", "Heatmap")),
        selectInput("summary_type", "Summary Type",
                    choices = c("Contingency Table", "Summary Statistics"))
      ),
      mainPanel(
        plotOutput("plotoutput"),
        tableOutput("tableoutput")
      )
    ))
    )
)


#Server
server <- function(input, output, session){
  
  #Get data based on which they want (air or water)
  api_data <- eventReactive(input$get, {
    if(input$data_select == "Air Quality"){
      airquality_query(pollutant = input$pollutant, year = input$year)
    }
    else if(input$data_select == "Water Quality"){
      waterquality_query(weather = input$weather, harbor = input$harbor, year = input$year)
    }
  })
  
  #Selecting columns
  output$cols_select <- renderUI({
    req(api_data())
    checkboxGroupInput("select_cols", "Select Columns", 
                       choices = names(api_data()), selected = names(api_data()))
  })
  
  #Filter final data
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
      paste0("nyc_data_", stringr::word(input$data_select, 1), ".csv")
    },
    content = function(file){
      write_csv(api_data(), file)
    }
  )
  
  #Next working on the Data exploration tab
  #
  
}

shinyApp(ui = ui, server = server)




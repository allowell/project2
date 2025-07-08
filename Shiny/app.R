

library(httr)
library(jsonlite)
library(tibble)
library(dplyr)
library(tidyverse)
library(bslib)

#Creating shiny app


#User Interface
ui <- fluidPage(
  titlePanel("New York City Water and Air Quality Investigation"),
  navset_card_underline(
    nav_panel("About", tags$img(height = 100, width = 300, 
                                src = "https://upload.wikimedia.org/wikipedia/commons/thumb/d/d6/Manhattan_skyline_from_Jersey_City.jpg/640px-Manhattan_skyline_from_Jersey_City.jpg"), 
              markdown(glue::glue(
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
      
      "))
      ),
    nav_panel("Data Download", sidebarLayout(
      sidebarPanel(
        selectInput("data_select", "Select Dataset",
                    choices = c("Air Quality", "Water Quality")),
        numericInput("year", "Year", value = 2020, min = 2000, max = 2023),
        selectInput("pollutant", "Pollutant for Air Quality", 
                    choices = c("Ozone (O3)", "Nitrogen dioxide (NO2)", "Fine particles (PM 2.5)")),
        selectInput("weather", "Weather for Water Quality", 
                    choices = c("Wet", "Dry")),
        selectInput("harbor", "Harbor Station for Water Quality", 
                    choices = c("Staten Island", "Hudson", "East River", "Jamaica Bay",
                                "Harlem River", "Triathlon", "Tributaries")),
        actionButton("get", "Get Data"),
        downloadButton("download", "Download Data")
      ),
      mainPanel(
        dataTableOutput("api_data")
      )
    )),
    nav_panel("Data Exploration", sidebarLayout(
      sidebarPanel(
        selectInput("plot_type", "Plot Type", 
                    choices = c("Scatterplot", "Histogram")),
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
  api_data <- eventReactive(input$get, {
    if(input$data_select == "Air Quality"){
      req(input$pollutant, input$year)
      airquality_query(pollutant = input$pollutant, year = input$year)
    }
    else if(input$data_select == "Water Quality"){
      req(input$weather, input$year, input$harbor)
      waterquality_query(weather = input$weather, harbor = input$harbor, year = input$year)
    }
  })
  
  output$api_data <- renderDataTable({
    api_data()
  })
  
  output$download <- downloadHandler(
    filename = function(){
      paste0("nyc_data_", stringr::word(input$data_select, 1), ".csv")
    },
    content = function(file){
      write_csv(api_data(), file)
    }
  )
  
}

shinyApp(ui = ui, server = server)




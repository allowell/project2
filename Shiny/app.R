
#Required Libraries
library(httr)
library(jsonlite)
library(tibble)
library(dplyr)
library(tidyverse)
library(bslib)
library(ggplot2)
library(ggridges)
library(stringr)

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
        
        #Conditional based on Air Quality
        conditionalPanel(condition = "input.data_select == 'Air Quality'",
                         numericInput("year_air", "Year", value = 2020, min = 2020, max = 2023)),
        conditionalPanel(condition = "input.data_select == 'Air Quality'",
                         checkboxGroupInput("pollutant", "Pollutant for Air Quality", 
                                            choices = c("Ozone (O3)", "Nitrogen dioxide (NO2)", 
                                                        "Fine particles (PM 2.5)"))),
        conditionalPanel(condition = "input.data_select == 'Water Quality'",
                         sliderInput("year_water", "Year", value = c(2018, 2020), min = 2000, max = 2023),
                         step = 1),
        conditionalPanel(condition = "input.data_select == 'Water Quality'",
                         checkboxGroupInput("weather", "Weather for Water Quality", 
                                            choices = c("Wet", "Dry"))),
        conditionalPanel(condition = "input.data_select == 'Water Quality'",
                         checkboxGroupInput("harbor", "Harbor Station for Water Quality", 
                                            choices = c("Staten Island", "Hudson", "East River", "Jamaica Bay",
                                                        "Harlem River", "Triathlon", "Tributaries"))),
        actionButton("get", "Get Data"),
        uiOutput("cols_select"),
        downloadButton("download", "Download Data")
      ),
      mainPanel(
        dataTableOutput("api_data")
      )
    )),
    
    #Next tab, the data exploration
    nav_panel("Data Exploration", sidebarLayout(
      sidebarPanel(
        selectInput("summary_type", "Summary Type",
                    choices = c("Contingency Table", "Summary Statistics")),
        selectInput("plot_type", "Plot Type",
                    choices = c("Histogram", "Scatterplot", "Boxplot", "Ridgeline")),
        uiOutput("sum_var1"),
        uiOutput("sum_var2"),
        uiOutput("plot_var1"),
        uiOutput("plot_var2"),
        uiOutput("plot_var3"),
        uiOutput("facet_q"),
        uiOutput("facet_var")
      ),
      mainPanel(
        tableOutput("tableoutput"),
        plotOutput("plotoutput")
      )
    ))
    )
)


#Server
server <- function(input, output, session){
  
  #Get data based on which they want (air or water)
  api_data <- eventReactive(input$get, {
    if(input$data_select == "Air Quality"){
      airquality_query(pollutant = input$pollutant, year = input$year_air)
    }
    else if(input$data_select == "Water Quality"){
      waterquality_query(weather = input$weather, harbor = input$harbor, year = input$year_water)
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
      #extracting first word of data selected (Air or Water) using stringr
      paste0("nyc_data_", stringr::word(input$data_select, 1), ".csv")
    },
    content = function(file){
      write_csv(final_data(), file)
    }
  )
  
  #Next working on the Data exploration tab
  
  #Selecting which variables are going to be looked at 
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
    numeric <- names(final_data())[sapply(final_data(), is.numeric)]
    selectInput("plot_var2", "Variable 2 - Plot (must be numeric)", choices = numeric)
  })
  output$plot_var3 <- renderUI({
    req(final_data())
    character <- names(final_data())[sapply(final_data(), is.character)]
    selectInput("plot_var3", "Variable 3 - Plot (color)", choices = character)
  })
  output$facet_q <- renderUI({
    req(final_data())
    radioButtons("facet_q", "Facet this Plot?", choices = c("Yes", "No"), selected = "No")
  })
  output$facet_var <- renderUI({
    req(final_data())
    character <- names(final_data())[sapply(final_data(), is.character)]
    if(input$facet_q == "Yes"){
      selectInput("facet_var", "Facet Variable", choices = character)
    }
  })
  
  
  #First the tables
  output$tableoutput <- renderTable({
    req(final_data(), input$sum_var1, input$sum_var2)
    #Contingency table
    if(input$summary_type == "Contingency Table"){
      return(table(as.factor(final_data()[[input$sum_var1]]), 
                          as.factor(final_data()[[input$sum_var2]])))
      print(input$sum_var1)
      print(input$sum_var2)
    }
    if(input$summary_type == "Summary Statistics"){
      final_data() |>
        #I was having issues with my code here and did research that I might 
        #need to do .data instead which is why I did so here and it worked
        group_by(.data[[input$sum_var1]]) |>
        summarise(
          "Mean" = mean(.data[[input$sum_var2]], na.rm = TRUE),
          "Median" = median(.data[[input$sum_var2]], na.rm = TRUE),
          "Standard Deviation" = sd(.data[[input$sum_var2]], na.rm = TRUE),
          "N" = n()
        )
    }
  })
  
  #Next the plots
  output$plotoutput <- renderPlot({
    
    #Histogram
    if(input$plot_type == "Histogram"){
      plot <- ggplot(final_data(), aes(x = final_data()[[input$plot_var1]])) +
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
                                       y = final_data()[[input$plot_var2]])) +
        geom_boxplot() +
        labs(title = "Boxplot of Selected Plot Variables 1 and 2", 
             x = input$plot_var1,
             y = input$plot_var2)
    }
    #Heatmap
    else if(input$plot_type == "Ridgeline"){
      plot <- ggplot(final_data(), aes(x = final_data()[[input$plot_var2]],
                                       y = final_data()[[input$plot_var1]])) +
        geom_density_ridges() +
        labs(title = "Ridgeline Plot of Selected Plot Variables 1 and 2", 
             x = input$plot_var2,
             y = input$plot_var1)
    }
    
    if(input$facet_q == "Yes"){
      plot <- plot + facet_wrap(~ final_data()[[input$facet_var]])
    }
    
    plot
    
  })

  
}

shinyApp(ui = ui, server = server)




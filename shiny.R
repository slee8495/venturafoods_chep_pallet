# Load necessary libraries
library(shiny)
library(shinythemes)
library(DT)
library(tidyverse)
library(lubridate)
library(readxl)

# Source your functions file
source("functions.R")

# Define common DT options
DTOptions <- list(
  pageLength = 100,
  scrollX = TRUE,
  dom = 'Blfrtip',
  buttons = c('copy', 'csv', 'excel'),
  fixedColumns = list(leftColumns = 2),
  rownames = FALSE
)

# UI 
ui <- navbarPage(
  theme = shinythemes::shinytheme("flatly"),
  "CHEP Pallet Reconciliation",
  tabPanel("Data Upload",
           sidebarLayout(
             sidebarPanel(
               fileInput("jde_file", "Upload JDE file (CSV format)", accept = c(".csv")),
               fileInput("as400_file", "Upload AS400 file (CSV format)", accept = c(".csv")),
               fileInput("chep_file", "Upload CHEP file (CSV format)", accept = c(".csv"))
             ),
             mainPanel()
           )
  ),
  navbarMenu("Data Cleaning Automation",
             tabPanel("JDE", 
                      DTOutput("jde_table"),
                      downloadButton("download_jde", "Download JDE Data")),
             tabPanel("AS400", 
                      DTOutput("as400_table"),
                      downloadButton("download_as400", "Download Legacy Data")),
             tabPanel("CHEP", 
                      DTOutput("chep_table"),
                      downloadButton("download_chep", "Download CHEP Data"))
  ),
  navbarMenu("CHEP x Ventura",
             tabPanel("CHEP vs AS400",
                      tabsetPanel(
                        tabPanel("Based on CHEP Data", 
                                 selectInput("filter_chep_as400_based_on_chep", "Filter by Match:", choices = c("All", "Y", "N")),
                                 DTOutput("chep_as400_based_on_chep_table"),
                                 downloadButton("download_compared_1", "Download")),
                        tabPanel("Based on AS400 Data", 
                                 selectInput("filter_chep_as400_based_on_as400", "Filter by Match:", choices = c("All", "Y", "N")),
                                 DTOutput("chep_as400_based_on_as400_table"),
                                 downloadButton("download_compared_2", "Download"))
                      )
             ),
             tabPanel("CHEP vs JDE",
                      tabsetPanel(
                        tabPanel("Based on CHEP Data", 
                                 selectInput("filter_chep_jde_based_on_chep", "Filter by Match:", choices = c("All", "Y", "N")),
                                 DTOutput("chep_jde_based_on_chep_table"),
                                 downloadButton("download_compared_3", "Download")),
                        tabPanel("Based on JDE Data", 
                                 selectInput("filter_chep_jde_based_on_jde", "Filter by Match:", choices = c("All", "Y", "N")),
                                 DTOutput("chep_jde_based_on_jde_table"),
                                 downloadButton("download_compared_4", "Download"))
                      )
             )
  ),
  windowTitle = "CHEP Pallet Reconciliation",
  header = tagList(
    div(class = "navbar-header pull-right", img(src = "VenturaFoodsLogo.png", height = "70px"))
  )
)





options(shiny.maxRequestSize = 50 * 1024^2)

# Server logic
server <- function(input, output, session) {
  cleaned_jde_data <- reactiveVal()
  cleaned_as400_data <- reactiveVal()
  cleaned_chep_data <- reactiveVal()
  
  # JDE File Processing
  observeEvent(input$jde_file, {
    req(input$jde_file)
    jde_data <- read_csv(input$jde_file$datapath)
    cleaned_jde_data(jde_cleaning(jde_data))
    output$jde_table <- renderDT({
      datatable(cleaned_jde_data(), extensions = "Buttons", options = DTOptions, rownames = FALSE)
    })
  })
  
  # AS400 File Processing
  observeEvent(input$as400_file, {
    req(input$as400_file)
    as400_data <- read_csv(input$as400_file$datapath)
    cleaned_as400_data(as400_cleaning(as400_data))
    output$as400_table <- renderDT({
      datatable(cleaned_as400_data(), extensions = "Buttons", options = DTOptions, rownames = FALSE)
    })
  })
  
  # CHEP File Processing
  observeEvent(input$chep_file, {
    req(input$chep_file)
    chep_data <- read_csv(input$chep_file$datapath)
    cleaned_chep_data(chep_cleaning(chep_data))
    output$chep_table <- renderDT({
      datatable(cleaned_chep_data(), extensions = "Buttons", options = DTOptions, rownames = FALSE)
    })
  })
  
  # Function to filter data based on Match column
  filter_data <- function(data, input_filter) {
    if (input_filter != "All") {
      return(data %>% filter(Match == input_filter))
    }
    return(data)
  }
  
  # Apply styles to the datatable
  style_table <- function(data_table, columns_colors) {
    for (col in names(columns_colors)) {
      data_table <- data_table %>% 
        formatStyle(columns = col, backgroundColor = columns_colors[[col]], target = "cell")
    }
    return(data_table)
  }
  
  # CHEP vs AS400 based on CHEP Data
  output$chep_as400_based_on_chep_table <- renderDT({
    req(cleaned_chep_data(), cleaned_as400_data())
    data <- chep_as400_based_on_chep(cleaned_chep_data(), cleaned_as400_data())
    filtered_data <- filter_data(data, input$filter_chep_as400_based_on_chep)
    style_table(datatable(filtered_data, extensions = "Buttons", options = DTOptions, rownames = FALSE), 
                c("Plt Qty (CHEP)" = "lightcoral", "Plt Qty (Legacy)" = "lightblue", "Plt Qty (CHEP) - Plt Qty (Legacy)" = "lightgreen"))
  })
  
  # CHEP vs AS400 based on AS400 Data
  output$chep_as400_based_on_as400_table <- renderDT({
    req(cleaned_chep_data(), cleaned_as400_data())
    data <- chep_as400_based_on_as400(cleaned_as400_data(), cleaned_chep_data())
    filtered_data <- filter_data(data, input$filter_chep_as400_based_on_as400)
    style_table(datatable(filtered_data, extensions = "Buttons", options = DTOptions, rownames = FALSE), 
                c("Plt Qty (CHEP)" = "lightcoral", "Plt Qty (Legacy)" = "lightblue", "Plt Qty (CHEP) - Plt Qty (Legacy)" = "lightgreen"))
  })
  
  # CHEP vs JDE based on CHEP Data
  output$chep_jde_based_on_chep_table <- renderDT({
    req(cleaned_chep_data(), cleaned_jde_data())
    data <- chep_jde_based_on_chep(cleaned_chep_data(), cleaned_jde_data())
    filtered_data <- filter_data(data, input$filter_chep_jde_based_on_chep)
    style_table(datatable(filtered_data, extensions = "Buttons", options = DTOptions, rownames = FALSE), 
                c("Plt Qty (CHEP)" = "lightcoral", "Plt Qty (JDE)" = "lightblue", "Plt Qty (CHEP) - Plt Qty (JDE)" = "lightgreen"))
  })
  
  # CHEP vs JDE based on JDE Data
  output$chep_jde_based_on_jde_table <- renderDT({
    req(cleaned_jde_data(), cleaned_chep_data())
    data <- chep_jde_based_on_jde(cleaned_jde_data(), cleaned_chep_data())
    filtered_data <- filter_data(data, input$filter_chep_jde_based_on_jde)
    style_table(datatable(filtered_data, extensions = "Buttons", options = DTOptions, rownames = FALSE), 
                c("Plt Qty (CHEP)" = "lightcoral", "Plt Qty (JDE)" = "lightblue", "Plt Qty (CHEP) - Plt Qty (JDE)" = "lightgreen"))
  })
  
  
  
  
  # Download handler for JDE data
  output$download_jde <- downloadHandler(
    filename = function() { paste("jde-data-", Sys.Date(), ".csv", sep = "") },
    content = function(file) { write.csv(cleaned_jde_data(), file, row.names = FALSE) 
      
    })
  
  # Download handler for AS400 data
  output$download_as400 <- downloadHandler(
    filename = function() { paste("as400-data-", Sys.Date(), ".csv", sep = "") },
    content = function(file) { write.csv(cleaned_as400_data(), file, row.names = FALSE) 
      
    })
  
  # Download handler for CHEP data
  output$download_chep <- downloadHandler(
    filename = function() { paste("chep-data-", Sys.Date(), ".csv", sep = "") },
    content = function(file) { write.csv(cleaned_chep_data(), file, row.names = FALSE) 
      
    })
  
  # Download handler for "Based on CHEP Data" under "CHEP vs AS400"
  output$download_compared_1 <- downloadHandler(
    filename = function() { paste("compared-chep-as400-", Sys.Date(), ".csv", sep = "") },
    content = function(file) {
      data <- chep_as400_based_on_chep(cleaned_chep_data(), cleaned_as400_data())
      filtered_data <- filter_data(data, input$filter_chep_as400_based_on_chep)
      write.csv(filtered_data, file, row.names = FALSE)
    })
  
  # Download handler for "Based on CHEP Data" under "CHEP vs AS400"
  output$download_compared_2 <- downloadHandler(
    filename = function() { paste("compared-chep-as400-", Sys.Date(), ".csv", sep = "") },
    content = function(file) {
      data <- chep_as400_based_on_as400(cleaned_as400_data(), cleaned_chep_data())
      filtered_data <- filter_data(data, input$filter_chep_as400_based_on_as400)
      write.csv(filtered_data, file, row.names = FALSE)
    })
  
  # Download handler for "Based on CHEP Data" under "CHEP vs AS400"
  output$download_compared_3 <- downloadHandler(
    filename = function() { paste("compared-chep-jde-", Sys.Date(), ".csv", sep = "") },
    content = function(file) {
      data <- chep_jde_based_on_chep(cleaned_chep_data(), cleaned_jde_data())
      filtered_data <- filter_data(data, input$filter_chep_jde_based_on_chep)
      write.csv(filtered_data, file, row.names = FALSE)
    })
  
  # Download handler for "Based on CHEP Data" under "CHEP vs AS400"
  output$download_compared_4 <- downloadHandler(
    filename = function() { paste("compared-chep-jde-", Sys.Date(), ".csv", sep = "") },
    content = function(file) {
      data <- chep_jde_based_on_jde(cleaned_jde_data(), cleaned_chep_data())
      filtered_data <- filter_data(data, input$filter_chep_jde_based_on_jde)
      write.csv(filtered_data, file, row.names = FALSE)
    })
  
}
# Run the app
shinyApp(ui = ui, server = server)

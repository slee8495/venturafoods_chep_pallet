# Load necessary libraries
library(shiny)
library(shinythemes)
library(DT)
library(tidyverse)
library(lubridate)
library(readxl)
library(shinyWidgets)


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
               fileInput("jde_file", "Upload JDE file (CSV format); **only 1 location is allowed to upload**", accept = c(".csv")),
               fileInput("as400_file", "Upload AS400 file (CSV format)", accept = c(".csv")),
               fileInput("chep_file", "Upload CHEP files (CSV format); **you can upload multiple files**", accept = c(".csv"), multiple = TRUE)
             ),
             mainPanel()
           )
  ),
  navbarMenu("Data Cleaning Automation",
             tabPanel("JDE", 
                      fluidRow(
                        column(4,
                               pickerInput("jde_ship_location_filter", "Filter by Ship Location (JDE)", 
                                           choices = c("All"), 
                                           selected = "All", 
                                           multiple = TRUE, 
                                           options = list(`actions-box` = TRUE))
                        ),
                        column(4,
                               pickerInput("ship_or_receipt_jde_filter", "Filter by Ship or Receipt (JDE)", 
                                           choices = c("All"), 
                                           selected = "All",
                                           options = list(`actions-box` = TRUE)))
                      ),
                      DTOutput("jde_table"),
                      downloadButton("download_jde", "Download JDE Data")
             ),
             tabPanel("AS400", 
                      fluidRow(
                        column(4, 
                               dateRangeInput("date_range_as400", "Select Date Range:", start = Sys.Date() - 30, end = Sys.Date())
                        ),
                        column(4,
                               pickerInput("ship_location_as400_filter", "Filter by Ship Location (AS400)", 
                                           choices = c("All"), 
                                           selected = "All", 
                                           multiple = TRUE,
                                           options = list(`actions-box` = TRUE)
                               )
                        )
                      ),
                      DTOutput("as400_table"),
                      downloadButton("download_as400", "Download Legacy Data")
             ),
             tabPanel("CHEP", 
                      pickerInput("ship_location_chep_filter", "Filter by Ship Location (CHEP)", 
                                  choices = c("All"), 
                                  selected = "All", 
                                  multiple = TRUE,
                                  options = list(`actions-box` = TRUE)),
                      pickerInput("receipt_location_chep_filter", "Filter by Receipt Location (CHEP)", 
                                  choices = c("All"), 
                                  selected = "All", 
                                  multiple = TRUE,
                                  options = list(`actions-box` = TRUE)),
                      DTOutput("chep_table"),
                      downloadButton("download_chep", "Download CHEP Data")
             )
  ),
  navbarMenu("CHEP x Ventura",
             tabPanel("CHEP vs AS400",
                      tabsetPanel(
                        tabPanel("Based on CHEP Data", 
                                 selectInput("filter_chep_as400_based_on_chep", "Filter by Match:", choices = c("All", "Y", "N")),
                                 DTOutput("chep_as400_based_on_chep_table"),
                                 downloadButton("download_compared_1", "Download")),
                        tabPanel("Based on AS400 Data", 
                                 fluidRow(
                                   column(6, 
                                          dateRangeInput("date_range_as400_data", "Select Date Range:", start = Sys.Date() - 30, end = Sys.Date())
                                   ),
                                   column(6, 
                                          selectInput("filter_chep_as400_based_on_as400", "Filter by Match:", choices = c("All", "Y", "N"))
                                   )
                                 ),
                                 DTOutput("chep_as400_based_on_as400_table"),
                                 downloadButton("download_compared_2", "Download"))
                      )
             ),
             tabPanel("CHEP vs JDE",
                      tabsetPanel(
                        tabPanel("Based on CHEP Data", 
                                 fluidPage(
                                   column(6, selectInput("filter_chep_jde_based_on_chep", "Filter by Match:", choices = c("All", "Y", "N"))),
                                   column(6, selectInput("filter_ship_or_receipt_jde", "Filter by Ship or Receipt (JDE):", choices = c("All", "Ship", "Receipt")))
                                 ),
                                 DTOutput("chep_jde_based_on_chep_table"),
                                 downloadButton("download_compared_3", "Download")),
                        tabPanel("Based on JDE Data", 
                                 fluidPage(
                                   column(6, dateRangeInput("date_range_jde_data", "Select Date Range:", start = Sys.Date() - 30, end = Sys.Date())),
                                   column(6, selectInput("filter_chep_jde_based_on_jde", "Filter by Match:", choices = c("All", "Y", "N"))),
                                   column(6, selectInput("filter_ship_or_receipt_jde_2", "Filter by Ship or Receipt (JDE):", choices = c("All", "Ship", "Receipt")))
                                 ),
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
    
    
    # Update choices for ship location filter (JDE)
    updatePickerInput(session, "jde_ship_location_filter", 
                      choices = sort(unique(as.factor(cleaned_jde_data()$ship_location_jde))),
                      selected = unique(as.factor(cleaned_jde_data()$ship_location_jde)))
    
    # Update choices for ship or receipt filter (JDE)
    updatePickerInput(session, "ship_or_receipt_jde_filter", 
                      choices = c("All", sort(unique(as.character(cleaned_jde_data()$ship_or_receipt_jde)))),
                      selected = "All")
    
    output$jde_table <- renderDT({
      data <- cleaned_jde_data()
      
      if (!"All" %in% input$jde_ship_location_filter) {
        data <- data %>% filter(ship_location_jde %in% input$jde_ship_location_filter)
      }
      
      if (input$ship_or_receipt_jde_filter != "All") {
        data <- data %>% filter(ship_or_receipt_jde == input$ship_or_receipt_jde_filter)
      }
      style_table(datatable(data, 
                            extensions = c("Buttons", "FixedHeader"), 
                            options = list(pageLength = 100,
                                           dom = "Blfrtip",
                                           buttons = c("copy", "csv", "excel"),
                                           scrollX = TRUE,
                                           scrollY = "1500px",
                                           fixedHeader = TRUE,
                                           fixedColumns = list(leftColumns = 2)), 
                            rownames = FALSE),
                  c(plt_qty_jde = "lightblue",
                    customer_po_number_jde = "lightgray"))
    })
  })
  
  # AS400 File Processing
  observeEvent(input$as400_file, {
    req(input$as400_file)
    as400_data <- read_csv(input$as400_file$datapath)
    cleaned_as400_data(as400_cleaning(as400_data))
    
    # Update choices for ship location filter
    updatePickerInput(session, "ship_location_as400_filter", 
                      choices = sort(unique(as.factor(cleaned_as400_data()$ship_location_as400))),
                      selected = unique(as.factor(cleaned_as400_data()$ship_location_as400)))
    
    output$as400_table <- renderDT({
      data <- cleaned_as400_data()
      if (!is.null(input$date_range_as400)) {
        data <- data %>% filter(ship_date_as400 >= input$date_range_as400[1] & ship_date_as400 <= input$date_range_as400[2])
      }
      if (!"All" %in% input$ship_location_as400_filter) {
        data <- data %>% filter(ship_location_as400 %in% input$ship_location_as400_filter)
      }
      if (!"All" %in% input$ship_location_as400_filter) {
        data <- data %>% filter(ship_location_as400 %in% input$ship_location_as400_filter)
      }
      style_table(datatable(data, 
                            extensions = c("Buttons", "FixedHeader"), 
                            options = list(pageLength = 100,
                                           dom = "Blfrtip",
                                           buttons = c("copy", "csv", "excel"),
                                           scrollX = TRUE,
                                           scrollY = "1500px",
                                           fixedHeader = TRUE,
                                           fixedColumns = list(leftColumns = 2)), 
                            rownames = FALSE),
                  c(plt_qty_as400 = "lightblue",
                    bill_of_lading_as400 = "lightgray"))
    })
  })
  
  # CHEP File Processing
  observeEvent(input$chep_file, {
    req(input$chep_file)
    files <- input$chep_file$datapath
    
    # Read and combine all files
    combined_chep_data <- purrr::map_df(files, read_csv)
    
    # Clean and store the combined data
    cleaned_chep_data(chep_cleaning(combined_chep_data))
    
    # Unique, convert to numeric, sort, and then convert back to character for ship location
    ship_location_sorted <- unique(cleaned_chep_data()$ship_location_chep)
    ship_location_sorted <- sort(as.numeric(ship_location_sorted))
    ship_location_sorted <- na.omit(ship_location_sorted) # Remove NA values if present
    ship_location_sorted <- as.character(ship_location_sorted)
    
    # Unique, convert to numeric, sort, and then convert back to character for receipt location
    receipt_location_sorted <- unique(cleaned_chep_data()$receipt_location_chep)
    receipt_location_sorted <- sort(as.numeric(receipt_location_sorted))
    receipt_location_sorted <- na.omit(receipt_location_sorted) # Remove NA values if present
    receipt_location_sorted <- as.character(receipt_location_sorted)
    
    # Update choices for CHEP filters
    updatePickerInput(session, "ship_location_chep_filter", 
                      choices = c(ship_location_sorted),
                      selected = ship_location_sorted)
    updatePickerInput(session, "receipt_location_chep_filter", 
                      choices = c(receipt_location_sorted),
                      selected = receipt_location_sorted)
    
    output$chep_table <- renderDT({
      data <- cleaned_chep_data()
      if (!"All" %in% input$ship_location_chep_filter) {
        data <- data %>% filter(ship_location_chep %in% input$ship_location_chep_filter)
      }
      if (!"All" %in% input$receipt_location_chep_filter) {
        data <- data %>% filter(receipt_location_chep %in% input$receipt_location_chep_filter)
      }
      style_table(datatable(data, 
                            extensions = c("Buttons", "FixedHeader"), 
                            options = list(pageLength = 100,
                                           dom = "Blfrtip",
                                           buttons = c("copy", "csv", "excel"),
                                           scrollX = TRUE,
                                           scrollY = "1500px",
                                           fixedHeader = TRUE,
                                           fixedColumns = list(leftColumns = 2)), 
                            rownames = FALSE),
                  c(plt_qty_chep = "lightgreen",
                    customer_po_number_chep = "lightgray",
                    bill_of_lading_chep = "lightgray"))
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
    style_table(datatable(filtered_data, 
                          extensions = c("Buttons", "FixedHeader"), 
                          options = list(pageLength = 100,
                                         dom = "Blfrtip",
                                         buttons = c("copy", "csv", "excel"),
                                         scrollX = TRUE,
                                         scrollY = "1500px",
                                         fixedHeader = TRUE,
                                         fixedColumns = list(leftColumns = 2)), 
                          rownames = FALSE),
                c("Plt Qty (CHEP)" = "lightgreen", 
                  "Plt Qty (Legacy)" = "lightblue", 
                  "Plt Qty (CHEP) - Plt Qty (Legacy)" = "lightcoral",
                  "Bill of Lading (Legacy)" = "lightgray",
                  "Bill of Lading (CHEP)" = "lightgray"))
  })
  
  # CHEP vs AS400 based on AS400 Data
  output$chep_as400_based_on_as400_table <- renderDT({
    req(cleaned_chep_data(), cleaned_as400_data())
    data <- chep_as400_based_on_as400(cleaned_as400_data(), cleaned_chep_data())
    
    if (!is.null(input$date_range_as400_data)) {
      data <- data %>% filter(`Ship Date (Legacy)` >= input$date_range_as400_data[1] & `Ship Date (Legacy)` <= input$date_range_as400_data[2])
    }
    
    filtered_data <- filter_data(data, input$filter_chep_as400_based_on_as400)
    style_table(datatable(filtered_data, 
                          extensions = c("Buttons", "FixedHeader"), 
                          options = list(pageLength = 100,
                                         dom = "Blfrtip",
                                         buttons = c("copy", "csv", "excel"),
                                         scrollX = TRUE,
                                         scrollY = "1500px",
                                         fixedHeader = TRUE,
                                         fixedColumns = list(leftColumns = 2)), 
                          rownames = FALSE),
                c("Plt Qty (CHEP)" = "lightgreen", 
                  "Plt Qty (Legacy)" = "lightblue", 
                  "Plt Qty (CHEP) - Plt Qty (Legacy)" = "lightcoral",
                  "Bill of Lading (Legacy)" = "lightgray",
                  "Bill of Lading (CHEP)" = "lightgray"))
  })
  
  # CHEP vs JDE based on CHEP Data
  output$chep_jde_based_on_chep_table <- renderDT({
    req(cleaned_chep_data(), cleaned_jde_data())
    data <- chep_jde_based_on_chep(cleaned_chep_data(), cleaned_jde_data())
    filtered_data <- filter_data(data, input$filter_chep_jde_based_on_chep)
    
    # Apply additional filter for Ship or Receipt
    if (input$filter_ship_or_receipt_jde != "All") {
      filtered_data <- filtered_data %>% filter(`Ship or Receipt (JDE)` == input$filter_ship_or_receipt_jde)
    }
    
    style_table(datatable(filtered_data, 
                          extensions = c("Buttons", "FixedHeader"), 
                          options = list(pageLength = 100,
                                         dom = "Blfrtip",
                                         buttons = c("copy", "csv", "excel"),
                                         scrollX = TRUE,
                                         scrollY = "1500px",
                                         fixedHeader = TRUE,
                                         fixedColumns = list(leftColumns = 2)), 
                          rownames = FALSE),
                c("Plt Qty (CHEP)" = "lightgreen", 
                  "Plt Qty (JDE)" = "lightblue", 
                  "Plt Qty (CHEP) - Plt Qty (JDE)" = "lightcoral",
                  "Customer PO # (JDE)" = "lightgray",
                  "Customer PO # (CHEP)" = "lightgray"))
  })
  
  
  
  
  
  # CHEP vs JDE based on JDE Data
  output$chep_jde_based_on_jde_table <- renderDT({
    req(cleaned_jde_data(), cleaned_chep_data())
    data <- chep_jde_based_on_jde(cleaned_jde_data(), cleaned_chep_data())
    
    if (!is.null(input$date_range_jde_data)) {
      data <- data %>% filter(`Actual Ship Date (JDE)` >= input$date_range_jde_data[1] & `Actual Ship Date (JDE)` <= input$date_range_jde_data[2])
    }
    
    filtered_data <- filter_data(data, input$filter_chep_jde_based_on_jde)
    
    # Apply additional filter for Ship or Receipt
    if (input$filter_ship_or_receipt_jde_2 != "All") {
      filtered_data <- filtered_data %>% filter(`Ship or Receipt (JDE)` == input$filter_ship_or_receipt_jde_2)
    }
    
    style_table(datatable(filtered_data, 
                          extensions = c("Buttons", "FixedHeader"), 
                          options = list(pageLength = 100,
                                         dom = "Blfrtip",
                                         buttons = c("copy", "csv", "excel"),
                                         scrollX = TRUE,
                                         scrollY = "1500px",
                                         fixedHeader = TRUE,
                                         fixedColumns = list(leftColumns = 2)), 
                          rownames = FALSE),
                c("Plt Qty (CHEP)" = "lightgreen", 
                  "Plt Qty (JDE)" = "lightblue", 
                  "Plt Qty (CHEP) - Plt Qty (JDE)" = "lightcoral",
                  "Customer PO # (JDE)" = "lightgray",
                  "Customer PO # (CHEP)" = "lightgray"))
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
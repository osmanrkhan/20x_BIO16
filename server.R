library(shiny)
library(tidyverse)
library(rlang)
library(readr)
library(forecast)
library(plotly)
library(glue)


function(input, output, session){
  # RAW DATA SECTION ------------------------------------------------------------------####
  data <- data_preview_server(id = "data_vars", start_path = "data/processed_data/")
  # Getting head table 
  # TODO: Control number of lines 
  output$preview <- renderTable({
    head(data(), 20)
  })
  # Histogram plots  
  hist_plt <- histogram_server(id = "histo_plot", variables = raw_variables, data = data()) 
  
  output$histogram <- renderPlotly({
    hist_plt()
  })
  
  vvt_plt <- plot_vvt_server(id = "time_plot", data = data(), varlist = raw_variables)
  
  output$vvt_plt <- renderPlotly({
    vvt_plt()
  })
  
  pairplot <- bivariate_server(id = "bivar_plot", variables = raw_variables, data = data())
  
  # render pairplot
  output$pairplot <- renderPlotly({
    suppressWarnings(pairplot$plot)
  })
  
  ## FULL DATA SECTION ----------------------------------------------------------------####
  
  full_data <- reactive({
    readRDS(file = "data/processed_data/silas_little_2018.rds")
  })
  
  # Getting head table 
  output$full_preview <- renderTable({
    head(full_data(), 10)
  })
  
  # nee time plot
  full_time_plot <-  full_time_plot_server("full_var_vs_time", full_data(), full_variables)
  
  output$full_time_plot <- renderPlot({
    full_time_plot()
  })
  
  
  # regression section 
  reg <- regression_server("reg_select", full_data())
}
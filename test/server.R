library(shiny)
library(tidyverse)
library(rlang)
library(readr)
library(forecast)
library(plotly)
library(glue)
source("dataLoading.R")
source("histogram.R")
source("var_vs_time.R")
source("bivariate_plot.R")

function(input, output, session){

  # load data on push load_data
  data <- data_preview_server(id = "data_vars", start_path = "../data/processed_data/silas_little")
  
  # Getting head table 
  # TODO: Control number of lines 
  output$preview <- renderTable({
    head(data(), 10)
  })
  
  
  # Histogram plots  
  hist_plt <- histogram_server(id = "histo_plot", variables = variables, data = data()[seq(1,86400)]) 
  
  output$histogram <- renderPlotly({
    hist_plt()
  })
  
  
  plot_var_vs_time_server(id = "time_plot", variables = variables, data = data())
 
  
  
  pairplot <- bivariate_server(id = "bivar_plot", variables = variables, data = data())
  
  # render pairplot
  output$pairplot <- renderPlotly({
    suppressWarnings(pairplot$plot)
  })
}
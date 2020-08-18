library(shiny)
library(tidyverse)
library(rlang)
library(readr)
library(forecast)
library(plotly)
library(glue)
source("dataPreview.R")
source("histogram.R")
source("var_vs_time.R")
source("bivariate_plot.R")

function(input, output, session){
  # List of variables and their more formatted names for easy plotting
  variables <- list("Vertical Wind Speed" = "w" , 
                    "Horizontal Wind Speed (North)" = "v" , 
                    "Horizontal Wind Speed (East)" ="u",
                    "CO2" = "CO2",
                    "Water Vapor" = "H2O",
                    "Air Temperature" = "airtemp")
  
  # load data on push load_data
  data <- data_preview_server(id = "data_vars", start_path = "data/processed_data/")
  
  # Getting head table 
  # TODO: Control number of lines 
  output$preview <- renderTable({
    head(data(), 20)
  })
  
  
  # Histogram plots  
  hist_plt <- histogram_server(id = "histo_plot", variables = variables, data = data()) 
  
  output$histogram <- renderPlotly({
    hist_plt()
  })
  
  
 vvt_plt <- plot_var_vs_time_server(id = "time_plot", variables = variables, data = data())
  
  output$vvt_plt_vs_time <- renderPlotly({
    vvt_plt()
  })
  
  pairplot <- bivariate_server(id = "bivar_plot", variables = variables, data = data())
  
  # render pairplot
  output$pairplot <- renderPlotly({
    suppressWarnings(pairplot$plot)
  })
}
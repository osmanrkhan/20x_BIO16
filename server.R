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
source("NEE_bivariate.R")

function(input, output, session){
  # List of variables and their more formatted names for easy plotting
  variables <- list("Vertical Wind Speed" = "w" , 
                    "Horizontal Wind Speed (North)" = "v" , 
                    "Horizontal Wind Speed (East)" ="u",
                    "CO2" = "CO2",
                    "Water Vapor" = "H2O",
                    "Air Temperature" = "airtemp")
  
  # load data on push load_data
  start_path = "data/processed_data/"
  data <- data_preview_server(id = "data_vars", start_path)
  
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
  
  nee_data <- data_preview_server(id = "nee_data_vars", start_path)
  # Getting head table 
  # TODO: Control number of lines 
  output$nee_preview <- renderTable({
    head(nee_data(), 10)
  })
  eddy_cov_variables = list("Air Temperature" = "TA",
                            "Air Temperature Squared" = "TA.2",
                            "Photosynthetically Active Radiation" = "PPFD_in",
                            "Soil Moisture" = "Vol.W.C",
                            "Relative Humidity" = "RH")
  
  
  nee_bivar_plt <- NEE_bivar_server(id = "NEE_bivariate", variables = eddy_cov_variables, data = nee_data())
  
  output$NEE_bivariate_plot <- renderPlotly({
    suppressWarnings(nee_bivar_plt())
  })
  
  
}
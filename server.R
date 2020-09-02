library(shiny)
library(tidyverse)
library(rlang)
library(readr)
library(forecast)
library(plotly)
library(glue)


function(input, output, session){
  
  # List of variables and their more formatted names for easy plotting
  variables <- list("Vertical Wind Speed" = "w" , 
                    "Horizontal Wind Speed (North)" = "v" , 
                    "Horizontal Wind Speed (East)" ="u",
                    "CO2" = "CO2",
                    "Water Vapor" = "H2O",
                    "Air Temperature" = "airtemp")
  
  eddy_cov_variables = list("NEE" = "NEE",
                            "Air Temperature" = "TA",
                            "Air Temperature Squared" = "TA.2",
                            "Photosynthetically Active Radiation" = "PPFD_in",
                            "Soil Moisture" = "Vol.W.C",
                            "Relative Humidity" = "RH")
  eddy_cov_time_variables = list("Julian Day" = "JD", "Month" = "DT", "Hours of Day" =  "HM")
  
          # ----------------------------------------------------------------#
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
  
  vvt_plt <- plot_vvt_server(id = "time_plot", data = data())
  
  output$vvt_plt <- renderPlotly({
    vvt_plt()
  })
  
  pairplot <- bivariate_server(id = "bivar_plot", variables = variables, data = data())
  
  # render pairplot
  output$pairplot <- renderPlotly({
    suppressWarnings(pairplot$plot)
  })
  
             # ----------------------------------------------------------------#
  
  nee_data <- data_preview_server(id = "nee_data_vars", start_path = "data/processed_data/")
  # Getting head table 
  # TODO: Control number of lines 
  output$nee_preview <- renderTable({
    head(nee_data(), 10)
  })
  
  # nee time plot
  nee_time_plt <-  NEE_time_plot_server("NEE_var_vs_time", nee_data(), eddy_cov_time_variables,
                                        eddy_cov_variables)
  
  output$NEE_time_plot <- renderPlotly({
    suppressWarnings(nee_time_plt())
  })
  
  nee_bivar_plt <- NEE_bivar_server(id = "NEE_bivariate", variables = eddy_cov_variables, data = nee_data())
  
  output$NEE_bivariate_plot <- renderPlotly({
    suppressWarnings(nee_bivar_plt())
  })
  
  output$sumtab <- renderText({"Placeholder text for summary table"})
  output$sumplot <- renderText({"Placeholder plot for summary table"})
}
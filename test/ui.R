library(shiny)
library(plotly)
source("dataLoading.R")
source("histogram.R")
source("var_vs_time.R")
source("bivariate_plot.R")


options(shiny.maxRequestSize=50*1024^2)

# ui elements 
fluidPage(
  h3("File selection"),
  sidebarLayout(
    sidebarPanel(
      data_preview_var_ui("data_vars")
    ),
    mainPanel(
      h4("Data Preview"),
      tableOutput("preview")
    )
  ),
  
  # Histogram
  h3("Histogram"),
  sidebarLayout(
    sidebarPanel(
      histogram_ui(id = "histo_plot")
    ),
    mainPanel(
      #plotlyOutput("box_plot"),
      plotlyOutput("histogram")
    )
  ),
  
  # Variable vs time  
  h3("Variable vs Time"),
  plot_var_vs_time_ui1("time_plot", variables),
  
  # Pair plots 
  h3("Bivariate plots"),
  sidebarLayout(
    sidebarPanel(
      bivariate_ui("bivar_plot")
    ),
    mainPanel(
      plotlyOutput("pairplot"),
      plotlyOutput("covarplot")
    )
  ),
)

library(shiny)
library(plotly)
source("dataLoading.R")
source("histogram.R")
source("var_vs_time.R")
source("bivariate_plot.R")
source("NEE_bivariate.R")

options(shiny.maxRequestSize=50*1024^2)

#' files used for data loading
#' 
site =  list("Silas Little" = "silas_little")
raw_data_files = list("First DataSet" = "comb_2018-06-05_2018-01-19",
                   "Second DataSet" = "comb_2018-06-06_2018-01-20" )
nee_data_files = list("DataSet" = "2018")
# ui elements 
fluidPage(
  h3("Raw Data File selection"),
  sidebarLayout(
    sidebarPanel(
      data_preview_var_ui("data_vars", raw_data_files, site)
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
    )
  ),
  # NEE file selection
  h3("NEE Data File selection"),
  sidebarLayout(
    sidebarPanel(
      data_preview_var_ui("nee_data_vars", nee_data_files, site)
    ),
    mainPanel(
      h4("NEE Data Preview"),
      tableOutput("nee_preview")
    )
  ),
  # NEE bivariate plot
  h3("NEE Bivariate plots"),
  sidebarLayout(
    sidebarPanel(
      NEE_bivar_ui("NEE_bivariate" )
    ),
    mainPanel(
      plotlyOutput("NEE_bivariate_plot")
    )
  )
)

library(shiny)
library(plotly)
source("dataPreview.R")
source("histogram.R")
source("var_vs_time.R")
source("bivariate_plot.R")

options(shiny.maxRequestSize=50*1024^2)

# ui elements 
fluidPage(
  h3("Data selection"),
  h5("In this section you can select which data set to load by selecting measurement site, 
    season, and available dates within the season. The data set for this section are 10Hz measurements 
    1 day (24 hours). For the sake of simplicity, all values are averaged within the second"),
  tags$ul(
    tags$li(tags$strong("second"),": Current time in seconds"),
    tags$li(tags$strong("minute"),": Current time in minutes"),
    tags$li(tags$strong("hour"),": Current time in hours"),
    tags$li(tags$strong("u"),": Horizontal Wind Speed (East) in meters per second"),
    tags$li(tags$strong("v"),": Horizontal Wind Speed (North) in meters per second"),
    tags$li(tags$strong("w"),": Vertical Wind Speed"),
    tags$li(tags$strong("airtemp"),": Air Temperature"),
    tags$li(tags$strong("CO2"), ": CO2 measurement "),
    tags$li(tags$strong("H2O")),
    tags$li(tags$strong("code"))
  ),
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
  sidebarLayout(
    sidebarPanel(
      plot_var_vs_time_ui("time_plot")
    ),
    mainPanel(
      plotlyOutput("vvt_plt_vs_time"),
    )
  ),
  
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

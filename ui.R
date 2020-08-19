library(shiny)
library(plotly)
source("dataPreview.R")
source("histogram.R")
source("var_vs_time.R")
source("bivariate_plot.R")

options(shiny.maxRequestSize=50*1024^2)

# ui elements 
fluidPage(
  h3("Data Preview"),
  sidebarLayout(
    sidebarPanel(
      h5("In this section you can select which data set to load by selecting measurement site, 
    season, and available dates within the season. The data set for this section are 10Hz measurements 
    1 day (24 hours). For the sake of simplicity, all values are averaged within the second. In the box below, you can 
         select site, as well as the specific combined data set (combining winter and summer) available for each site"),
      tags$ul(
        tags$li(tags$strong("second"),": Current time in seconds"),
        tags$li(tags$strong("time"), ": Current time in POSIX format"),
        tags$li(tags$strong("u"),": Horizontal Wind Speed (East) in meters per second"),
        tags$li(tags$strong("v"),": Horizontal Wind Speed (North) in meters per second"),
        tags$li(tags$strong("w"),": Vertical Wind Speed in meters per second"),
        tags$li(tags$strong("airtemp"),": Sonic air temperature, calculated from speed of sound measurements in Celsius"),
        tags$li(tags$strong("CO2"), ": CO2 mixing ratio (micro-mol CO2 per mol of air)"),
        tags$li(tags$strong("H2O"), ": H2O mixing ratio (micro-mol H2O per mol of air)"),
        tags$li(tags$strong("code"), ": Error code for sonic sensor paths")
      ),
      data_preview_var_ui("data_vars"),
      tags$ul(
        tags$li(tags$strong("comb_2018-06-05_2018-01-19"), ": Data set combining dates 06/05/2018 (summer) and 01/19/2018 (winter)"),
        tags$li(tags$strong("comb_2018-06-06_2018-01-20"), ": Data set combining dates 06/06/2018 (summer) and 01/20/2018 (winter)")
      )
    ),
    mainPanel(
      tableOutput("preview")
    )
  ),
  
  # Histogram
  h3("Histogram"),
  sidebarLayout(
    sidebarPanel(
      h5("In this section, you can observe the distribution of values by plotting a histogram of the variable. 
       You can plot histograms for each season, or super impose them on the same plot"),
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

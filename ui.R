library(shiny)
library(plotly)
# source("dataPreview.R")
# source("histogram.R")
# source("var_vs_time.R")
# source("bivariate_plot.R")
# source("NEE_bivariate.R")
# source("NEE_time_plot.R")

options(shiny.maxRequestSize=50*1024^2)

#' files used for data loading
#' 
site =  list("Silas Little" = "silas_little")
raw_data_files = list("Combined 06/05/2018 and 01/19/2018" = "comb_2018-06-05_2018-01-19",
                      "Combined 06/06/2018 and 01/20/2018" = "comb_2018-06-06_2018-01-20")
nee_data_files = list("DataSet" = "2018")
eddy_cov_variables = list("NEE" = "NEE",
                          "Air Temperature" = "TA",
                          "Air Temperature Squared" = "TA.2",
                          "Photosynthetically Active Radiation" = "PPFD_in",
                          "Soil Moisture" = "Vol.W.C",
                          "Relative Humidity" = "RH")
eddy_cov_time_variables = list("Julian Day" = "JD", "Month" = "DT", "Hours of Day" =  "HM")

# ui elements 
fluidPage(
  h1("Raw Eddy Covariance Data"),
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
      data_preview_var_ui("data_vars", raw_data_files, site),
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
      plotlyOutput("histogram")
    )
  ),
  
  h3("Variable vs Time"),
  sidebarLayout(
    sidebarPanel(
      plot_vvt_ui(id = "time_plot")
    ),
    mainPanel(
      plotlyOutput("vvt_plt")
    )
  ),
  
  # all these breaks are because we're fixing the size of the vvt plots 
  br(),
  br(),
  br(),
  br(),
  br(),
  
  # Pair plots 
  h3("Bivariate plots"),
  sidebarLayout(
    sidebarPanel(
      bivariate_ui("bivar_plot")
    ),
    mainPanel(
      plotlyOutput("pairplot")
    )
  ), # NEE file selection
  hr(),
  h1("Year long Eddy Covariance Data"),
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
  
  # NEE variable vs time
  h3("NEE Time plots"),
  sidebarLayout(
    sidebarPanel(
      NEE_time_plot_ui("NEE_var_vs_time", eddy_cov_variables, eddy_cov_time_variables)
    ),
    mainPanel(
      plotlyOutput("NEE_time_plot")
    )
  ),
  # NEE bivariate plot
  h3("NEE Bivariate plots"),
  sidebarLayout(
    sidebarPanel(
      NEE_bivar_ui("NEE_bivariate", eddy_cov_variables )
    ),
    mainPanel(
      plotlyOutput("NEE_bivariate_plot")
    )
  ),
  
  # Regression builder
  h3("Gapfilling regression model"),
  sidebarLayout(
    sidebarPanel(
      
    ), mainPanel(
      textOutput("sumtab"),
      textOutput("sumplot")
    )
  )
)

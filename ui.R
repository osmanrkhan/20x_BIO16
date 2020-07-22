library(shiny)
library(plotly)
options(shiny.maxRequestSize=50*1024^2)

# ui elements 
fluidPage(
  h3("File selection"),
  sidebarLayout(
    sidebarPanel(
      selectInput("site", h3("Select Site"), choices = list("Silas Little" = "silas_little"), 
                  selected = "silas_little"),
      selectInput("season", h3("Select season"),
                  choices = list("summer", "winter"), selected = 1),
      selectInput("date", h3("Select date"), ""),
      actionButton(inputId = "load", "Load Data")
    ),
    mainPanel(
      h4("Data Preview"),
      tableOutput(outputId = "preview")
    )
  ),
  
  
  #h3("Data Preview"),
  #selectInput("window_size", "Select the size of window for running average", 
  #            choices = list("10 seconds" = "10", "30 seconds " = "30" , "1 minute" = "60"),
  #            selected = "10 seconds"),
  #textOutput("checking"),
  
  # Histogram
  h3("Histogram"),
  sidebarLayout(
    sidebarPanel(
      selectInput("histogram_var", h4("Select box"), 
                  choices = list( "Vertical Wind Speed" = "w" , 
                                  "Horizontal Wind Speed (North)" = "v" , 
                                  "Horizontal Wind Speed (East)" ="u",
                                  "CO2" = "CO2",
                                  "Water Vapor" = "H2O",
                                  "Air Temperature" = "airtemp"),
                  selected = "CO2"),
      
      sliderInput("histogram_bins", h4("number of bins"),
                  min = 0, max = 100, value = 50),
      actionButton("load_hist", "Load Graph")
    ),
    mainPanel(
      plotlyOutput("histogram")
    )
  ),
  
  # Variable vs time  
  h3("Variable vs Time"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("vvt", "Select Variables", 
                        choices = list( "Vertical Wind Speed" = "w" , 
                                        "Horizontal Wind Speed (North)" = "v" , 
                                        "Horizontal Wind Speed (East)" ="u",
                                        "CO2" = "CO2",
                                        "Water Vapor" = "H2O",
                                        "Air Temperature" = "airtemp"),
                        selected = "w"),
      radioButtons("timescale", h4("Render Time Scale"),
                   list("Second" = "sec",
                        "Minute" = "min",
                        "Hour" = "hr")),
      actionButton("load_vvt", "Load graph")
    ),
    mainPanel(
      plotlyOutput("vvt_plt_vs_time"),
    )
  ),
  
  # Pair plots 
  h3("Bivariate plots"),
  sidebarLayout(
    sidebarPanel(
      selectInput("pairplot_xvar", "Select X Axis", 
                  choices = list( "Vertical Wind Speed" = "w" , 
                                  "Horizontal Wind Speed (North)" = "v" , 
                                  "Horizontal Wind Speed (East)" ="u",
                                  "CO2" = "CO2",
                                  "Water Vapor" = "H2O",
                                  "Air Temperature" = "airtemp"),
                  selected = "w"),
      selectInput("pairplot_yvar", "Select Y Axis", 
                  choices = list( "Vertical Wind Speed" = "w" , 
                                  "Horizontal Wind Speed (North)" = "v" , 
                                  "Horizontal Wind Speed (East)" ="u",
                                  "CO2" = "CO2",
                                  "Water Vapor" = "H2O",
                                  "Air Temperature" = "airtemp"),
                  selected = "v"),
      actionButton("load_pair", "Load Graph")
    ),
    mainPanel(
      plotlyOutput("pairplot")
    )
  )
)

library(shiny)
library(plotly)
options(shiny.maxRequestSize=50*1024^2)
fluidPage(
  h3("File selection"),
  inputPanel(selectInput("season", h3("Select season"),
                         choices = list("summer", "winter"), selected = 1)),
  inputPanel(
    selectInput("file", h3("Select date"), 
                choices = list("june 5 2018" =  "june_5_2018", "june 6 2018"="june_6_2018"), 
                selected = "june_5_2018")
  ),
  h3("Data Preview"),
  selectInput("window_size", "Select the size of window for running average", 
              choices = list("10 seconds" = "10", "30 seconds " = "30" , "1 minute" = "60"),
              selected = "10 seconds"),
  textOutput("checking"),
  
  tableOutput("preview"),
  # Histogram
  h3("Histogram"),
  sidebarLayout(
    sidebarPanel(
      selectInput("histogram_var", h4("Select box"), 
                  choices = list("CO2" = "CO2", "horizontal speed 1" = "u" , "horizontal speed 1" = "v" , 
                                 "vertical Speed" = "w", "Temperature" = "airtemp", "H2O" = "H2O"),
                  selected = "CO2"),
      
      sliderInput("histogram_bins", h4("number of bins"),
                  min = 0, max = 100, value = 50)
    ),
    mainPanel(
      plotlyOutput("histogram")
    )
  ),
  
  # Variable vs time  
  h3("wind vs Time"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("vvt_wind", "Select Variables", 
                        choices = list( "vertical wind speed" = "u" , "horizontal wind speed1" = "v" , "horizontal wind speed2" ="w"),
                        selected = "u"),
      selectInput("vvt_time", "Select Time scale", 
                  choices = list("seconds" = "sec", "minutes" = "min" , "hours" = "hours"),
                  selected = "sec")
    ),
    mainPanel(
      plotlyOutput("vvt_plt")
    )
  ),
  
  h3("Variable vs Time"),
  sidebarLayout(
    sidebarPanel(
      selectInput("vvt", "Select Variables", 
                         choices = list( "CO2" = "CO2" , "Temperature" = "airtemp" , "H2O" ="H2O"),
                         selected = "CO2"),
      selectInput("v_time", "Select Time scale", 
                  choices = list("seconds" = "sec", "minute" = "min" , "hours" = "hours"),
                  selected = "sec")
    ),
    mainPanel(
      plotlyOutput("vvt_plt_vs_time"),
    )
  ),
  
  # Pair plots 
  h3("Bivariate plots"),
  sidebarLayout(
    sidebarPanel(
      selectInput('pairplot_xvar', "X Axis", 
                  choices = list("CO2"="CO2","H2O"="H2O","Temperature"='airtemp',
                                "u" = "u", "w" = 'w', "v"="v")),
      selectInput('pairplot_yvar', "Y Axis", 
                  choices = list("CO2"="CO2","H2O"="H2O","Temperature"="airtemp",
                                 "u" = "u", "w" = 'w', "v"="v"), selected = "airtemp")
    ),
    mainPanel(
      plotOutput("pairplot")
    )
  )
)

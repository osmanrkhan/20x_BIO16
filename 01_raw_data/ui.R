library(shiny)
options(shiny.maxRequestSize=50*1024^2)
fluidPage(
  #TODO: https://stackoverflow.com/questions/21465411/r-shiny-passing-reactive-to-selectinput-choices
  h3("Alternative file selection process"),
  inputPanel(selectInput("season", h3("Select season"), 
                         choices = list("Summer", "Winter"), selected = "Summer")),
  conditionalPanel(
    condition = "input.season === Summer",
    selectInput("file", h3("Select date"), 
                choices = list("june 5 2018", "june 6 2018"), 
                selected = "june 5 2018")
  ),
  conditionalPanel(
    condition = "input.season === Winter",
    selectInput("file", h3("Select date"), 
                choices = list("jan 19 2018" , "jan 20 2018"), 
                selected = "jan 19 2018")
  ),
  # Histogram
  h3("Histogram"),
  sidebarLayout(
    sidebarPanel(
      selectInput("histogram_var", h4("Select box"), 
                  choices = list("CO2", "u" , "v" , "w",
                                 "airtemp"),
                  selected = 1),
      
      sliderInput("histogram_bins", h4("number of bins"),
                  min = 0, max = 100, value = 50)
    ),
    mainPanel(
      plotOutput("histogram")
    )
  ),
  
  # Variable vs time  
  h3("Variable vs Time"),
  sidebarLayout(
    sidebarPanel(
      selectInput("vvt_wind", "Select Variables", 
                        choices = list( "Vertical Wind Speed" = "w" , 
                                        "Horizontal Wind Speed (North)" = "v" , 
                                        "Horizontal Wind Speed (East)" ="u",
                                        "CO2" = "CO2",
                                        "Water Vapor" = "H2O",
                                        "Air Temperature" = "airtemp"),
                        selected = "w"),
      h4("Render Time Scale"),
      actionButton("sec", "Second"),
      actionButton("min", "Minute"),
      actionButton("hour", "Hour")
    ),
    mainPanel(
      plotOutput("vvt_plt")
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
                                 "u" = "u", "w" = 'w', "v"="v"), selected = "airtemp"),
      actionButton("pairplot_second", "Second"),
      actionButton("pairplot_minute", "Minute"),
      actionButton("pairplot_hour", "Hour")
    ),
    mainPanel(
      plotOutput("pairplot")
    )
  )
)

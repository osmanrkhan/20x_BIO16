library(shiny)
options(shiny.maxRequestSize=50*1024^2)
fluidPage(
  h3("Data Preview"),
  sidebarLayout(
    sidebarPanel(
      fileInput("rawdata", "Choose CSV File", multiple = T, 
                accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
      numericInput("n_row", "Number of rows to import", value = 6000, max = 864000,
                     step = 10)
    ),
    mainPanel(
      tableOutput("preview")
    )
  ),
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
      checkboxGroupInput("vvt_wind", "Select Variables", 
                        choices = list( "vertical wind speed" = "u" , "horizontal wind speed1" = "v" , "horizontal wind speed2" ="w"),
                        selected = "u"),
      selectInput("vvt_time", "Select Time scale", 
                  choices = list("1/10th sec" = 1, "sec" = 2, "min" = 3, "hours"= 4),
                  selected = 1)
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
                                 "u" = "u", "w" = 'w', "v"="v"), selected = "airtemp")
    ),
    mainPanel(
      plotOutput("pairplot")
    )
  )
)

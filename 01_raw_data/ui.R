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
  h3("Pair Plots"),
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
  ),
  h3("Correlogram"),
  plotOutput("correlogram")
)

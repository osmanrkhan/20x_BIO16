#' lets the user select the site, season and date of the file
#' @param id a string to link input and output
#' @param datasets a named list of the columns of the data 
#' @param site a named list of possible sites that the user can choose 
#' @return a taglist of input
#' @import shiny
data_preview_var_ui <- function(id, datasets, site){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4,
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
             selectInput(ns("site"), h5(strong("Select site to view data")), choices = site, selected = site[[1]]),
             selectInput(ns("dataset"), h5(strong("Select the data set available for your site of interest")),
                         choices = datasets, selected = datasets[[1]]),
             
             #actionButton(inputId = ns("load"), "Click to load data")
             tags$ul(
               tags$li(tags$strong("comb_2018-06-05_2018-01-19"), ": Data set combining dates 06/05/2018 (summer) and 01/19/2018 (winter)"),
               tags$li(tags$strong("comb_2018-06-06_2018-01-20"), ": Data set combining dates 06/06/2018 (summer) and 01/20/2018 (winter)")
             )
      ),
      column(8,
             tableOutput(ns("preview"))
      )
    )   
  )
}

#' @title Constructs the path to the file chosen
#' @param id to link the input and the output
#' @param start_path starting path to be added to the full path
#' @return the loaded data
#' @import shiny 
#' @importFrom purrr map
data_preview_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session){
      #finding the path
      data <- reactive({
        req(input$dataset)
        filename <- glue("extdata/{site}_{dataset}.rds", site = input$site, dataset = input$dataset)
        readRDS(file = app_sys(filename)) 
      })
      # Getting head table 
      # TODO: Control number of lines 
      output$preview <- renderTable({
        utils::head(data(), 20)
      })
      return(data)
      #readRDS(file = path())
      #print(path())
    }
  )
}
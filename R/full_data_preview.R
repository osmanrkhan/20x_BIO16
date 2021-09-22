#' lets the user select the site, season and date of the file
#' @param id a string to link input and output
#' @return a taglist of input
#' @import shiny
full_data_preview_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4,
             h5("Using an Eddy Covariance solver (such as the ", 
                tags$a(href="https://www.licor.com/env/support/EddyPro/software.html", "EddyPro7"),
                "from LICOR Biosciences, who also provided the measurement devices), researchers can estimate CO2 flux and Net Ecosystem Exchange (NEE) from raw data. 
                          The most common approach is to solve the flux at each half-hour interval. Here in this section we have a data set for the same site (Silas Little) that has been processed 
                          using an Eddy Covariance solver. The data is for the entire year (instead of only one day), and has estimations every half hour. The full data includes the following variables:"),
             tags$ul(
               tags$li(tags$strong("date"),": Current date/time in POSIX format"),
               tags$li(tags$strong("JD"), ": Julian Day, a numeric counter from 1 until 365"),
               tags$li(tags$strong("NEE"),": Net ecosystem exchange"),
               tags$li(tags$strong("PPFD_in"),": Photosynthetically active radiation"),
               tags$li(tags$strong("TA"),": Air temperature"),
               tags$li(tags$strong("RH"),": Relative humidity"),
               tags$li(tags$strong("Vol.W.C"), ": Soil Moisture"),
               tags$li(tags$strong("Soil.T"), ": Soil Temperature"),
               tags$li(tags$strong("season"), ": Current season (either Winter (W) or Growing Season (GS))"),
               tags$li(tags$strong("timeofday"), ": Time of day (either Night or Day)")
             )
      ),
      column(8,
             tableOutput(ns("full_preview"))
      )
    )
  )
}
#' @title Server function for pre-loading data files
#' @param id to link the input and the output
#' @return the loaded data 
#' @import shiny
#' @importFrom utils head
full_data_preview_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session){
     
      full_data <- reactive({
        readRDS(file = app_sys("extdata/silas_little_2018.rds"))
      })
      
      # Getting head table 
      output$full_preview <- renderTable({
        utils::head(full_data(), 10)
      })
      return (full_data)
    }
  )
}

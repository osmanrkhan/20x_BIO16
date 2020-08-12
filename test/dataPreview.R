library(shiny)

#lets the user select the site, season and date of the file
#param @id: to link input and output
data_preview_var_ui <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(ns("site"), h3("Select Site"), choices = list("Silas Little" = "silas_little"), 
                selected = "silas_little"),
    selectInput(ns("season"), h3("Select season"),
                choices = list("summer", "winter"), selected = 1),
    selectInput(ns("date"), h3("Select date"), ""),
    actionButton(inputId = ns("load"), "Load Data")
  )
}

#' constructs the path to the file chosen
#' param @id: to link the input and the output
#' param @start_path: starting path to be added to the full path
data_preview_server <- function(id, start_path) {
  moduleServer(
    id,
    function(input, output, session){
      summer <- list("June 5th 2018" = "June_5_2018", "June 6th 2018" = "June_6_2018")
      winter <- list("Jan 19th 2018" = "Jan_19_2018", "Jan 20th 2018" = "Jan_20_2018")
      
      observe({
        if(grepl(input$season, "winter")){
          updateSelectInput(session, inputId = "date",
                            choices = winter, 
                            selected = winter[[1]])
        }
        else{
          updateSelectInput(session, inputId = "date",
                            choices = summer, 
                            selected = summer[[1]])
        }
      })
      
      #finding the path
      path <- reactive({
        req(input$date)
        glue("{start_path}{site}_{season}_{date}.rds", start_path = start_path, site = input$site, 
             season = input$season, date = input$date)
      })
      
      data <- eventReactive(eventExpr = input$load,{
        readRDS(path())
      })
      return(data)
    }
  )
}
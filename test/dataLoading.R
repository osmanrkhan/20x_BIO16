library(shiny)

#lets the user select the site, season and date of the file
#param @id: to link input and output
data_preview_var_ui <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(ns("dataset"), 
                h3("Select data Set"), 
                choices = list("First DataSet" = "comb_2018-06-05_2018-01-19",
                               "Second DataSet" = "comb_2018-06-06_2018-01-20" ), 
                selected = "First DataSet"),
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
      #finding the path
      path <- reactive({
        req(input$dataset)
        glue("{start_path}_{dataset}.rds", start_path = start_path, dataset = input$dataset)
      })
      
      data <- eventReactive(eventExpr = input$load,{
        readRDS(path())
      })
      return(data)
    }
  )
}
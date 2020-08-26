library(shiny)

#lets the user select the site, season and date of the file
#param @id: to link input and output
data_preview_var_ui <- function(id, datasets, site){
  ns <- NS(id)
  tagList(
    selectInput(ns("site"), h5(strong("Select site to view data")), choices = site, 
                selected = site[[1]]),
    selectInput(ns("dataset"), h3("Select data Set"), choices = datasets, selected = datasets[[1]]),
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
        glue("{start_path}{site}_{dataset}.rds", start_path = start_path, site = input$site, dataset = input$dataset)
      })
      
      data <- eventReactive(eventExpr = input$load,{
        readRDS(path())
      })
      return(data)
    }
  )
}
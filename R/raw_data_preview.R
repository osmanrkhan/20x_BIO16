library(shiny)

#' lets the user select the site, season and date of the file
#' param @id: a string to link input and output
#' @datasets: a named list of the columns of the data 
#' @site: a named list of possible sites that the user can choose 
#' @return a taglist of input
data_preview_var_ui <- function(id, datasets, site){
  ns <- NS(id)
  tagList(
    selectInput(ns("site"), h5(strong("Select site to view data")), choices = site, selected = site[[1]]),
    selectInput(ns("dataset"), h5(strong("Select the data set available for your site of interest")),
                choices = datasets, selected = datasets[[1]])
    #actionButton(inputId = ns("load"), "Click to load data")
  )
}

#' constructs the path to the file chosen
#' param @id: to link the input and the output
#' param @start_path: starting path to be added to the full path
#' @return the loaded data 
data_preview_server <- function(id, start_path) {
  moduleServer(
    id,
    function(input, output, session){
      #finding the path
      data <- reactive({
         req(input$dataset)
         path <- glue("{start_path}{site}_{dataset}.rds", start_path = start_path, site = input$site, 
              dataset = input$dataset)
         readRDS(path)
         
      })
      return(data)
      #readRDS(file = path())
      #print(path())
    }
  )
}
library(shiny)

#lets the user select the site, season and date of the file
#param @id: to link input and output
data_preview_var_ui <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(ns("site"), h5(strong("Select site to view data")), choices = list("Silas Little" = "silas_little"), 
                selected = "silas_little"),
    selectInput(ns("dataset"), h5(strong("Select the data set available for your site of interest")),
                choices = list("comb_2018-06-05_2018-01-19", "comb_2018-06-06_2018-01-20"), 
                selected = 1),
    actionButton(inputId = ns("load"), "Click to load data")
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
      
      #finding the path
      path <- reactive({
        req(input$dataset)
        glue("{start_path}{site}_{dataset}.rds", start_path = start_path, site = input$site, 
             dataset = input$dataset)
      })
      
      data <- eventReactive(eventExpr = input$load,{
        readRDS(path())
      })
      return(data)
    }
  )
}
#' lets the user select the variables to be graphed
#' param @id: to link ui and server
NEE_time_plot_ui <- function(id, y_variables, x_variables){
  ns <- NS(id)
  tagList(
    selectInput(ns("xvar"), h4("Select the time variable"), 
                choices = x_variables,
                selected = 1),
    selectInput(ns("yvar"), h4("Select your variable"), 
                choices = y_variables,
                selected = 1),
    actionButton(ns("load"), "Load Graph")
  )
}

#' returns a potly graph depending on which type of graph was selected
#' param @id: id that connects the ui to the server
#' param @data: the data to work with. a reactive variable
#' param @variables: the variables to graph to make nice axis 
NEE_time_plot_server <- function(id, data, x_variables, y_variables){
  moduleServer(
    id,
    function(input, output, session){
      
      sk_plt <- eventReactive(input$load,{
        form_ylabel <- names(y_variables)[which(y_variables == input$yvar)]
        form_xlabel <- names(x_variables)[which(x_variables == input$xvar)]
    
        x = pull(data, input$xvar)
        y = pull(data, input$yvar)
          
        plot <- plot_ly(type = "scatter", mode = "markers") %>% 
          add_markers(x = ~x  , y = ~y) %>% 
          layout(xaxis = list(title = form_xlabel), yaxis = list(title = form_ylabel), 
                 title = glue("{yvar} vs {xvar}", yvar = form_ylabel, xvar = form_xlabel))
        
      })
      
      return(sk_plt)         
      
    }
  )
}

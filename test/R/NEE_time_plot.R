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
        
        plt <- ggplot(data = data, aes_string( x = input$xvar, y=input$yvar))+ geom_point() +
          labs(x=form_xlabel, y = form_ylabel) + ggtitle(glue("{yvar} Vs {xvar}", xvar =  form_xlabel, 
                                                              yvar = form_ylabel)) +
        theme_bw() + theme(plot.title = element_text(hjust = 0.5))
        
      })
      
      return(sk_plt)         
      
    }
  )
}

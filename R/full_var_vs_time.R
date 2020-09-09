#' lets the user select the variables to be graphed
#' param @id: to link ui and server
full_time_plot_ui <- function(id, y_variables, x_variables){
  ns <- NS(id)
  tagList(
    selectInput(ns("xvar"), h5(strong("Select which time scale is of interest")), 
                choices = x_variables,
                selected = 1),
    selectInput(ns("yvar"), h5(strong("Select your variable")), 
                choices = y_variables,
                selected = 1),
    actionButton(ns("load"), "Load Graph")
  )
}

#' returns a potly graph depending on which type of graph was selected
#' param @id: id that connects the ui to the server
#' param @data: the data to work with. a reactive variable
#' param @variables: the variables to graph to make nice axis 
full_time_plot_server <- function(id, data, varlist){
  moduleServer(
    id,
    function(input, output, session){
      plot <- eventReactive(input$load,{
        ylab <- get_var_fullname(input$yvar, varlist)
        xlab <- get_var_fullname(input$xvar, varlist)
        plt <- ggplot(data = data, aes(x = !!sym(input$xvar), y = !!sym(input$yvar))) + theme_bw() + labs(y = ylab, x = xlab)
        if (input$xvar %in% c('season','timeofday')){
          plt + geom_violin(aes(fill = !!sym(input$xvar))) + scale_fill_discrete(c('blue', 'orange')) 
        } else {
          plt + geom_point(color = "orange", size = 1.5) + stat_smooth()
        }
      })
      return(plot)
    })
}

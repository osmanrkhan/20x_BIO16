#' lets the user select the variables to be graphed
#' param @id: to link ui and server
full_time_plot_ui <- function(id, y_variables, x_variables){
  ns <- NS(id)
  tagList(
    h3("Observing variables changing by different units of time"),
    sidebarLayout(
      sidebarPanel(
        h5("In this section you can observe how different meterological variables of interest change over time. 
                     You can plot changes by day as a count from 1 to 365, by season (Winter and Growing Season), by hour of day, or by time of day (Night vs Day)"),
        selectInput(ns("xvar"), h5(strong("Select which time scale is of interest")), 
                    choices = x_variables,
                    selected = 1),
        selectInput(ns("yvar"), h5(strong("Select your variable")), 
                    choices = y_variables,
                    selected = 1),
        actionButton(ns("load"), "Load Graph")
      ),
      mainPanel(
        plotOutput(ns("full_time_plot"))
      )
    )
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
      output$full_time_plot <- renderPlot({
        plot()
      })
    })
}

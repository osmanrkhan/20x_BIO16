library(shiny)

#' lets the user select the variables to be graphed
#' param @id: to link input and output
#' param @variables: a named list of variables to choose from
#' @return a taglist of input function
histogram_ui <- function(id, variables){
  ns <- NS(id)
  tagList(
    selectInput(ns("histogram_var"), h5(strong("Select variable to plot histogram")), 
                choices = variables ,
                selected = variables[[1]]),
    
    sliderInput(ns("histogram_bins"), h5(strong("Select the number of bins, higher bins means more
                                         'columns' and therefore a denser histogram")),
                min = 0, max = 100, value = 50),
    selectInput(ns("season"), h5(strong("Select season to plot. You can select 'summer', 'winter', 
                                 as well as seeing both plots overlaid using the 'both' option")), list("Summer" = "summer", 
                                                        "Winter" = "winter", 
                                                        "Both" = "both")),
    actionButton(ns("load_hist"), "Click to load graph")
  )
}

#' Plot the histogram for the file chosen
#' @param id: to link the input and the output 
#' @param variables: the variables to plot -- used to get nice labels on axes
#' @param data: the data to work with -- usually a reactive object
#' @return a plotly object 

histogram_server <- function(id, variables, data) {
  moduleServer(
    id,
    function(input, output, session){
      hist_plt <- eventReactive(input$load_hist,{
        req(data)
        
        form_label <- names(variables)[which(variables == input$histogram_var)] # formatted labels
        plt <- plot_ly(alpha = 0.6, nbinsx = input$histogram_bins)
        if (input$season == "summer"){
          plt <- add_histogram(plt, data %>% filter(season == "summer") %>% pull(!!input$histogram_var), 
                               name = "summer") 
        } else if (input$season == "winter"){
          plt <- add_histogram(plt, data %>% filter(season == "winter") %>% pull(!!input$histogram_var), 
                               name = "winter") 
        } else {
          plt <- add_histogram(plt, data %>% filter(season == "summer") %>% pull(!!input$histogram_var), 
                               name = "summer") 
          plt <- add_histogram(plt, data %>% filter(season == "winter") %>% pull(!!input$histogram_var), 
                               name = "winter") 
        }
        plt <- layout(plt, barmode = "overlay", title = glue("Histogram of {form_label}", form_label = form_label),
                      xaxis = list(title = "Values"), yaxis = list(title = "Percentages"))
        plt
      })
      return(hist_plt)
    }
  )
}

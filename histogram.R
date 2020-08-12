library(shiny)

#' lets the user select the variables to be graphed
#' param @id: to link input and output
histogram_ui <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(ns("histogram_var"), h4("Select box"), 
                choices = list( "Vertical Wind Speed" = "w" , 
                                "Horizontal Wind Speed (North)" = "v" , 
                                "Horizontal Wind Speed (East)" ="u",
                                "CO2" = "CO2",
                                "Water Vapor" = "H2O",
                                "Air Temperature" = "airtemp"),
                selected = "CO2"),
    
    sliderInput(ns("histogram_bins"), h4("number of bins"),
                min = 0, max = 100, value = 50),
    actionButton(ns("load_hist"), "Load Graph")
  )
}

#' Plot the histogram for the file chosen
#' param @id: to link the input and the output
#' param @variables: the variables to plot
#' param @data: the data to work with

histogram_server <- function(id, variables, data) {
  moduleServer(
    id,
    function(input, output, session){
      hist_plt <- eventReactive(input$load_hist,{
        req(data)
        
        form_label <- names(variables)[which(variables == input$histogram_var)] # formatted labels
        plt <- plot_ly(alpha = 0.6, nbinsx = input$histogram_bins) %>%
          add_histogram(pull(data,input$histogram_var), name = form_label, histnorm = "percent")
      })
      return(hist_plt)
    }
  )
}

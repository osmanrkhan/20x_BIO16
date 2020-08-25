#' lets the user select the variables to be graphed
#' param @id: to link ui and server
NEE_bivar_ui <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(ns("var"), h4("Select box"), 
                choices = list("Air Temperature" = "TA",
                               "Air Temperature Squared" = "TA.2",
                               "Photosynthetically Active Radiation" = "PPFD_in",
                               "Soil Moisture" = "Vol.W.C",
                               "Relative Humidity" = "RH"
                                ),
                selected = "Air Temperature"),
    radioButtons(ns("separation"), h4("Separate by"),
                 choices = list("Seasons" = "JD_c", "Daily" = "HM_c", "No Separation" = "none"),
                 selected = "none"),
    actionButton(ns("load"), "Load Graph")
  )
}

#' returns a potly graph depending on which type of graph was selected
#' param @id: id that connects the ui to the server
#' param @data: the data to work with. a reactive variable
#' param @variables: the variables to graph to make nice axis 
NEE_bivar_server <- function(id, data, variables){
  moduleServer(
    id,
    function(input, output, session){
     
  
      sk_plt <- eventReactive(input$load,{
        form_label <- names(variables)[which(variables == input$var)]
        
        if(grepl(input$separation, "none")){
          nee_var = "NEE"
          x = pull(data, input$var)
          y = pull(data, "NEE")
         
          plot <- plot_ly(type = "scatter", mode = "markers") %>% 
            add_markers(x = ~x  , y = ~y) %>% 
            layout(xaxis = list(title = form_label), yaxis = list(title = nee_var), title = form_label)
        }
        
        else{
          nee_var = "NEE"
          mytheme <- theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15)), 
                                legend.title = element_text(colour = "steelblue",  face = "bold.italic", family = "Helvetica"), 
                                legend.text = element_text(face = "italic", colour="steelblue4",family = "Helvetica"), 
                                axis.title = element_text(family = "Helvetica", size = (10), colour = "black"),
                                axis.text = element_text(family = "Courier", colour = "cornflowerblue", size = (10)))
          
          
          plt <- ggplot(data = data, aes_string(x = input$var, y="NEE", colour = input$separation)) +
            geom_point() + facet_grid(reformulate(input$separation)) + labs(x=form_label, y = "NEE") + 
            mytheme 
          plot <- ggplotly(plt)
        }
        plot
      })
      
      return(sk_plt)         
      
    }
  )
}

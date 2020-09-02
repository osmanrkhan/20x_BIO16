#' lets the user select the variables to be graphed
#' param @id: to link ui and server
#' param @variables: the variables to be graphed against NEE
#' param @separateby: the variables to separate
NEE_bivar_ui <- function(id, variables, separateby){
  ns <- NS(id)
  tagList(
    selectInput(ns("var"), h4("Select box"), 
                choices = variables[which(!variables %in% "NEE")],
                selected = 1),
    radioButtons(ns("separation"), h4("Separate by"),
                 choices = separateby,
                 selected = separateby[3]),
    actionButton(ns("load"), "Load Graph")
  )
}

#' returns a potly graph depending on which type of graph was selected
#' param @id: id that connects the ui to the server
#' param @data: the data to work with. a reactive variable
#' param @variables: the variables to graph to make nice axis 
#' param @separation_options: the separations options for title
#' return a plotly object
NEE_bivar_server <- function(id, data, variables, separation_options){
  moduleServer(
    id,
    function(input, output, session){
      
      sk_plt <- eventReactive(input$load,{
        
        form_label <- names(variables)[which(variables == input$var)]
        
        if(grepl(input$separation, "none")){
          plt <- ggplot(data = data, aes_string( x = input$var, y="NEE"))+ geom_point(color="#E69F00") +
            labs(x=form_label, y = "NEE") + ggtitle(glue("NEE Vs {yvar}", yvar = form_label)) +  theme_bw()+
            theme(plot.title = element_text(hjust = 0.5))
          
        }
        
        else{
          separation = names(separation_options)[which(!separation_options %in% input$separation)]
          nee_var = "NEE"
          plt <- ggplot(data = data[!is.na(pull(data,input$separation)),], aes_string(x = input$var, y="NEE", colour = input$separation)) +
            geom_point() + facet_grid(reformulate(input$separation)) + labs(x=form_label, y = "NEE") + 
            ggtitle(glue("NEE VS {yvar} by {sep}", yvar = form_label, sep = separation)) + theme_bw() +
            theme(legend.position="none", plot.title = element_text(hjust = 0.5))
            
          
        }
        plot <- ggplotly(plt)
        plot
      })
      
      return(sk_plt)         
      
    }
  )
}

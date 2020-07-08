library(shiny)
library(tidyverse)
library(rlang)
library(readr)
library(corrr)

function(input, output){
  # Getting the data in 
  data <- reactive({
    req(input$rawdata)
    as_tibble(read_csv(input$rawdata$datapath, n_max = input$n_row))
  })
  
  # Getting head table 
  output$preview <- renderTable({
    head(data())
  })
  
  # pair plot 
  plt <- reactive({ggplot(data(),
                          aes(x = !!sym(input$pairplot_xvar), 
                              y = !!sym(input$pairplot_yvar)))})
  output$pairplot <- renderPlot({
    plt() + geom_point() + geom_smooth()
  })
  
  # correlogram
  
}
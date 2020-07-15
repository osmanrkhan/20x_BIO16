library(shiny)
library(tidyverse)
library(rlang)
library(readr)
library(forecast)
library(plotly)
library(corrr)
library(zoo)


function(input, output,session){
  # Getting the data in 
  
  observe({
    if(grepl(input$season, "winter")){
      updateSelectInput(session, inputId = "file",
                        choices = list("jan 19 2018" =  "jan_19_2018", "jan 20 2018"="jan_20_2018"), 
                        selected = "jan_19_2018")
    }
    else{
      updateSelectInput(session, inputId = "file",
                        choices = list("june 5 2018" =  "june_5_2018", "june 6 2018"="june_6_2018"), 
                        selected = "june_5_2018")
    }
  })
  
  path <- reactive({
    req(input$file)
    paste("../data/processed_data/silas_little_", 
          input$season, "_", input$file, ".rds", sep="")
  })
  
 # output$checking <-renderText({
  #  path()
   # })
  data <- reactive({
    as_tibble(rollmean(readRDS(path()),as.numeric(as.character(input$window_size)), align = "center"))
  });
   
  
  # Getting head table 
  output$preview <- renderTable({
    head(data())
  })
  
  
  # Getting histogram
  output$histogram <- renderPlotly({
    req(data())
    switch(input$histogram_var,
           "CO2" = plot_ly(alpha = 0.6, nbinsx = input$histogram_bins) %>% 
             add_histogram(data()$CO2, name = "CO2", histnorm = "percent")%>% 
             layout(title = "Histogram of CO2", xaxis = list(title = "CO2"),yaxis = list(title = "percent")),
           
           "u" = plot_ly(alpha = 0.6, nbinsx = input$histogram_bins) %>% 
             add_histogram(data()$u, name = "horizontal speed", histnorm = "percent")%>%
             layout(title = "Histogram of horizontal speed", xaxis = list(title = "U"),
                    yaxis = list(title = "percent")),
           
           "v" = plot_ly(alpha = 0.6, nbinsx = input$histogram_bins) %>% 
             add_histogram(data()$v, name = "horizontal speed", histnorm = "percent")%>% 
             layout(title = "Histogram of horizontal speed", xaxis = list(title = "V"),
                    yaxis = list(title = "percent")),
           
           "w" = plot_ly(alpha = 0.6, nbinsx = input$histogram_bins) %>% 
             add_histogram(data()$w, name = "vertical speed", histnorm = "percent")%>% 
             layout(title = "Histogram of vertical speed", xaxis = list(title = "vertical speed"),
                    yaxis = list(title = "percent")),
           
           "airtemp" = plot_ly(alpha = 0.6, nbinsx = input$histogram_bins) %>% 
             add_histogram(data()$airtemp, name = "Temperature", histnorm = "percent")%>% 
             layout(title = "Histogram of air Temperature", xaxis = list(title = "airTemp"),
                    yaxis = list(title = "percent")),
           
           "H2O" = plot_ly(alpha = 0.6, nbinsx = input$histogram_bins) %>% 
             add_histogram(data()$H2O, name = "Water", histnorm = "percent")%>% 
             layout(title = "Histogram of H2O", xaxis = list(title = "H2O"),yaxis = list(title = "percent")),
    )
  })
  

  t <- reactiveValues(data = NULL)
  unit <- reactiveValues(data = NULL);
  
  observeEvent(input$sec, {
    t$data <- seq(0,86400, length = length(data()$CO2))
    unit$data <- "seconds"
  })
  
  observeEvent(input$min, {
    t$data <- seq(0,86400, length = length(data()$CO2))/60
    unit$data <- "minutes"
  })
  
  observeEvent(input$hour, {
    t$data <- seq(0,86400, length = length(data()$CO2))/3600
    unit$data <- "hours"
  })  
  
  output$vvt_plt_vs_time <- renderPlotly({
    
    req(data())
    if (is.null(t$data) || length(t$data) != length(data()$CO2)) return()
    if (is.null(unit$data)) return()
    switch(input$vvt,
           "CO2" =  plot_ly(x = ~t$data,  y = ~data()$CO2, type = "scatter", mode = "lines") %>% 
             layout(title = as.character(num_row), yaxis = list(title = "CO2"),xaxis = 
                      list(title = paste("time(", unit$data,")", sep = ""))),
           
           "airtemp" = plot_ly(x = ~t$data,  y = ~data()$airtemp, type = "scatter", mode = "lines") %>% 
             layout(title = "Temperature Vs Time", yaxis = list(title = "Temperature"),xaxis = 
                      list(title = paste("time(", unit$data,")", sep = ""))),
           
           "H2O" = plot_ly(x = ~t$data,  y = ~data()$H2O, type = "scatter", mode = "lines") %>% 
             layout(title = "H2O Vs Time", yaxis = list(title = "H2O"),xaxis = 
                      list(title = paste("time(", unit$data,")", sep = ""))),
           
           "u" = plot_ly(x = ~t$data,  y = ~data()$u, type = "scatter", mode = "lines") %>% 
             layout(title = "Horizontal Wind Speed (East) Vs Time", 
                    yaxis = list(title = "Horizontal Wind Speed (East)"),xaxis = 
                      list(title = paste("time(", unit$data,")", sep = ""))),
           
           "v" = plot_ly(x = ~t$data,  y = ~data()$v, type = "scatter", mode = "lines") %>% 
             layout(title = "Horizontal Wind Speed (North) Vs Time", 
                    yaxis = list(title = "Horizontal Wind Speed (North)"),xaxis = 
                      list(title = paste("time(", unit$data,")", sep = ""))),
           
           "w" = plot_ly(x = ~t$data,  y = ~data()$w, type = "scatter", mode = "lines") %>% 
             layout(title = "VerticalWind Speed Vs Time", 
                    yaxis = list(title = "vertical Wind Speed"),xaxis = 
                      list(title = paste("time(", unit$data,")", sep = ""))),
    )
  })
  # pair plot 
  # TODO: Add functionality to 
  plt <- reactive({ggplot(data(),
                          aes(x = !!sym(input$pairplot_xvar), 
                              y = !!sym(input$pairplot_yvar)))})
  output$pairplot <- renderPlot({
    plt() + geom_point() + geom_smooth()
  })
}
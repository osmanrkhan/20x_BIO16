library(shiny)
library(tidyverse)
library(rlang)
library(readr)
library(forecast)
library(plotly)

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
  
  
  # Getting histogram
  output$histogram <- renderPlot({
    hist(data()$CO2)
    switch(input$histogram_var,
           "CO2" = hist(data()$CO2, breaks = input$histogram_bins + 1,col = "#75AADB", border = "white"),
           "u" = hist(data()$u, breaks = input$histogram_bins + 1,col = "#75AADB", border = "white"),
           "v" = hist(data()$v, breaks = input$histogram_bins + 1,col = "#75AADB", border = "white"),
           "w" = hist(data()$w, breaks = input$histogram_bins + 1,col = "#75AADB", border = "white"),
           "airtemp" = hist(data()$airtemp, breaks = input$histogram_bins + 1,col = "#75AADB", border = "white"))
  })

  output$vvt_plt <- renderPlot({
      col_val=c()
      var_data = c()
      col_list=c("blue", "red","black")
      for(i in 1:length(input$vvt_wind)){
        col_val = c(col_val, col_list[grep(input$vvt_wind[i], names(data()))])
        var_data = c(var_data, data()[grep(input$vvt_wind[i], names(data()))])
      }
      
      t<-seq(0,input$n_row, length = input$n_row)
      var_data[[1]] = var_data[[1]][!is.na(var_data[[1]])]
      plot(t,  var_data[[1]], xlab="time(hours)", ylab = "wind", type="s",
           col=col_val[1])
      for(i in 1:length(var_data)){
        var_data[[i]] = var_data[[i]][!is.na(var_data[[i]])]
        lines(t,var_data[[i]], col=col_val[i])
      }
      
      legend("bottomright", input$vvt_wind, col=col_val, bg= "gray93")
  })
  plt <- reactive({ggplot(data(),
                          aes(x = !!sym(input$pairplot_xvar), 
                              y = !!sym(input$pairplot_yvar)))})
  output$pairplot <- renderPlot({
    plt() + geom_point() + geom_smooth()
  })
}
library(shiny)
library(tidyverse)
library(rlang)
library(readr)
library(corrr)
library(plotly)
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

  output$vvt_plt <- renderPlotly({
    req(data())
      col_val=c()
      var_data = c()
      col_list=c("blue", "red","black")
      for(i in 1:length(input$vvt_wind)){
        col_val = c(col_val, col_list[grep(paste("^", input$vvt_wind[i],"$", sep = ""), names(data()))])
        var_data = c(var_data, data()[grep(paste("^", input$vvt_wind[i],"$", sep = ""), names(data()))])
      }
      num_row = length(data()$u)
      time_divisor <- 1;
      if(grepl("min", input$vvt_time)){
        time_divisor <- 60;
      }
      else if(grepl("hours", input$vvt_time)){
        time_divisor <- 3600;
      }
      t<-seq(0,86400, length = num_row)/time_divisor
      
      if(length(var_data) > 0){
        var_data[[1]] = var_data[[1]][!is.na(var_data[[1]])]
        fig <- plot_ly(x = ~t,  y = ~var_data[[1]], type = "scatter", mode = "lines",
                       color = I(col_list[1]), name = input$vvt_wind[[1]])
        for(i in min(2, length(var_data)):length(var_data)){
          if(i > 1) {
            var_data[[i]] = var_data[[i]][!is.na(var_data[[i]])]
            fig <- fig %>% add_trace(x = ~t, y = ~var_data[[i]], type = "scatter", mode = "lines",
                                   color = I(col_list[i]), name = input$vvt_wind[[i]])
          }
        
        }
        fig <- fig %>% layout(title = "wind versus time",
                xaxis = list(title = paste("time(", input$vvt_time,")", sep ="")),
                yaxis = list(title = "Wind"))
      }
  })
  
  output$vvt_plt_vs_time <- renderPlotly({
    req(data())
    num_row = length(data()$CO2)
    time_divisor <- 1;
    if(grepl("min", input$v_time)){
      time_divisor <- 60;
    }
    else if(grepl("hours", input$v_time)){
      time_divisor <- 3600;
    }
    t<-seq(0,84400, length = num_row)/time_divisor
   
    
    switch(input$vvt,
           "CO2" =  plot_ly(x = ~t,  y = ~data()$CO2, type = "scatter", mode = "lines") %>% 
             layout(title = "CO2 Vs Time", yaxis = list(title = "CO2"),xaxis = 
                      list(title = paste("time(", input$v_time,")", sep = ""))),
           
           "airtemp" = plot_ly(x = ~t,  y = ~data()$airtemp, type = "scatter", mode = "lines") %>% 
             layout(title = "Temperature Vs Time", yaxis = list(title = "Temperature"),xaxis = 
                      list(title = paste("time(", input$v_time,")", sep = ""))),
           
           "H2O" = plot_ly(x = ~t,  y = ~data()$H2O, type = "scatter", mode = "lines") %>% 
             layout(title = "H2O Vs Time", yaxis = list(title = "H2O"),xaxis = 
                      list(title = paste("time(", input$v_time,")", sep = ""))),
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
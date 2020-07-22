library(shiny)
library(tidyverse)
library(rlang)
library(readr)
library(forecast)
library(plotly)
library(glue)


function(input, output, session){
  # Reactive input files for dates 
  summer <- list("June 5th 2018" = "June_5_2018", "June 6th 2018" = "June_6_2018")
  winter <- list("Jan 19th 2018" = "Jan_19_2018", "Jan 20th 2018" = "Jan_20_2018")
  
  # List of variables and their more formatted names for easy plotting
  variables <- list("Vertical Wind Speed" = "w" , 
                  "Horizontal Wind Speed (North)" = "v" , 
                  "Horizontal Wind Speed (East)" ="u",
                  "CO2" = "CO2",
                  "Water Vapor" = "H2O",
                  "Air Temperature" = "airtemp")
  
  # observe the input$season to see which dates to see
  observe({
    if(grepl(input$season, "winter")){
      updateSelectInput(session, inputId = "date",
                        choices = winter, 
                        selected = winter[[1]])
    }
    else{
      updateSelectInput(session, inputId = "date",
                        choices = summer, 
                        selected = summer[[1]])
    }
  })
  
  path <- reactive({
    req(input$date)
    glue("data/processed_data/{site}_{season}_{date}.rds", site = input$site, 
         season = input$season, date = input$date)
  })
  
  # load data on push load_data
  data <- eventReactive(eventExpr = input$load,{
      readRDS(path())
  })
  
  # Getting head table 
  # TODO: Control number of lines 
  output$preview <- renderTable({
    head(data(), 15)
  })

  
  # Histogram plots  
  hist_plt <- eventReactive(input$load_hist,{
    req(data())
    form_label <- names(variables)[which(variables == input$histogram_var)] # formatted labels
    plt <- plot_ly(alpha = 0.6, nbinsx = input$histogram_bins) %>%
      add_histogram(pull(data(),input$histogram_var), name = form_label, histnorm = "percent") %>%
      layout(title = glue("Histogram of {var}", var = form_label), 
             xaxis = list(title = form_label),
             yaxis = list(title = "Percent"), bargap = 0.1)
    plt
  })
  
  output$histogram <- renderPlotly({
    hist_plt()
  })
  
  # Time plots 
  #TODO: move form_label into reactive element 
  
  skeleton_plt <- reactive({
    form_label <- names(variables)[which(variables == input$vvt)]
    plt <- plot_ly(type = "scatter", mode = "lines") %>% 
      layout(title = glue("{var} vs Time", var = form_label, yaxis = list(title = form_label)))
    plt
  })
  

  # Observe the radio buttons
  vvt_labs <- reactive({
    if (input$timescale == "sec"){
      labs <- list(units = "seconds", scale = 1, x = data()$second)
    } else if (input$timescale == "min"){
      labs <- list(units = "minutes", scale = 60, x = data()$minute)
    } else if (input$timescale == "hr"){
      labs <- list(units = "hours", scale = 3600, x = data()$hour)
    }
    return(labs)
  })
  
  # Plotting
  vvt_plt <- eventReactive(input$load_vvt,{
    sk_plt <- skeleton_plt() %>% layout(xaxis = list(title = glue("Time ({unit})", 
                                                                  unit = vvt_labs()$units))) 
    for (i in 1:length(input$vvt)){
      series <- names(variables)[which(variables == input$vvt[i])]
      y <- forecast::ma(x = pull(data(), input$vvt[i]), order = vvt_labs()$scale)
      x <- vvt_labs()$x
      sk_plt <- sk_plt %>% add_lines(x = x, y = y, name = series)
    }
    return(sk_plt)
  })
  
  
  output$vvt_plt_vs_time <- renderPlotly({
    vvt_plt()
  })
  
  
  # pair plot 
  
  pairplot <- eventReactive(input$load_pair,{
    form_label_x <- names(variables)[which(variables == input$pairplot_xvar)]
    form_label_y <- names(variables)[which(variables == input$pairplot_yvar)]
    time <- rep(1:48, each = 1800)/2
    p <-  plot_ly(x = ~pull(data(), input$pairplot_xvar), 
                               y = ~pull(data(), input$pairplot_yvar), frame = ~time, 
                  mode = "markers", type = "scatter") %>%
                        layout(xaxis = list(title = form_label_x), yaxis = list(title = form_label_y),
                               title = glue("{xaxis} vs {yaxis}", xaxis = form_label_x, yaxis = form_label_y))
    p<- p %>%
      animation_slider(
        currentvalue = list(prefix = "time: ", font = list(color="red"), suffix = " hours")
      )
  })
  
  cov <- reactive({
    as.data.frame(cov(data))
  })
  covarplot <- eventReactive(input$load_pair, {
    form_label_x <- names(variables)[which(variables == input$pairplot_xvar)]
    form_label_y <- names(variables)[which(variables == input$pairplot_yvar)]
    time <- rep(1:48, each = 1800)/2
    p <-  plot_ly(x = ~pull(data(), input$pairplot_xvar), 
                  y = ~pull(data(), input$pairplot_yvar), frame = ~time, 
                  mode = "markers", type = "scatter") %>%
      layout(xaxis = list(title = form_label_x), yaxis = list(title = form_label_y),
             title = glue("{xaxis} vs {yaxis}", xaxis = form_label_x, yaxis = form_label_y))
    p<- p %>%
      animation_slider(
        currentvalue = list(prefix = "time: ", font = list(color="red"), suffix = " hours")
      )
  })
  
  output$pairplot <- renderPlotly({
    pairplot()
  })
}
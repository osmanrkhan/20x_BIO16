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
    glue("../data/processed_data/{site}_{season}_{date}.rds", site = input$site, 
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

  #data <- reactive({
  #  as_tibble(rollmean(readRDS(path()),as.numeric(as.character(input$window_size)), align = "center"))
  #});
  
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
  
  # Control what to plot depending on what buttons were pushed 
  axes_timeplt <- reactiveValues(x = NULL,y = NULL,xlab = NULL, ylab = NULL) 
  # Observe the action buttons 
  observeEvent(input$sec,{
   form_label <- names(variables)[which(variables == input$vvt)]
   axes_timeplt$x <- data()$second
   axes_timeplt$y <- pull(data(), input$vvt)
   axes_timeplt$xlab <- glue("Time ({unit})", unit = "seconds")
   axes_timeplt$ylab <- form_label
  })
  observeEvent(input$min,{
    form_label <- names(variables)[which(variables == input$vvt)]
    axes_timeplt$x <- data()$minute
    axes_timeplt$y <- forecast::ma(pull(data(), input$vvt),60)
    axes_timeplt$xlab <- glue("Time ({unit})", unit = "minutes")
    axes_timeplt$ylab <- glue("Moving Average {lab}", lab = form_label)
  })
  observeEvent(input$hour,{
    form_label <- names(variables)[which(variables == input$vvt)]
    axes_timeplt$x <- data()$hour
    axes_timeplt$y <- forecast::ma(pull(data(), input$vvt),3600)
    axes_timeplt$xlab <- glue("Time ({unit})", unit = "hours")
    axes_timeplt$ylab <- glue("Moving Average {lab}", lab = form_label)
  })
  
  
  output$vvt_plt_vs_time <- renderPlotly({
    if(is.null(axes_timeplt$x) | is.null(axes_timeplt$y)) return()
    form_label <- names(variables)[which(variables == input$vvt)]
    skeleton_plt() %>% add_lines(x = axes_timeplt$x, y = axes_timeplt$y, name = form_label) %>%
      layout(xaxis = list(title = axes_timeplt$xlab), yaxis = list(title = axes_timeplt$ylab))
  })

  # pair plot 
  
  
  pairplot <- eventReactive(input$load_pair,{
    form_label_x <- names(variables)[which(variables == input$pairplot_xvar)]
    form_label_y <- names(variables)[which(variables == input$pairplot_yvar)]
    p <- plot_ly() %>% add_trace(x = pull(data(), input$pairplot_xvar), 
                               y = pull(data(), input$pairplot_yvar), mode = "markers", type = "scattergl") %>%
                        layout(xaxis = list(title = form_label_x), yaxis = list(title = form_label_y),
                               title = glue("{xaxis} vs {yaxis}", xaxis = form_label_x, yaxis = form_label_y))
  })
  
  output$pairplot <- renderPlotly({
    pairplot()
  })
  #plt <- reactive({ggplot(data(),
  #                        aes(x = !!sym(input$pairplot_xvar), 
  #                            y = !!sym(input$pairplot_yvar)))})
  #output$pairplot <- renderPlot({
  #  plt() + geom_point() + geom_smooth()
  #})
}
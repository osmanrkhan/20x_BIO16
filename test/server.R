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
  
  
  # plot_ly animation
  
  pairplot <- eventReactive(input$load_pair,{
    form_label_x <- names(variables)[which(variables == input$pairplot_xvar)]
    form_label_y <- names(variables)[which(variables == input$pairplot_yvar)]
    
    frame = as.integer(input$frame)
    each = 3600/frame
    time <- round(rep(1:(24*frame), each = each)/frame, 2)

    mod = c()
    for(i in 1:(24*frame)){
      x = pull(data(), input$pairplot_xvar)[seq((i-1)*(each) + 1, length = each)]
      y = pull(data(), input$pairplot_yvar)[seq((i-1)*(each) + 1, length = each)]
      mod = c(mod, fitted(lm(y ~ x)))
    }
    
    p <-  plot_ly(x = ~pull(data(), input$pairplot_xvar), 
                  y = ~pull(data(), input$pairplot_yvar), frame = ~time, 
                  mode = "markers", type = "scatter", name = "data values")  %>%
      add_lines(x = ~pull(data(), input$pairplot_xvar), y= ~mod, frame = ~time, name = "Regression Line") %>%
                        layout(xaxis = list(title = form_label_x), yaxis = list(title = form_label_y),
                               title = glue("{xaxis} vs {yaxis}", xaxis = form_label_x, yaxis = form_label_y))
    p<- p  %>% animation_opts(frame = 1500, transition = 1000) %>%
      animation_slider(
        currentvalue = list(prefix = "time: ", font = list(color="red"), suffix = " hours")
      )
  })
  
  cov <- reactive({
    as.data.frame(cov(data))
  })
  
  #using sliders. Not animated but refocus on each step
  #issues covariance not working (probaly minor bug)
  
  covarplot <- eventReactive(input$load_pair, {
    form_label_x <- names(variables)[which(variables == input$pairplot_xvar)]
    form_label_y <- names(variables)[which(variables == input$pairplot_yvar)]
    
    frame = as.integer(input$frame)
    each = 3600/frame
    aval <- list()
    mod <- list()
    max_num = 24*frame
    for(step in 1:max_num){
      x=pull(data(), input$pairplot_xvar)[seq((step-1)*(each) + 1, length = each)]
      y=pull(data(), input$pairplot_yvar)[seq((step-1)*(each) + 1, length = each)]
      aval[[step]] <-list(visible = FALSE,
                          name = paste0('h = ', round(step/frame, 3)),
                          x=x,
                          y=y)
      covar = 0.3 # cov(x, y = y, method = "pearson")
      mod[[step]] <-list(visible = FALSE,
                         name = paste0('covariance = ', round(covar, 2)),
                         x=x,
                         y=fitted(lm(y ~ x)))
    }
    aval[1][[1]]$visible = TRUE
    mod[1][[1]]$visible = TRUE
    
    # create steps and plot all traces
    steps <- list()
    fig <- plot_ly()
    for (i in 1:max_num) {
      fig <- add_markers(fig,x=aval[i][[1]]$x,  y=aval[i][[1]]$y, visible = aval[i][[1]]$visible, 
                         name = aval[i][[1]]$name, type = 'scatter', marker  = list(color = 'royalblue'), hoverinfo = 'name')
      name = " hours"
      if(i<=frame){
        name = " hour"
      }
      
      step <- list(args = list('visible', rep(FALSE, length(aval))), label =  paste0(round( i/frame, 2), name),
                   method = 'restyle')
      step$args[[2]][i] = TRUE  
      steps[[i]] = step 
    }  
    for(i in 1:max_num){
      fig <- add_lines(fig,x=mod[i][[1]]$x,  y=mod[i][[1]]$y, visible = mod[i][[1]]$visible, 
                       name = mod[i][[1]]$name, type = 'scatter', 
                       line  = list(color = 'firebrick', width = 4), hoverinfo = 'name')
    }
    
    # add slider control to plot
    fig <- fig %>%
      layout(xaxis = list(title = form_label_x), yaxis = list(title = form_label_y),
             title = glue("{xaxis} vs {yaxis}", xaxis = form_label_x, yaxis = form_label_y),
             sliders = list(list(active = 1, currentvalue = list(prefix = "Frequency: "),
                                 steps = steps)))
    
  })
  
  output$pairplot <- renderPlotly({
     pairplot()
  })
}
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
  
  # function to validate vvt input  
  vvt_selection <- function(input){
    wind_variables <- c("u", "v", "w")
    non_wind <- which(!input %in% wind_variables) # index of non wind variables 
    wind <- which(input %in% wind_variables)
    if (length(non_wind) > 1 && length(wind) >= 1){ # if there is a wind variable and more than one non-wind
      return("Can't choose more than one meterologcial variable alongside wind variables")
    } else if (length(wind) == 0 && length(non_wind) > 2) { # if there are more than 2 non-wind variables with no wind 
      return("Can't choose more than two meterological variables at the same time")
    } else {
      return(NULL)
    }
  }
  
  # Plotting
  vvt_plt <- eventReactive(input$load_vvt,{
    validate(
      vvt_selection(input$vvt) # validating input$vvt
    )
    
    wind_variables <- c('u', 'v', 'w') # which variables are wind 
    
    sk_plt <- skeleton_plt() %>% layout(xaxis = list(title = glue("Time ({unit})", 
                                                                  unit = vvt_labs()$units))) 
    
    # wind and non-wind inputs
    non_winds <- which(!input$vvt %in% wind_variables)
    wind <- which(input$vvt %in% wind_variables)
    
    if (length(wind) >= 1 && length(non_winds) > 0){ # if there is one wind variable 
      non_wind_series <- names(variables)[which(variables == input$vvt[non_winds])]
      y1 <- list(side = "left", title = "Wind Speed")
      y2 <- list(side = "right", overlaying = "y", title = non_wind_series)
      for (i in 1:length(wind)){
        wind_series <- series <- names(variables)[which(variables == input$vvt[wind[i]])]
        y <- forecast::ma(pull(data(), input$vvt[wind[i]]), order = vvt_labs()$scale)
        x <- vvt_labs()$x
        sk_plt <- sk_plt %>% add_lines(x = x, y = y, name = wind_series)
      }
      y <- forecast::ma(pull(data(), input$vvt[non_winds]), order = vvt_labs()$scale)
      sk_plt <- sk_plt %>% add_lines(x = x, y = y, name = non_wind_series, yaxis = "y2")
      sk_plt <- sk_plt %>% layout(yaxis2 = y2, yaxis = y1)
    } else if (length(wind) == 0  && length(non_winds) > 0) {
      series_1 <- names(variables)[which(variables == input$vvt[non_winds[1]])]
      series_2 <- names(variables)[which(variables == input$vvt[non_winds[2]])]
      y1 <- list(side = "left", title = series_1)
      y2 <- list(side = "right", overlaying = "y1", title = series_2)
      for (i in 1:length(non_winds)){
        y <- forecast::ma(pull(data(), input$vvt[non_winds[i]]), order = vvt_labs()$scale)
        x <- vvt_labs()$x
        sk_plt <- sk_plt %>% add_lines(x = x, y = y, name = get(glue("series_{num}", num = i)), 
                                       yaxis = glue("y{num}", num = i))
      }
      sk_plt <- sk_plt %>% layout(yaxis2 = y2, yaxis = y1)
    } else if (length(wind) > 0 && length(non_winds) == 0){
      for (i in 1:length(wind)){
        wind_series <- series <- names(variables)[which(variables == input$vvt[wind[i]])]
        y <- forecast::ma(pull(data(), input$vvt[wind[i]]), order = vvt_labs()$scale)
        x <- vvt_labs()$x
        sk_plt <- sk_plt %>% add_lines(x = x, y = y, name = wind_series)
      }
      sk_plt <- sk_plt %>% layout(yaxis = list(title = "Wind Speed"))
    } else {
      sk_plt <- NULL
    }
    return(sk_plt)
  })
  
  
  output$vvt_plt_vs_time <- renderPlotly({
    vvt_plt()
  })
  
  # calculating the covariance 
  cov <- reactive({
    as.data.frame(cov(data))
  })
  
  # pair plot reactive values
  pairplot <- reactiveValues(plot = NULL)
  
  # Observe event for pushing one of the buttons
  observeEvent(input$load_time,{
    form_label_x <- names(variables)[which(variables == input$pairplot_xvar)] # label for x axis
    form_label_y <- names(variables)[which(variables == input$pairplot_yvar)] # label for y axis
    
    frame <- as.integer(input$frame) # what is the time frame
    each <- 3600/frame # divide hour by time frame 
    aval <- list()
    mod <- list()
    max_num = 24*frame # 24 times number of frames
    for(step in 1:max_num){ # for each step 
      # get the data at each time step
      x <- pull(data(), input$pairplot_xvar)[seq((step-1)*(each) + 1, length = each)]  
      y <- pull(data(), input$pairplot_yvar)[seq((step-1)*(each) + 1, length = each)]
      aval[[step]] <-list(visible = FALSE,
                          name = paste0('h = ', round(step/frame, 3)),
                          x=x,
                          y=y)
      covar <- stats::cov(x = x,y = y, method = "pearson")
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
      
      step <- list(args = list('visible', rep(FALSE, length(aval))), 
                   label =  paste0(round( i/frame, 2), name),
                   method = 'restyle')
      step$args[[2]][i] = TRUE  
      steps[[i]] = step 
    }  
    for(i in 1:max_num){
      fig <- add_lines(fig,x=mod[i][[1]]$x,  y=mod[i][[1]]$y, visible = mod[i][[1]]$visible, 
                       name = mod[i][[1]]$name, type = 'scattergl', 
                       line  = list(color = 'firebrick', width = 4), hoverinfo = 'name')
    }
    
    # add slider control to plot
    fig <- fig %>%
      layout(xaxis = list(title = form_label_x), yaxis = list(title = form_label_y),
             title = glue("{xaxis} vs {yaxis}", xaxis = form_label_x, yaxis = form_label_y),
             sliders = list(list(active = 1, currentvalue = list(prefix = "Frequency: "),
                                 steps = steps)))
    pairplot$plot <- fig
  })
  
  observeEvent(input$load_all, {
    form_label_x <- names(variables)[which(variables == input$pairplot_xvar)] # label for x axis
    form_label_y <- names(variables)[which(variables == input$pairplot_yvar)] # label for y axis
    # getting data using pull()
    x <- pull(data(),input$pairplot_xvar)
    y <- pull(data(), input$pairplot_yvar)
    mod <- lm(y ~ x) # linear model for trendline
    fig <- plot_ly() %>% 
      layout(title = glue("{xvar} vs {yvar}", xvar = form_label_x, yvar = form_label_y), 
             xaxis = list(title = form_label_x), yaxis = list(title = form_label_y)) # getting plot labels
    fig <- fig %>% add_lines(x = x, y = fitted(mod), name = glue("Correlation: {value}", value = round(stats::cor(x,y),2)), 
                             line = list(color = "firebrick", width = 4)) %>% 
      add_trace(x = x, y = y, type = "scattergl", name = "Bivariate", 
                             marker = list(color = "royalblue"))
       # adding the bivariate scatter plot and the trend line  
    pairplot$plot <- fig
  })

  # render pairplot
  output$pairplot <- renderPlotly({
    suppressWarnings(pairplot$plot)
  })
}
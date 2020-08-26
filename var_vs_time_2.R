library(shiny)
library(dplyr)

#' lets the user select the variables to be graphed
#' @param id: to link input and output
plot_var_vs_time_ui <- function(id) {
  ns <- NS(id)
  # assemble UI elements
  tagList(
    h5("Select variable to plot versus time"),
    checkboxGroupInput("vvt",
                       choices = list( "Vertical Wind Speed" = "w" , 
                                       "Horizontal Wind Speed (North)" = "v" , 
                                       "Horizontal Wind Speed (East)" ="u",
                                       "CO2" = "CO2",
                                       "Water Vapor" = "H2O",
                                       "Air Temperature" = "airtemp"),
                       selected = "CO2"),
    selectInput("season", h5(strong("Selecting Season")),
                choices = list("Summer" = 1, "Winter" = 2, "Both" = 3)),
    actionButton("load_vvt", "Load graph")
  )
}

plot_var_vs_time_server <- function(id, variables, data){
  moduleServer(
    id,
    function(input, output, session){
      sk_plt <- plot_ly(data = data) %>%
        layout(title = "Variable vs Time", xaxis = list(title = "Time"))
      
      vvt_plt <- eventReactive(input$load_vvt,{
        # validate selection so that there is only one non-wind variable if there are wind variables, 
        # and that there can only be 2 non-wind variables at max if there are no wind variables
        validate(vvt_selection(input$vvt))
        
        if (length(input$vvt) == 1){
          series <- get_var_fullname(input$vvt)
          y_dat <- pull(data, input$vvt)
          sk_plt <- sk_plt %>% add_lines(x = ~time, y = y_dat, name = series)
        } else {
          # name of wind variables 
          wind_variables <- c('u','v','w')
          
          # index for wind variables and non wind variables 
          non_winds <- which(!input$vvt %in% wind_variables)
          winds <- which(input$vvt %in% wind_variables)
          
          # if there is more than 2 wind and more than 1 non_wind, set 2 axes
          if (length(winds) >= 1 && length(non_winds) > 0){
            # for each wind variable, add a line corresponding
            for (i in 1:length(winds)){
              series_name <- get_var_fullname(input$vvt[winds][i])
              y_dat <- pull(data, input$vvt[winds][i])
              sk_plt <- sk_plt %>% add_lines(y = y_dat, name = series_name, x = ~time) 
            }
            non_wind_series <- names(variables)[which(variables == input$vvt[non_winds])]
            non_wind_dat <- pull(data, input$vvt[non_winds][1]) # get y-axis
            sk_plt <- sk_plt %>% add_lines(y = non_wind_dat, x = ~time, yaxis = "y2", name = non_wind_series)
            y_axis1 <- list(side = "left", title = "Wind Speed (m/s)")
            y_axis2 <- list(side = "right", overlaying = "y", title = non_wind_series)
            sk_plt <- sk_plt %>% layout(yaxis2 = y_axis2, yaxis = y_axis1)
          } else if (length(winds) == 0 && length(non_winds) > 0){
            # this situation is when there are no wind variables and maximum 2 non-wind variables 
            # getting the name of the series 
            series_1 <- get_var_fullname(input$vvt[non_winds][1])
            series_2 <- get_var_fullname(input$vvt[non_winds][2])
            # adding plots one at a time 
            sk_plt <- sk_plt %>% add_lines(y = pull(data, input$vvt[non_winds][1]), x=~time, name = series_1) %>%
              add_lines(y = pull(data, input$vvt[non_winds][2]), x = ~time, yaxis = "y2", name = series_2)
            # setttings for the two axes 
            y1 <- list(side = "left", title = series_1)
            y2 <- list(side = "right", overlaying = "y", title = series_2)
            sk_plt <- sk_plt %>% layout(yaxis2 = y2, yaxis = y1)
          } else if (length(winds) > 0 && length(non_winds) == 0){
            # this situation is where there are only wind variables 
            for (i in 1:length(winds)){
              series_name <- get_var_fullname(input$vvt[winds][i])
              y_dat <- pull(data, input$vvt[winds][i])
              sk_plt <- sk_plt %>% add_lines(y = y_dat, name = series_name, x = ~time)
            }
            sk_plt <- sk_plt %>% layout(yaxis = list(title = "Wind Speed (m/s)"))
          } else { # other option then do not render plot 
            sk_plt <- NULL
          }
        }
        # getting the stepmode forward or backward 
        sk_plt %>% layout(xaxis = list(
          rangeselector = list(
            buttons = list(
              list(
                count = 30,
                label = "30 minutes",
                step = "minute",
                stepmode = "backward"),
              list(
                count = 1,
                label = "1 hour",
                step = "hour",
                stepmode = "backward"
              ),
              list(step = "all"))),
          rangeslider = list(type = "date")))
      }) 
      return(vvt_plt)
    }
  )
}



#' function to validate vvt input 
#' @param input the input from the ui
#' @return NULL if input is validated or an error message 
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



#' helper function to plot the chosen variable with time
#' @param id: to link the input and the output
#' @param variables: the variables to plot
#' @param data: the data to work with
#' @param indices: the indices to use for the data
plot_by_time <- function(id, variables, data, indices, input, output, session, title){
  # Time plots 
  #TODO: move form_label into reactive element 
  
  skeleton_plt <- reactive({
    form_label <- names(variables)[which(variables == input$vvt)]
    plt <- plot_ly(type = "scatter", mode = "lines") %>% 
      layout(title = glue("{title}", title = title), yaxis = list(title = form_label))
    plt
  })
  
  
  # Plotting
  vvt_plt <- eventReactive(input$load_vvt,{
    validate(
      vvt_selection(input$vvt) # validating input$vvt
    )
    
    wind_variables <- c('u', 'v', 'w') # which variables are wind 
    
    sk_plt <- skeleton_plt() %>% layout(xaxis = list(title = "Time (seconds)"))
    
    col = c("firebrick", "royalblue", "yellow", "grey")
    # wind and non-wind inputs
    non_winds <- which(!input$vvt %in% wind_variables)
    wind <- which(input$vvt %in% wind_variables)
    
    time_frame <- frame <- as.double(input$frame1) # what is the time frame ()
    each <- 3600/frame # divide hour by time frame
    num_frames = 24*frame # total number of frames
    total_var <- list()
    total_var <- list()
    #slider 
    #create the slider steps
    steps <- create_slider_step(num_frames, time_frame)
    
    
    if (length(wind) >= 1 && length(non_winds) > 0){ # if there is one wind variable
      
      non_wind_series <- names(variables)[which(variables == input$vvt[non_winds])]
      
      
      
      wind_min = 500
      wind_max = -500
      
      for (i in 1:length(wind)){ #go trhough list of winds
        wind_series <- series <- variables[which(variables == input$vvt[wind[i]])]
        y_val = pull(data, input$vvt[wind[i]])[indices]
        
        curr_min = min(y_val)
        curr_max  = max(y_val)
        
        if(curr_min < wind_min){
          wind_min =  curr_min
        }
        
        if(curr_max > wind_max){
          wind_max =  curr_max
        }
        
        total_var[[i]] = create_frames(num_frames, y_val, data$second, each, wind_series)
      }
      
      for(indx in 1:length(wind)){ # go through each wind variable
        mod = total_var[[indx]]
        for(i in 1:num_frames){ # create the plot
          sk_plt <- add_lines(sk_plt,x=mod[i][[1]]$x,  y=mod[i][[1]]$y, visible = mod[i][[1]]$visible, 
                              name = mod[i][[1]]$name, line  = list(color = col[wind[indx]], width = 2),
                              type = 'scattergl', hoverinfo = 'name')
        }
      }
      
      #put the non wind variable in the plot
      y1 <- pull(data, input$vvt[non_winds])[indices]
      aval = create_frames(num_frames, y1, data$second[indices], each, input$vvt[non_winds])
      
      for(i in 1:num_frames){
        sk_plt <- add_lines(sk_plt,x=aval[i][[1]]$x,  y=aval[i][[1]]$y, visible = aval[i][[1]]$visible, 
                            name = aval[i][[1]]$name,line  = list(color = col[non_winds], width = 2),
                            hoverinfo = 'name', yaxis = "y2")
      }
      
      y_axis1 <- list(side = "left", title = "Wind Speed", range = c(wind_min, wind_max))
      y_axis2 <- list(side = "right", overlaying = "y", title = non_wind_series, 
                      range = c(min(y1), max(y1)))
      
      sk_plt <- sk_plt %>% layout(yaxis2 = y_axis2, yaxis = y_axis1,
                                  sliders = list(list(active = 1, currentvalue = list(prefix = "Frequency: "),
                                                      steps = steps)))
      
    } 
    
    else if (length(wind) == 0  && length(non_winds) > 0) { #  2 non winds variable
      
      series_1 <- names(variables)[which(variables == input$vvt[non_winds[1]])]
      series_2 <- names(variables)[which(variables == input$vvt[non_winds[2]])]
      
      #contains a list of min and max for both variables
      min_max = list()
      
      for (i in 1:length(non_winds)){
        y <- pull(data, input$vvt[non_winds[i]])[indices]
        
        min_max[[i]] = c(min(y), max(y))
        x <- data$second[indices]
        total_var[[i]] = create_frames(num_frames, y, x, each, get(glue("series_{num}", num = i)))
      }
      
      for(indx in 1:length(non_winds)){ # go through each non wind variable
        mod = total_var[[indx]]
        for(i in 1:num_frames){ # create the plot
          sk_plt <- add_lines(sk_plt,x=mod[i][[1]]$x,  y=mod[i][[1]]$y, visible = mod[i][[1]]$visible, 
                              name = mod[i][[1]]$name, line  = list(color = col[indx], width = 2),
                              type = 'scattergl', hoverinfo = 'name', yaxis = glue("y{num}", num = indx))
        }
      }
      
      y1 <- list(side = "left", title = series_1, range = min_max[[1]])
      y2 <- list(side = "right", overlaying = "y1", title = series_2)
      if(length(non_winds)  == 2){
        y2 <- list(side = "right", overlaying = "y1", title = series_2, range = min_max[[2]])
      }
      
      
      sk_plt <- sk_plt %>% layout(
        yaxis2 = y2, yaxis = y1, 
        sliders = list(list(active = 1, currentvalue = list(prefix = "Frequency: "),
                            steps = steps)))
      
    } 
    
    else if (length(wind) > 0 && length(non_winds) == 0){ # only winds variable
      
      
      
      wind_min = 500
      wind_max = -500
      
      for (i in 1:length(wind)){ #go trhough list of winds
        wind_series <- series <- variables[which(variables == input$vvt[wind[i]])]
        y_val = pull(data, input$vvt[wind[i]])[indices]
        curr_min = min(y_val)
        curr_max  = max(y_val)
        
        if(curr_min < wind_min){
          wind_min =  curr_min
        }
        
        if(curr_max > wind_max){
          wind_max =  curr_max
        }
        
        total_var[[i]] = create_frames(num_frames, y_val, data$second[indices], each, wind_series)
      }
      
      for(indx in 1:length(wind)){ # go through each wind variable
        mod = total_var[[indx]]
        for(i in 1:num_frames){ # create the plot
          sk_plt <- add_lines(sk_plt,x=mod[i][[1]]$x,  y=mod[i][[1]]$y, visible = mod[i][[1]]$visible, 
                              name = mod[i][[1]]$name, line  = list(color = col[wind[indx]], width = 2),
                              type = 'scattergl', hoverinfo = 'name')
        }
      }
      
      sk_plt <- sk_plt %>%
        layout(yaxis = list(title = "Wind Speed", range = c(wind_min, wind_max)),
               sliders = list(list(active = 1, currentvalue = list(prefix = "Frequency: "),
                                   steps = steps)))
    } 
    
    else {
      sk_plt <- NULL
    }
    
    sk_plt
  })
  
  return(vvt_plt)
}




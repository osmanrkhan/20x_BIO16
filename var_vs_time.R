
library(shiny)
library(dplyr)

#' lets the user select the variables to be graphed
#' param @id: to link input and output
plot_var_vs_time_ui <- function(id){
  ns <- NS(id)
  tagList(
    checkboxGroupInput(ns("vvt"), "Select Variables", 
                       choices = list( "Vertical Wind Speed" = "w" , 
                                       "Horizontal Wind Speed (North)" = "v" , 
                                       "Horizontal Wind Speed (East)" ="u",
                                       "CO2" = "CO2",
                                       "Water Vapor" = "H2O",
                                       "Air Temperature" = "airtemp"),
                       selected = "w"),
    selectInput(ns("frame1"), "Select Time Frame for Time Plots",
                choices = list("30 mintes" = 2, "1 hour" = 1)),
    
    radioButtons(ns("timescale"), h4("Render Time Scale"),
                 list("Second" = "sec",
                      "Minute" = "min",
                      "Hour" = "hr")),
    actionButton(ns("load_vvt"), "Load graph")
  )
}

#' function to validate vvt input 
#' param: @input the input from the ui
#' return NULL if input is validated or an error message 
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

#' plots the chosen variable with time
#' param @id: to link the input and the output
#' param @variables: the variables to plot
#' param @data: the data to work with

plot_var_vs_time_server <- function(id, variables, data) {
  moduleServer(
    id,
    function(input, output, session){
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
          labs <- list(units = "seconds", scale = 1, x = data$second)
        } else if (input$timescale == "min"){
          labs <- list(units = "minutes", scale = 60, x = data$minute)
        } else if (input$timescale == "hr"){
          labs <- list(units = "hours", scale = 3600, x = data$hour)
        }
        return(labs)
      })
      
      # Plotting
      vvt_plt <- eventReactive(input$load_vvt,{
        validate(
          vvt_selection(input$vvt) # validating input$vvt
        )
        
        wind_variables <- c('u', 'v', 'w') # which variables are wind 
        
        sk_plt <- skeleton_plt() %>% layout(xaxis = list(title = glue("Time ({unit})", 
                                                                      unit = vvt_labs()$units))) 
        
        col = c("firebrick", "royalblue", "yellow", "grey")
        # wind and non-wind inputs
        non_winds <- which(!input$vvt %in% wind_variables)
        wind <- which(input$vvt %in% wind_variables)
        
        time_frame <- frame <- as.double(input$frame1) # what is the time frame ()
        each <- 3600/frame # divide hour by time frame
        num_frames = 24*frame # total number of frames
        total_var <- list()
        #slider 
        #create the slider steps
        steps <- create_slider_step(num_frames, time_frame)
        
        
        if (length(wind) >= 1 && length(non_winds) > 0){ # if there is one wind variable
          
          non_wind_series <- names(variables)[which(variables == input$vvt[non_winds])]
          y_axis <- list(side = "left", title = "Wind Speed")
          y_axis2 <- list(side = "right", overlaying = "y", title = non_wind_series)
          
         
          
          for (i in 1:length(wind)){ #go trhough list of winds
            wind_series <- series <- variables[which(variables == input$vvt[wind[i]])]
            y_val = forecast::ma(pull(data, input$vvt[wind[i]]), order = vvt_labs()$scale)
            total_var[[i]] = create_frames(num_frames, y_val, vvt_labs()$x, each, wind_series)
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
          y1 <- forecast::ma(pull(data, input$vvt[non_winds]), order = vvt_labs()$scale)
          
          aval = create_frames(num_frames, y1, vvt_labs()$x, each, input$vvt[non_winds])
          
          for(i in 1:num_frames){
            sk_plt <- add_lines(sk_plt,x=aval[i][[1]]$x,  y=aval[i][[1]]$y, visible = aval[i][[1]]$visible, 
                                name = aval[i][[1]]$name,line  = list(color = col[non_winds], width = 2),
                                hoverinfo = 'name', yaxis = "y2")
          }
          
          sk_plt <- sk_plt %>%
            layout(yaxis2 = y_axis2, yaxis = y_axis,
                   sliders = list(list(active = 1, currentvalue = list(prefix = "Frequency: "),
                                       steps = steps)))
          
        } 
        
        else if (length(wind) == 0  && length(non_winds) > 0) {
          
          series_1 <- names(variables)[which(variables == input$vvt[non_winds[1]])]
          series_2 <- names(variables)[which(variables == input$vvt[non_winds[2]])]
          y1 <- list(side = "left", title = series_1)
          y2 <- list(side = "right", overlaying = "y1", title = series_2)
          
          for (i in 1:length(non_winds)){
            y <- forecast::ma(pull(data, input$vvt[non_winds[i]]), order = vvt_labs()$scale)
            x <- vvt_labs()$x
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
          
          sk_plt <- sk_plt %>% layout(
            yaxis2 = y2, yaxis = y1, 
            sliders = list(list(active = 1, currentvalue = list(prefix = "Frequency: "),
                                steps = steps)))
        
        } 
        
        else if (length(wind) > 0 && length(non_winds) == 0){
          for (i in 1:length(wind)){ #go trhough list of winds
            wind_series <- series <- variables[which(variables == input$vvt[wind[i]])]
            y_val = forecast::ma(pull(data, input$vvt[wind[i]]), order = vvt_labs()$scale)
            total_var[[i]] = create_frames(num_frames, y_val, vvt_labs()$x, each, wind_series)
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
            layout(yaxis = list(title = "Wind Speed"),
                   sliders = list(list(active = 1, currentvalue = list(prefix = "Frequency: "),
                                       steps = steps)))
        } 
        
        else {
          sk_plt <- NULL
        }
        
        sk_plt
      })
      return (vvt_plt)
    }
  )
}



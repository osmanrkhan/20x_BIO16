
library(shiny)
library(dplyr)

#' lets the user select the variables to be graphed
#' param @id: to link input and output
plot_var_vs_time_ui1 <- function(id, variables) {
    ns <- NS(id)
    
    # assemble UI elements
    tagList(
      
      fluidRow(
        
        column(
          width = 2,
          wellPanel(
            checkboxGroupInput(
              ns("vvt"),
              "Select Y variable",
              choices = variables,
              selected = variables[1]
            ),
            selectInput(ns("frame1"), "Select Time Frame for Time Plots",
                      choices = list("30 mintes" = 2, "1 hour" = 1)
            ),
            actionButton(ns("load_vvt"), "Load graph"),
          )
        ),
        
        column(
          width = 5,
          plotlyOutput(ns("plot1"))
        ),
        column(
          width = 5,
          plotlyOutput(ns("plot2"))
        )
      )
     
  )
}

#' lets the user select the variables to be graphed
#' param @id: to link input and output
plot_var_vs_time_ui2 <- function(id, variables) {
  ns <- NS(id)
  
  # assemble UI elements
  tagList(
    
    fluidRow(
      column(
        width = 8,
        plotlyOutput(ns("plot2"))
      ),
      column(
        width = 4,
        wellPanel(
          checkboxGroupInput(
            ns("vvt"),
            "Select Y variable",
            choices = variables,
            selected = variables[1]
          ),
          br(),
          actionButton(ns("load_vvt"), "Load graph"),
          selectInput(ns("frame1"), "Select Time Frame for Time Plots",
                      choices = list("30 mintes" = 2, "1 hour" = 1)
          )
        )
      )
    )
    
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
      
      title = "Summer"
      indices = seq(1, 86400)
      vvt_plt1 <- plot_by_time(id, variables, data, indices, input, output, session, title)
      
      output$plot1 <- renderPlotly({
        vvt_plt1()
      })
      
      title1 = "Winter"
      indices1 = indices + 86400
      vvt_plt2 <- plot_by_time(id, variables, data, indices1, input, output, session, title1)
      
      
      
      output$plot2 <- renderPlotly({
        vvt_plt2()
      })
      
    }
  )
  
}  

#' helper function to plot the chosen variable with time
#' param @id: to link the input and the output
#' param @variables: the variables to plot
#' param @data: the data to work with
#' param @indices: the indices to use for the data
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




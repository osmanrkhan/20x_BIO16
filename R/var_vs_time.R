library(shiny)
library(dplyr)

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


#' Function to generate the plot using a skeleton plot, data and input
#' @param input The input wrapped inside a server function
#' @param data The data set 
generate_vvtplt <- function(id, input, output, session, title = "Variable vs Time", data){
  # first, generate the skeleton plot 
  sk_plt <- plot_ly(data = data, height = 600) %>%
    layout(title = title, xaxis = list(title = "Time"))
  # if the length of input is 
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
  sk_plt <- sk_plt %>%  layout(xaxis = list(
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
  return(sk_plt)
}



#' lets the user select the variables to be graphed
#' @param id: to link input and output
plot_vvt_ui <- function(id){
  ns <- NS(id)
  tagList(
    checkboxGroupInput(ns("vvt"), h5("Select variable to plot versus time"),
                       choices = list( "Vertical Wind Speed" = "w" , 
                                       "Horizontal Wind Speed (North)" = "v" , 
                                       "Horizontal Wind Speed (East)" ="u",
                                       "CO2" = "CO2",
                                       "Water Vapor" = "H2O",
                                       "Air Temperature" = "airtemp"),
                       selected = "CO2"),
    selectInput(ns("season"), h5(strong("Selecting Season")),
                choices = list("Summer" = 1, "Winter" = 2, "Both" = 3)),
    actionButton(ns("load_vvt"), "Load graph")
  )
}


plot_vvt_server <- function(id, data){
  moduleServer(
    id,
    function(input, output, session){
      vvt_plt <- eventReactive(input$load_vvt,{
        # validate selection so that there is only one non-wind variable if there are wind variables, 
        # and that there can only be 2 non-wind variables at max if there are no wind variables
        validate(vvt_selection(input$vvt))
        # pipe input and data into generate plot 
        if (input$season == 1){
          data <- data %>% filter(season == "summer")
          generate_vvtplt(id, input, output, session, data = data)
        } else if (input$season == 2){
          data <- data %>% filter(season == "winter")
          generate_vvtplt(id, input, output, session, data = data)
        } else if (input$season == 3){
          summer_dt <- data %>% filter(season == "summer")
          winter_dt <- data %>% filter(season == "winter")
          summer_plt <- generate_vvtplt(id, input, output, session, data = summer_dt)
          winter_plt <- generate_vvtplt(id, input, output, session, data = winter_dt) 

          
          comb <- subplot(winter_plt, summer_plt, nrows = 2, margin = c(0,0,0.2,0.2),
                          titleX = F, titleY = F) %>% 
            layout(title = "Variable vs Time for Winter (top) and Summer (bottom)")
        }
      }) 
      return(vvt_plt)
    }
  )
}




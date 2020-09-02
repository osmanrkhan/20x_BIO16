
#'   List of variables and their more formatted names for easy plotting
#'   used throughout the app
#'   

variables <- list("Vertical Wind Speed" = "w" , 
                  "Horizontal Wind Speed (North)" = "v" , 
                  "Horizontal Wind Speed (East)" ="u",
                  "CO2" = "CO2",
                  "Water Vapor" = "H2O",
                  "Air Temperature" = "airtemp")

   
get_var_fullname <- function(varname){
  variables <- list("Vertical Wind Speed" = "w" , 
                    "Horizontal Wind Speed (North)" = "v" , 
                    "Horizontal Wind Speed (East)" ="u",
                    "CO2" = "CO2",
                    "Water Vapor" = "H2O",
                    "Air Temperature" = "airtemp")
  longname <- names(variables)[which(variables %in% varname)]
  return(longname) 
}

#' function to create a list of frames for plotting
#' 
#' param: @num_frames: number of frames
#' param: @y_val: y data
#' param @x_val: x data
#' param @each: the lenght of points in each frame
#' param @name: name of data
#' 
#' return : a named list in which each index contains: x values, y values, 
#' name of the variable and a visible parameterdecides which frame should be visible. 
#' 
create_frames <- function(num_frames, y_val, x_val, each, name){
  aval = list()
  for(step in 1:num_frames){#go through each frame
    y <- y_val[seq((step-1)*(each) + 1, length = each)]
    x <- x_val[seq((step-1)*(each) + 1, length = each)]
    aval[[step]] <-list(visible = FALSE,
                        name = name,
                        x=x,
                        y=y)
  }
  
  aval[1][[1]]$visible = TRUE
  return (aval)
}

#' method for creating which points should be visible at each timestamp
#' param: @num_frames: number of frames to plot
#' param: @time_frame:
#' return a list that decides which variable should be shown at each timestamp
create_slider_step <- function(num_frames, time_frame){
  #create the slider steps
  steps <- list()
  for (i in 1:num_frames) {
    
    name = " hours"
    if(i <= time_frame){
      name = " hour"
    }
    
    step <- list(args = list('visible', rep(FALSE, num_frames)), 
                 label =  paste0(round( i/time_frame, 2), name),
                 method = 'restyle')
    step$args[[2]][i] = TRUE  
    steps[[i]] = step 
  }  
  return(steps)
}
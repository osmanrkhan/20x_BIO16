#' Variables for the raw variables section 
library(patchwork)

raw_variables <- list("Vertical Wind Speed" = "w" , 
                  "Horizontal Wind Speed (North)" = "v" , 
                  "Horizontal Wind Speed (East)" ="u",
                  "CO2" = "CO2",
                  "Water Vapor" = "H2O",
                  "Air Temperature" = "airtemp")
site <-  list("Silas Little" = "silas_little")
raw_data_files <- list("Combined 06/05/2018 and 01/19/2018" = "comb_2018-06-05_2018-01-19",
                      "Combined 06/06/2018 and 01/20/2018" = "comb_2018-06-06_2018-01-20")

#' Variables for the full data section  

full_var_times <- c("JD", "hrmm", "season", "timeofday")

full_variables <- list(
  "Julian Day" = "JD",
  "Photosynthetically active radiation" = "PPFD_in",
  "Hour : Minute" = "hrmm",
  "Air Temperature" = "TA",
  "Relative Humidity" = "RH",
  "Soil Moisture" = "Vol.W.C",
  "Soil Temperature" = "Soil.T",
  "Season" = "season",
  "Time of Day" = "timeofday"
)

#' Getting full name of variables 
#' @param varname A vector of shortened name of variables such as 'w'
#' @param varlist A list of variables and their original codes 
#' @return A vector of matched full names 
get_var_fullname <- function(varname, varlist){
  longname <- names(varlist)[which(varlist %in% varname)]
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


#' @title Out of sample prediction models using train/test split 
#' @param data Data of data.frame format
#' @param formula Regression formula class
#' @param split_prop Train-test split. Default 3/4 of the data set will be training 
oos_prediction <- function(data, formula, split_prop = 3/4){
  split <- initial_split(data, prop = split_prop)
  train <- linear_reg(mode = "regression") %>% set_engine("lm") %>% 
    fit(data = training(split), formula = formula)
  estimate <- predict(train, new_data = testing(split), type = "raw")
  interval <- predict(train, new_data = testing(split), type = "pred_int")
  truth <- testing(split)$NEE
  df <- cbind(estimate, interval, truth)
  annotation <- glue("RMSE: {rmse_val} \n Predictive R-squared: {rsq_val}", 
                     rmse_val = round(rmse_vec(truth = df$truth, estimate = df$estimate),2),
                     rsq_val = round(rsq_trad_vec(truth = df$truth, estimate = df$estimate),3))
  plot <- ggplot(data = df, aes(x = estimate, y = truth)) + geom_point(col = "salmon", size = 2) + 
    geom_line() +
    geom_ribbon(aes(ymin = .pred_lower, ymax = .pred_upper), alpha = 0.3) + 
    stat_smooth(method = "lm", formula = y~x) + 
    labs(x = "Estimate", y = "Truth", title = "Out of Sample Prediction") + 
    annotate("text", x = min(df$estimate), y = max(df$truth), label = annotation, hjust = 0, size = 5) + theme_bw()
  return(plot)
} 

#' @title Evaluating lm fit on full data and evaluating 
#' @param data The data as data frame
#' @param formula Regression formula class 
#' @return A list of objects including model summary, fitting plots and residuals 
fit_eval <- function(data, formula){
  fit <- lm(formula = formula, data = data)
  mod_sum <- summary(fit) 
  
  # fitted values plot
  fitted <- fitted.values(fit)
  truth <- data$NEE
  df <- data.frame(fitted = fitted, truth = truth) %>% as.data.frame()
  annotation <- glue("AIC: {aic_val} \n Adj. R-squared: {rsq_val}", 
                     aic_val = round(AIC(fit),3),
                     rsq_val = round(mod_sum$adj.r.squared,3))
  fit_plot <- ggplot(df, aes(x = fitted, y = truth)) + geom_point(col = "salmon", size = 2) + 
    stat_smooth(method = "lm", formula = y~x) + labs(x = "Estimate", y = "Truth", title = "Fitted values vs. True") +
    annotate("text", x = min(df$fitted), y = max(df$truth), label = annotation, hjust = 0, size = 5) +
    theme_bw()
  residuals <- resid(fit)
  out <- list(summary = mod_sum, fit_plot = fit_plot, residuals = residuals)
  return(out)
}

gap_filling <- function(data, formula){
  complete_data <- data %>% filter(!is.na(NEE))
  missing_data <- data %>% filter(is.na(NEE))
  model <- lm(formula = formula, data = complete_data)
  new_NEE <- predict(model, newdata = missing_data %>% select(-NEE))
  missing_data <- missing_data %>% mutate(NEE = new_NEE)
  combined <- rbind(missing_data, complete_data) %>% arrange(date)
  
  # boxplot of NEE distribution by day
  p1 <- ggplot(combined, aes(x = season, fill = timeofday, y = NEE )) + 
    geom_boxplot() + theme_bw() + scale_fill_brewer(palette = "Dark2") + 
    labs(x = "Season", y = "Net Ecosystem Exchange", fill = "Time of Day", title = "Distribution of NEE by season and time of day")
  
  # barplot of total NEE
  p2 <- combined %>% group_by(season, timeofday) %>% summarize(NEE = sum(NEE)) %>% 
    ggplot(aes(y = NEE, x = season, fill = timeofday)) + geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = round(NEE,2), x = season, y = NEE/2), size = 4, position = position_dodge(width = 0.8), 
              vjust = 0, hjust = 0.5) + 
    theme_bw() + scale_fill_brewer(palette = "Dark2") +
    labs(x = "Season", y = "Net Ecosystem Exchange", 
         title = glue("Total NEE: {value}", value = round(sum(combined$NEE),2)),
         subtitle = glue("NEE in Winter Season: {winter}; NEE in Growing Season: {gs}",
                         winter = round(sum(combined %>% filter(season == "W") %>% pull(NEE)),2),
                         gs = round(sum(combined %>% filter(season == "GS") %>% pull(NEE)),2)))
  # NEE by JD
  p3 <- combined %>% group_by(JD) %>% summarise(NEE = sum(NEE)) %>% ggplot(aes(x = JD, y = NEE)) + 
    geom_point(color = "orange") + theme_bw() + labs(x = "Net Ecosystem Exchange", y = "Julian Day") + 
    stat_smooth()
  
  plot <- ((p1 + theme(legend.position = "none")) + p2)/p3
  return(plot)
}

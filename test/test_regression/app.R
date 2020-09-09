library(shiny)
library(glue)
library(parsnip)
library(yardstick)
library(rsample)
library(tidyverse)

variables <- list(
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

oob_prediction <- function(data, formula, split_prop = 3/4){
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

ui <- fluidPage(
  h3("Select Regression"),
  sidebarLayout(
    sidebarPanel(
      selectInput("select_var", "Select Variable",
                  choices = variables, selected = 1),
      selectInput("poly", "Polynomial Transformation",
                  choices = list("None" = 1, "Second degree" = 2, "Third degree" = 3), 
                  selected = 1),
      checkboxInput(inputId = "interaction", label = "Add interaction"),
      conditionalPanel(
        condition = "input.interaction == true",
        selectInput("interact_var", "Select Variable for Interaction", choices = variables)
      ),
      conditionalPanel(
        condition = "input.interaction == false",
        radioButtons("facet_by", "Separated By", 
                     choices = c("Season" = "season","Time of Day" = "timeofday","None" = "none"), 
                     selected = "none")
      ),
      actionButton("add_term", "Add term to model"),
      actionButton("reset", "Reset Model"),
      actionButton("fit", "Fit Model")
    ), mainPanel(
      tabsetPanel(
        tabPanel("Regression Builder", 
                 verbatimTextOutput("var_display"), 
                 verbatimTextOutput("regression_builder")),
        tabPanel("Bivariate Plots", plotOutput("bivariate_plot")),
        tabPanel("Model Evaluation", fluidPage(
          fluidRow(
            column(6,plotOutput("oos_pred")),
            column(6,plotOutput("fit_plot"))),
          fluidRow(
            column(12, verbatimTextOutput("mod_summary"))
          )
        ))
      )
    )
  )
)



server <- function(input, output, session){
  data <- readRDS(file = "data/processed_data/silas_little_2018.rds")
####----------------------- Regression Builder --------------------------------------####
  # construct reactive variable component 
  var <- reactive({
    text <- NULL
    if (input$poly == 1){
      text <- input$select_var
    } else {
      text <- glue("poly({var},{deg})", var = input$select_var, deg = input$poly)
    }
    
    if (input$interaction == T){
      text <- glue("{text}*{int}", text = text, int = input$interact_var)
    }
    text
  })
  
  # construct variable display component 
  output$var_display <- renderPrint({
    cat(var())
  })
  
  # construct term labels as reactive values 
  reg <- reactiveValues(term_labels = c())
  
  # append new terms as a stringset 
  observeEvent(input$add_term,{
   reg$term_labels <- append(reg$term_labels, var()) 
  })
  
  # remove everything if reset button is pushed 
  observeEvent(input$reset,{
    reg$term_labels <- c()
  })
  
  # model formula 
  model <- reactive({
    if(length(reg$term_labels) == 0){
      cat("No terms")
    } else {
      reformulate(termlabels = reg$term_labels, response = "NEE", intercept = TRUE)
    }
  })
  
  output$regression_builder <- renderPrint({
    model()
  })
  
####----------------------- Bivariate plot -----------------------------------####   
  plot <- reactive({
    x <- `^`(pull(data, input$select_var),as.numeric(input$poly))
    if (input$interaction == T){
      if (input$interact_var %in% c("season", "timeofday")){
        plt <- qplot(y = data$NEE, x = x, geom = "jitter", color = factor(pull(data, input$interact_var))) + 
          theme_bw() + stat_smooth(method = "lm", formula = y ~ x) +
          labs(y = "NEE", x = var())
      } else {
        group <- cut(pull(data, input$interact_var), breaks = 3) %>% factor(labels = c("low", "medium", "high"))
        plt <- qplot(y = data$NEE, x = x, geom = "jitter", color = group) + theme_bw() + stat_smooth(method = "lm", formula = y ~ x) +
          labs(y = "NEE", x = var(), color = group)
      }
    } else {
      plt <- qplot(y = data$NEE, x = x, geom = "jitter", color = I("steelblue")) + theme_bw() + stat_smooth(method = "lm", formula = y ~ x) +
        labs(y = "NEE", x = var()) 
    }
    plt
  })
  
  # bivariate plot 
  output$bivariate_plot <- renderPlot({
    plot()
  })
####----------------------- Model Evaluation -------------------####
  
  model_plots <- reactiveValues(
    oos_plot = NULL,
    fit_plot = NULL,
    mod_summary = NULL  
  )
  observeEvent(input$fit,{
    data_complete <- data %>% filter(!is.na(NEE))
    model_plots$oos_plot <- oob_prediction(data = data_complete, formula = model())
    eval <- fit_eval(data = data_complete, formula = model())
    model_plots$fit_plot <- eval$fit_plot
    model_plots$mod_summary <- eval$summary
  })
  
  output$oos_pred <- renderPlot(
    if(is.null(model_plots$oos_plot)){
      return()
    } else {
      model_plots$oos_plot
    }
  )
  output$fit_plot <- renderPlot(
    if(is.null(model_plots$fit_plot)){
      return()
    } else {
      model_plots$fit_plot
    }
  )
  output$mod_summary <- renderPrint(
    if(is.null(model_plots$mod_summary)){
      return("No model present")
    } else {
      model_plots$mod_summary
    }
  )
  
}

shinyApp(ui = ui, server = server)

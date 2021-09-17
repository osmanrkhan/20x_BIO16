#' Variable selection for regression
#' @param id Character used to specify namespace \code{shiny::\link[shiny]{NS}}
#' @return a \code{shiny::\link[shiny]{tagList}} containing ui elements 
#' @import shiny
regression_ui <- function(id, variables){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        h5("In this section, you can observe the relationship between the variable of interest (NEE) and the
           all the other meterological variables in the full data set. You can observe this bivariate relationship
           after applying polynomial transformations or interaction variables. Through this process you can start building a regression model
           which can then be used to gap-fill the remaining missing NEE values"), 
        h5("First, let's select a variable in the drop down"),
        selectInput(ns("select_var"), "Select Variable",
                    choices = variables[-which(variables %in% c("timeofday", "season"))], selected = 1),
        h5("As you select a variable, you can see the bivariate plot in the ", 
           strong("Bivariate Plot") ,
          " tab. Then, you can see the variable selected using it's label in the table 
          in R (from the preview above) what the status of the current variable in the ", 
          strong("Regression Builder"), 
          " tab, as well as seeing what is the current model. This is where you would 'stage' 
          a variable to be added to the regression. Next, let's select any transformation you want to apply. In this demonstration, 
          only the polynomial transformation is supported. If you select a transformation, 
          you can see both the bivariate plot and the staging area change in response to this choice"),
        selectInput(ns("poly"), "Polynomial Transformation",
                    choices = list("None" = 1, "Second degree" = 2, "Third degree" = 3), 
                    selected = 1),
        h5("Similarly, you can select whether to add an interaction term. As you select which variables to interact with, 
           you can see that both the plot and the staged variable change as this choice is made. If there is no interaction chosen, 
           you can observe this bivariate relationship separated by season and time of day."), 
        checkboxInput(ns("interaction"), label = "Add interaction"),
        conditionalPanel(
          condition = "input.interaction == true",
          ns = ns,
          selectInput(ns("interact_var"), "Select Variable for Interaction", 
                      choices = variables)
        ),
        # This section below allows a radio button to show if interaction is false so that students don't have to 
        # think of staging an interaction in order to see how the relationship changes depending on time of day or season 
        # Temporary removed per request by instructor. 
        # conditionalPanel(
        #   condition = "input.interaction == false",
        #   ns = ns,
        #   radioButtons(ns("facet_by"), "Separated By", 
        #                choices = c("Season" = "season","Time of Day" = "timeofday","None" = "none"), 
        #                selected = "none")
        # ),
        h5("Finally, with these buttons you can manipulate how to interact with the model"),
        tags$ul(
          tags$li(strong("Add term to model"), ": adds the currently staged term to the model. 
          You can observe the model slowly being built in the corresponding tab"),
          tags$li(strong("Reset Model"), ": Removes all terms added to model. 
          You can observe the model being emptied in the corresponding tab"),
          tags$li(strong("Fit Model"), ": Fits the model. You can observe the results of the model fit in the corresponding tab")
        ),
        actionButton(ns("add_term"), "Add term to model"),
        actionButton(ns("reset"), "Reset Model"),
        actionButton(ns("fit"), "Fit Model"),
        actionButton(ns("gapfill"), "Gapfill missing NEE values")
      ), 
      mainPanel(
        tabsetPanel(
          tabPanel("Bivariate Plots", plotOutput(ns("bivariate_plot"))),
          tabPanel("Regression Builder", 
                   textOutput(ns("explain_var")), # tabset doesn't allow direct ui elements for some reason
                   verbatimTextOutput(ns("var_display")), 
                   textOutput(ns("explain_mod")), 
                   verbatimTextOutput(ns("regression_builder"))),
          tabPanel("Model Evaluation", fluidPage(
            fluidRow(
              column(6,plotOutput(ns("oos_pred"))),
              column(6,plotOutput(ns("fit_plot")))),
            fluidRow(
              column(12, verbatimTextOutput(ns("mod_summary")))
            )
          )),
          tabPanel("Gapfilling", fluidPage(
            plotOutput(ns("nee_plt"))
          ))
        )
      )
    )
  )
}


#' @title Server function for regression module 
#' @param id Identifier for module, see Shiny Modules documentation
#' @param data The data being loaded 
#' @importFrom glue glue
#' @import shiny
#' @import ggplot2
#' @import plotly
#' @importFrom dplyr pull
#' @importFrom magrittr %>%
#' @import viridis
#' @importFrom ggplot2 ggplot qplot labs stat_smooth scale_color_viridis_d theme_bw  
regression_server <- function(id, data){
  moduleServer(
    id, 
    function(input, output, session){
      # The placeholder text due to weird shiny issues  
      output$explain_var <- renderText("\n Current variable to be added to the regression formula as predictor \n")
      output$explain_mod <- renderText("\n Current complete model for testing \n")
      # Here we create a reactive value of the variable that is currently of interest as string value 
      var <- reactive({
        text <- NULL
        if (input$poly == 1){
          text <- input$select_var # if no polynomial transformation 
        } else {
          text <- glue::glue("poly({var},{deg})", var = input$select_var, deg = input$poly) # poly is the command for polynomial
        }
        
        if (input$interaction == T){
          text <- glue("{text}*{int}", text = text, int = input$interact_var) # interaction 
        }
        text
      })
      
      # construct variable display component 
      output$var_display <- renderPrint({
        cat(var())
      })
      
      # construct term labels as reactive values that can be modified by buttons (add_terms button)
      reg <- reactiveValues(term_labels = c())
      
      # append new terms (staged as the var() reactive value string) to the term labels
      observeEvent(input$add_term,{
        reg$term_labels <- append(reg$term_labels, var()) 
      })
      
      # remove everything if reset button is pushed 
      observeEvent(input$reset,{
        reg$term_labels <- c()
      })
      
      # model formula using reformulate, which would take a vector of string and concatenate as terms of a formula
      # NEE will always be the outcome 
      model <- reactive({
        if(length(reg$term_labels) == 0){
          cat("No terms")
        } else {
          reformulate(termlabels = reg$term_labels, response = "NEE", intercept = TRUE)
        }
      })
      
      # A representation of the model is the formula of formula class 
      output$regression_builder <- renderPrint({
        model()
      })
      
      ####----------------------- Bivariate plot -----------------------------------####   
      # plotting a reactive bivariate plot depending on what is interesting 
      plot <- reactive({
        x <- `^`(dplyr::pull(data, input$select_var), as.numeric(input$poly))
        if (input$interaction == T){ # if there are no interaction variables 
          xlab <- strsplit(var(),"*",fixed = T)[[1]][1]
          if (input$interact_var %in% c("season", "timeofday")){
            legend_name <- ifelse(input$interact_var == "season", "Season", "Time of Day")
            # using qplot as it's quicker to do this interactively 
            plt <- ggplot2::qplot(y = data$NEE, x = x, geom = "jitter", 
                                  color = factor(dplyr::pull(data, input$interact_var))) + 
              ggplot2::theme_bw() + 
              ggplot2::stat_smooth(method = "lm", formula = y ~ x) +
              ggplot2::labs(y = "NEE", x = xlab, color = legend_name) + 
              ggplot2::scale_color_viridis_d()
          } else {
            group <- cut(dplyr::pull(data, input$interact_var), breaks = 3) %>% factor(labels = c("Low", "Medium", "High"))
            plt <- ggplot2::qplot(y = data$NEE, x = x, geom = "jitter", color = group) + 
              ggplot2::theme_bw() + 
              ggplot2::stat_smooth(method = "lm", formula = y ~ x) +
              ggplot2::labs(y = "NEE", x = xlab, color = "Levels of Interaction Values") + 
              ggplot2::scale_color_viridis_d()
          }
        } else {
          plt <- ggplot2::qplot(y = data$NEE, x = x, geom = "jitter", color = I("steelblue")) + 
            ggplot2::theme_bw() + 
            ggplot2::stat_smooth(method = "lm", formula = y ~ x) +
            ggplot2::labs(y = "NEE", x = var()) 
        }
        plt
      })
      
      # bivariate plot 
      output$bivariate_plot <- renderPlot({
        plot()
      })
      ####----------------------- Model Evaluation -------------------####
      # Making all these variables interactive values that can be interacted using a button 
      model_plots <- reactiveValues(
        oos_plot = NULL,
        fit_plot = NULL,
        mod_summary = NULL
      )
      # If the fit button was pressed, fill up the entire reactive value list of model_plots
      # see function oos_prediction and fit_eval for more documentation 
      observeEvent(input$fit,{
        data_complete <- data %>% filter(!is.na(NEE))
        model_plots$oos_plot <- oos_prediction(data = data_complete, formula = model())
        eval <- fit_eval(data = data_complete, formula = model())
        model_plots$fit_plot <- eval$fit_plot
        model_plots$mod_summary <- eval$summary
      })
      
      # render oos_plot
      output$oos_pred <- renderPlot(
        if(is.null(model_plots$oos_plot)){
          return()
        } else {
          model_plots$oos_plot
        }
      )
      # render fit_plot
      output$fit_plot <- renderPlot(
        if(is.null(model_plots$fit_plot)){
          return()
        } else {
          model_plots$fit_plot
        }
      )
      # render model summary 
      output$mod_summary <- renderPrint(
        if(is.null(model_plots$mod_summary)){
          return("No model present")
        } else {
          model_plots$mod_summary
        }
      ) # end render print mod summary
      nee_plot <- eventReactive(input$gapfill,{
        gap_filling(data = data, formula = model())
      })
      output$nee_plt <- renderPlot({
        nee_plot()
      })
    } # end input output function 
  ) # end module server
} # end regression_server function
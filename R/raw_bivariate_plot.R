#' @title Lets the user select the variables to be graphed
#' @param id: to link input and output
#' @param xvar: the x variables
#' @param yvar: the y variables
#' @import shiny
bivariate_ui <- function(id, xvar, yvar){
  ns <- NS(id)
  tagList(
    h3("Bivariate plots"),
    sidebarLayout(
      sidebarPanel(
        h5("In this section, you can see the observe the relationship between two variables in the raw data using bivarate plot.  
                    There are two modes: "), 
        tags$ul(
          tags$li("First, you can observe how the relationship (covariance) changes through time using a slider. This plot
                           can be loaded using the button ", strong("Load Bivariate Plot by Time")),
          tags$li("Second, you can observe the relationship using the complete raw data throughout the day. This plot can be loaded
                           using the button ", strong("Load Complete Bivariate Plot"))
        ),
        h5("Using the option ", strong("Select Time Frame for Time Plots"), " you can change the time step for the slider available in the first mode mentioned above"),
        selectInput(ns("pairplot_xvar"), "Select X Axis", 
                    choices = xvar,
                    selected = xvar[[1]]),
        selectInput(ns("pairplot_yvar"), "Select Y Axis", 
                    choices = yvar,
                    selected = yvar[[2]]),
        selectInput(ns("frame"), "Select Time Frame for Time Plots",
                    choices = list("30 minutes" = 2, "1 hour" = 1)),
        actionButton(ns("load_time"), "Load Bivariate Plot by Time"),
        actionButton(ns("load_all"), "Load Complete Bivariate Plot"),
        h6("Note: when you press the load graph option, you are graphing both
           summer and winter plots but it might take some time for the graph to appear.")
      ),
      mainPanel(
        navbarPage("Seasons",
          #---- SUMMER
          tabPanel("Summer",
            fluidPage(
              tabsetPanel(
              # summer slider-----------------------------------##
              tabPanel("Slider", 
                plotlyOutput(ns("pairplot_summer_slider"))
              ),
              # end of summer slider -----------------------
              # summer full graph -----------------------------
              tabPanel("Full Graph",
                plotOutput(ns("pairplot_summer_full_graph"))
              )
            )
          )
        ),
        #----- WINTER
        tabPanel("Winter",
          fluidPage(
            tabsetPanel(
              #winter slider-----------------------------------##
              tabPanel("Slider", 
                plotlyOutput(ns("pairplot_winter_slider"))
              ),
              # end of winter slider -----------------------
              # winter full graph -----------------------------
              tabPanel("Full Graph",
                plotOutput(ns("pairplot_winter_full_graph"))
              )
            )
          )
        )
      )
    )
  )
 )
}

#' @title Either plots a whole bivariate plot or a bivariate plot animation through time.
#' @param id: to link the input and the output
#' @param variables: the variables to plot
#' @param data: the data to work with
#' @import shiny
#' @importFrom magrittr %>%

bivariate_server <- function(id, variables, data) {
  season <- NULL
  moduleServer(
    id,
    function(input, output, session){
      plot_raw_data_bivariate(input,output,variables, data %>% filter(season == "summer"), "summer")
      plot_raw_data_bivariate(input,output,variables, data %>% filter(season == "winter"), "winter")
      
    }
  )
}


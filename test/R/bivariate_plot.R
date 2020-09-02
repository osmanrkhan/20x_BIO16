#' lets the user select the variables to be graphed
#' param @id: to link input and output
bivariate_ui <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(ns("pairplot_xvar"), "Select X Axis", 
                choices = list( "Vertical Wind Speed" = "w" , 
                                "Horizontal Wind Speed (North)" = "v" , 
                                "Horizontal Wind Speed (East)" ="u",
                                "CO2" = "CO2",
                                "Water Vapor" = "H2O",
                                "Air Temperature" = "airtemp"),
                selected = "w"),
    selectInput(ns("pairplot_yvar"), "Select Y Axis", 
                choices = list( "Vertical Wind Speed" = "w" , 
                                "Horizontal Wind Speed (North)" = "v" , 
                                "Horizontal Wind Speed (East)" ="u",
                                "CO2" = "CO2",
                                "Water Vapor" = "H2O",
                                "Air Temperature" = "airtemp"),
                selected = "v"),
    selectInput(ns("frame"), "Select Time Frame for Time Plots",
                choices = list("30 mintes" = 2, "1 hour" = 1)),
    actionButton(ns("load_time"), "Load Bivariate Plot by Time"),
    actionButton(ns("load_all"), "Load Complete Bivariate Plot")  
  )
}

#' either plots a whole bivariate plot or an animation where each frame contais 30 mn of data
#' param @id: to link the input and the output
#' param @variables: the variables to plot
#' param @data: the data to work with

bivariate_server <- function(id, variables, data) {
  moduleServer(
    id,
    function(input, output, session){
      # pair plot reactive values
      pairplot <- reactiveValues(plot = NULL)
      
      # Observe event for pushing one of the buttons
      observeEvent(input$load_time,{
        form_label_x <- names(variables)[which(variables == input$pairplot_xvar)] # label for x axis
        form_label_y <- names(variables)[which(variables == input$pairplot_yvar)] # label for y axis
        
        frame <- as.double(input$frame) # what is the time frame
        each <- 3600/frame # divide hour by time frame 
        aval <- list()
        mod <- list()
        max_num = 24*frame # 24 times number of frames
        
        for(step in 1:max_num){ # for each step 
          # get the data at each time step
          x <- pull(data, input$pairplot_xvar)[seq((step-1)*(each) + 1, length = each)]  
          y <- pull(data, input$pairplot_yvar)[seq((step-1)*(each) + 1, length = each)]
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
      
      #plot whole graph
      observeEvent(input$load_all, {
        form_label_x <- names(variables)[which(variables == input$pairplot_xvar)] # label for x axis
        form_label_y <- names(variables)[which(variables == input$pairplot_yvar)] # label for y axis
        # getting data using pull()
        x <- pull(data,input$pairplot_xvar)
        y <- pull(data, input$pairplot_yvar)
        mod <- lm(y ~ x) # linear model for trendline
        covar <-  round(stats::cor(x,y),2)
        y2 <- c(min(fitted(mod)), max(fitted(mod)))
        if(covar < 0){
          y2 <- c(max(fitted(mod)), min(fitted(mod)))
        }
        fig <- plot_ly() %>% 
          layout(title = glue("{xvar} vs {yvar}", xvar = form_label_x, yvar = form_label_y), 
                 xaxis = list(title = form_label_x), yaxis = list(title = form_label_y)) # getting plot labels
        fig <- fig %>% add_markers(x = x, y = y, type = "scattergl", name = "Bivariate", 
                                   marker = list(color = "royalblue")) %>% 
          add_lines(x = c(min(x),max(x)), y = y2, 
                    name = glue("Correlation: {value}", value = covar), 
                    line = list(color = "firebrick", width = 4)) 
        # adding the bivariate scatter plot and the trend line  
        pairplot$plot <- fig
      })
      return (pairplot)
    }
  )
}






#' @title Server function 
#' @param input Input 
#' @param output Output. For details, checkout Shiny documentation
#' @param session Session. For details, checkout Shiny documentation
#' @import shiny
app_server <- function(input, output, session){
    # RAW DATA SECTION ------------------------------------------------------------------####
    data <- data_preview_server(id = "data_vars")
    
    # Histogram plots  
    histogram_server(id = "histo_plot", variables = raw_variables, data = data()) 
    # Variable vs time plot 
    plot_vvt_server(id = "time_plot", data = data(), varlist = raw_variables)
    
    # Bivariate plot 
    pairplot <- bivariate_server(id = "bivar_plot", variables = raw_variables, data = data())
    
    ## FULL DATA SECTION ----------------------------------------------------------------####
    
    full_data <- full_data_preview_server("NEE_table")
    
    # nee time plot
    full_time_plot_server("full_var_vs_time", full_data(), full_variables)
    
    # regression section 
    reg <- regression_server("reg_select", full_data())
}
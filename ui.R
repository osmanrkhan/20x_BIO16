# UI for the application
# Pierre and Quang
# Last Updated: 9/9/2020
library(shiny)
library(plotly)


options(shiny.maxRequestSize=50*1024^2)

# full_variables, #
navbarPage("Eddy Covariance Lab",
    tabPanel("Raw Data Exploration",
      fluidPage(
        tabsetPanel(
          # raw data panel-----------------------------------##
          tabPanel("Raw Data Loading", 
            data_preview_var_ui("data_vars", raw_data_files, site)
          ),
          # end of raw data panel -----------------------
          # histogram panel -----------------------------
          tabPanel("Histogram",
            histogram_ui(id = "histo_plot", variables = raw_variables)
          ),
          # end of histogram panel ------------------
          # variable vs time panel ------------------
          tabPanel("Observing Variable across Time",
            plot_vvt_ui(id = "time_plot", variables = raw_variables)
          ),
          # end of variable vs time panel -------------
          # beginning of bivariate plot panel -----------
          tabPanel("Bivariate Plots", 
              bivariate_ui("bivar_plot", raw_variables, raw_variables)
          )
          # end of the bivariate plot panel -------------
        ), # end of the tabpanel list ----------------
      ) # end of the fluid page -------------------------
    ), # end of the top tabpanel for raw data ---------------- 
    tabPanel("Processed Data Exploration", 
        fluidPage(
          tabsetPanel(
            tabPanel("Full Data Loading",
                     full_data_preview_ui("NEE_table")
            ), # end full data loading tab panel 
            # beginning variable vs time tab panel 
            tabPanel("Observing Variable across Time",
              # NEE variable vs time
              full_time_plot_ui("full_var_vs_time", full_variables[-which(full_variables %in% full_var_times)], 
                                full_variables[which(full_variables %in% full_var_times)])
            ),# end variable vs time tab panel 
            tabPanel("Regression Model",
              # Regression section
              h3("Building regression model"),
              regression_ui("reg_select", full_variables)
            )
          ) # end of tabset panel 
        ) # end of fluid page --------------------------
    ) # end of processed data tab panel -------------------
) # end of navbar page ---------------------------------------


  
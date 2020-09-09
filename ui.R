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
             fluidRow(
               column(4,
                      h5("In this section you can select which data set to load by selecting measurement site, 
                      season, and available dates within the season. The data set for this section are 10Hz measurements 
                      1 day (24 hours). For the sake of simplicity, all values are averaged within the second. In the box below, you can 
                         select site, as well as the specific combined data set (combining winter and summer) available for each site"),
                      tags$ul(
                        tags$li(tags$strong("second"),": Current time in seconds"),
                        tags$li(tags$strong("time"), ": Current time in POSIX format"),
                        tags$li(tags$strong("u"),": Horizontal Wind Speed (East) in meters per second"),
                        tags$li(tags$strong("v"),": Horizontal Wind Speed (North) in meters per second"),
                        tags$li(tags$strong("w"),": Vertical Wind Speed in meters per second"),
                        tags$li(tags$strong("airtemp"),": Sonic air temperature, calculated from speed of sound measurements in Celsius"),
                        tags$li(tags$strong("CO2"), ": CO2 mixing ratio (micro-mol CO2 per mol of air)"),
                        tags$li(tags$strong("H2O"), ": H2O mixing ratio (micro-mol H2O per mol of air)"),
                        tags$li(tags$strong("code"), ": Error code for sonic sensor paths")
                      ),
                      data_preview_var_ui("data_vars", raw_data_files, site),
                      tags$ul(
                        tags$li(tags$strong("comb_2018-06-05_2018-01-19"), ": Data set combining dates 06/05/2018 (summer) and 01/19/2018 (winter)"),
                        tags$li(tags$strong("comb_2018-06-06_2018-01-20"), ": Data set combining dates 06/06/2018 (summer) and 01/20/2018 (winter)")
                      )
               ),
               column(8,
                      tableOutput("preview")
               )
             )     
          ),
          # end of raw data panel -----------------------
          # histogram panel -----------------------------
          tabPanel("Histogram",
             h3("Histogram"),
             sidebarLayout(
               sidebarPanel(
                 h5("In this section, you can observe the distribution of values by plotting a histogram of the variable. 
                    You can plot histograms for each season, or super impose them on the same plot"),
                 histogram_ui(id = "histo_plot")
               ),
               mainPanel(
                 plotlyOutput("histogram")
               )
            )
          ),
          # end of histogram panel ------------------
          # variable vs time panel ------------------
          tabPanel("Observing Variable across Time",
             h3("Variable vs Time"),
             sidebarLayout(
               sidebarPanel(
                 h5("In this section, you can observe how the variable changes as a function of time across the seasons. This is an 
                    interactive plot that allows you to also use a slider to observe the data at pre-specified or custom timescales"), 
                 h5("On top of each plot there are 3 buttons (1 hour, 30 minutes, all) that represents the different scales you can 
                    observe the plot. "),
                 tags$ul(
                   tags$li(strong("all"), " allows you to see the zoomed out plot encompassing the entire day"),
                   tags$li(strong("1 hour"), " allows you to restrict the zoom (the area between the two bars on the bottom panel of each plot) to
                           a 1 hour interval"),
                   tags$li(strong("30 minutes"), " allows you to restrict the zoom (the area between the two bars on the bottom panel of each plot) to
                           a 30 minute interval")
                 ),
                 plot_vvt_ui(id = "time_plot")
               ),
               mainPanel(
                 plotlyOutput("vvt_plt")
               )
             )
          ),
          # end of variable vs time panel -------------
          # beginning of bivariate plot panel -----------
          tabPanel("Bivariate Plots", 
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
                 bivariate_ui("bivar_plot")
               ),
               mainPanel(
                 plotlyOutput("pairplot")
               )
             ),
          )
          # end of the bivariate plot panel -------------
        ), # end of the tabpanel list ----------------
      ) # end of the fluid page -------------------------
    ), # end of the top tabpanel for raw data ---------------- 
    tabPanel("Processed Data Exploration", 
        fluidPage(
          tabsetPanel(
            tabPanel("Full Data Loading",
              fluidRow(
                column(4,
                       h5("Using an Eddy Covariance solver (such as the ", 
                          tags$a(href="https://www.licor.com/env/support/EddyPro/software.html", "EddyPro7"),
                          "from LICOR Biosciences, who also provided the measurement devices), researchers can estimate CO2 flux and Net Ecosystem Exchange (NEE) from raw data. 
                          The most common approach is to solve the flux at each half-hour interval. Here in this section we have a data set for the same site (Silas Little) that has been processed 
                          using an Eddy Covariance solver. The data is for the entire year (instead of only one day), and has estimations every half hour. The full data includes the following variables:"),
                       tags$ul(
                         tags$li(tags$strong("date"),": Current date/time in POSIX format"),
                         tags$li(tags$strong("JD"), ": Julian Day, a numeric counter from 1 until 365"),
                         tags$li(tags$strong("NEE"),": Net ecosystem exchange"),
                         tags$li(tags$strong("PPFD_in"),": Photosynthetically active radiation"),
                         tags$li(tags$strong("TA"),": Air temperature"),
                         tags$li(tags$strong("RH"),": Relative humidity"),
                         tags$li(tags$strong("Vol.W.C"), ": Soil Moisture"),
                         tags$li(tags$strong("Soil.T"), ": Soil Temperature"),
                         tags$li(tags$strong("season"), ": Current season (either Winter (W) or Growing Season (GS))"),
                         tags$li(tags$strong("timeofday"), ": Time of day (either Night or Day)")
                       )
                ),
                column(8,
                       tableOutput("full_preview")
                )
              )
            ), # end full data loading tab panel 
            # beginning variable vs time tab panel 
            tabPanel("Observing Variable across Time",
              # NEE variable vs time
              h3("Observing variables changing by different units of time"),
              sidebarLayout(
                sidebarPanel(
                  h5("In this section you can observe how different meterological variables of interest change over time. 
                     You can plot changes by day as a count from 1 to 365, by season (Winter and Growing Season), by hour of day, or by time of day (Night vs Day)"),
                  full_time_plot_ui("full_var_vs_time", full_variables[-which(full_variables %in% full_var_times)], 
                                    full_variables[which(full_variables %in% full_var_times)])
                ),
                mainPanel(
                  plotOutput("full_time_plot")
                )
              )
            ),# end variable vs time tab panel 
            tabPanel("Regression Model",
              # Regression section
              h3("Building regression model"),
              regression_ui("reg_select", full_variables)
            ),
          ) # end of tabset panel 
        ) # end of fluid page --------------------------
    ) # end of processed data tab panel -------------------
) # end of navbar page ---------------------------------------


  
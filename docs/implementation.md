# EddyCovariance (Bio 16 Laboratory) Web Application Implementation
### Quang Mnguyen and Pierre Desvallons, DIFUSE September 2020.

## App structure

- As with any web package R shiny needs a ui to create the view and a server to handle requests. The ui function is responsible for creating the views for each input (also called widgets) and output. The server app is responsible for building the output objects usually by using the widget values. Basic examples can be found at [Shiny Tutorials](https://shiny.rstudio.com/tutorial/written-tutorial/lesson1) read through lesson 4.  

- As stated in the design, the website is broken into module to keep the UI and server functions from congesting. So the overarching UI calls the function of each module's ui; the same is true for the server. The server and UI are connected through IDs. Input and output IDs in Shiny apps share a global namespace, meaning, each ID of shiny objects must be unique across the entire app. However, when modularizing the application, shiny gives you the possibility to create a namespace for each module. In consequence, the programmer doesn't have to worry about uniqueness of input/output ids between modules. For more information about modules, please read [shiny module](https://shiny.rstudio.com/articles/modules.html).
- Helpers.r: This file contains helper functions to the module files and the global variables that is shared by multiple files in the app.
- It is assumed that the programmer knows the names of each column in the data sets and is able to create a global named list containing the names of the column. All data sets use RDS format.


## Raw Data section

### Data loading Module
The data set for this section are 10Hz measurements for 1 day (24 hours). For the sake of simplicity, all values are averaged within the second. To make filtering out seasons easier and faster, the data set used is a combination of a full day in summer and full day in winter.

```r
#' lets the user select the site, season and date of the file
#' param @id: a string to link input and output
#' param @datasets: a list of available datasets
#' param @site: a named list of possible sites where the data comes from
#' @return a taglist of input
data_preview_var_ui <- function(id, datasets, site)

#' constructs the path to the file chosen
#' param @id: to link the input and the output
#' param @start_path: starting path to be added to the full path
#' @return the loaded data
data_preview_server <- function(id, start_path)
```
The `data_preview_var_ui` function is to create an ui that lets the user choose the dataset that they want to work with. The site and datasets variables are used to create 2 select input options ([selectInput()](https://shiny.rstudio.com/reference/shiny/1.5.0/selectInput.html)) that allows the user to select its dataset. The function return a list of shiny inputs to the main ui file.

 The `data_preview_var_server` function constructs the data path to the data and loads the data by calling readRDS. At the end the data is return to the server file that calls the function.

### Histogram Module
In this module the user is able to graph distribution of any variables and observe the difference between of the distribution in winter and summer.

```r
#' lets the user select the variables to be graphed
#' param @id: to link input and output
#' param @variables: a named list of variables to choose from
#' @return a taglist of input function
histogram_ui <- function(id, variables)

#' Plot the histogram for the variable chosen
#' @param id: to link the input and the output
#' @param variables: the variables to plot -- used to get nice labels on axes
#' @param data: the data to work with -- usually a reactive object
#' @return a plotly object
histogram_server <- function(id, variables, data)
```

The `histogram _ui` function creates two select input ([selectInput](https://shiny.rstudio.com/reference/shiny/1.5.0/selectInput.html) options, one slider input ([sliderInput](https://shiny.rstudio.com/reference/shiny/1.5.0/sliderInput.html)), and  one action button input ([actionButton()](https://shiny.rstudio.com/reference/shiny/1.5.0/actionButton.html)). The first select input option is to allow the user to select the variable to graph; The second input section allows the user to choose the season; The slider input allows the user to change the number of bins and lastly when the action button is pressed A new histogram will change according to the new input.

 The `histogram _server` functions plot the histogram based on the variable chosen, number of bins, and season. The [eventReactive](https://shiny.rstudio.com/reference/shiny/1.5.0/observeEvent.html) function is used on the action button in `histogram _ui` in order to ensure that the server doesn't change the output unless the user presses the action Button. Once the action button is pressed, a plotly object ([plotly](https://plotly.com/r/)) is initialized, then based on the input season we add an histogram ([add_histogram](https://plotly.com/r/histograms)) to the plotly object. Lastly, we add some formatting to the graph by calling the plotly function `layout`. It is important to note that `barmode = "overlay"` argument allows two histograms to be overlaid.

### Variable vs Time Module

### Bivariate Plot Module
```r
#' lets the user select the variables to be graphed
#' param @id: to link input and output
#' param @xvar: the x variables
#' param @yvar: the y variables
#'
bivariate_ui <- function(id, xvar, yvar)

#' either plots a whole bivariate plot or a bivariate plot animation through time.
#' param @id: to link the input and the output
#' param @variables: the variables to plot
#' param @data: the data to work with

bivariate_server <- function(id, variables, data)

#' helper function to plot a whole bivariate plot or a bivariate plot animation through time.
#' param @input: list of input that user has chosen (given to us by shiny).
#' param @output: list of available ouput action (given to us by shiny).
#' param @variables: the variables to plot.
#' param @data: the data to work with.
#' param @season: which season to graph

plot_raw_data_bivariate <- function(input, output, variables, data, season) #called by bivariate server
```

In this module, the bivariate of two variables is plotted using two methods.

The first method is a simple bivariate plot of two variables.
In the second method a slider is created to show the bivariate relationship through time (24 hours) for a fixed (user-selected interval). In other words, the data is divided into chunks(or frames) of a fixed, user-selected time interval, then the bivariate relationship for each frame was graphed. Finally, a slider was created to allow the user to switch in between frames. For example, if the interval in 30 minutes, the first frame would graph data from 0:00 - 0:30, the second frame would graph data from 0:30 - 1:00 etc...

The `bivariate_ui` function creates three select inputs ([selectInput](https://shiny.rstudio.com/reference/shiny/1.5.0/selectInput.html) options. Two of these select input allow the user to select the variables to plot, and the last allows the user to choose the time interval for the bivariate plot through time.  Two action buttons ([actionButton()](https://shiny.rstudio.com/reference/shiny/1.5.0/actionButton.html)) are used to let the user choose which graph to plot. A navigation panel is used to separate the data for summer from winter. See [navbarPage](https://shiny.rstudio.com/reference/shiny/1.0.5/navbarPage.html).

The `bivariate_server` makes use of the [eventReactive](https://shiny.rstudio.com/reference/shiny/1.5.0/observeEvent.html) option that R shiny provides to observe when the user presses the action buttons.

When the user selects the option to plot the bivariate through time, the following takes place. A list of frames is created where each frame keeps track of its own x and y values, its own boolean variable that checks if it should be visible, and finally its name. Then the frames are added to a plotly object by looping through the frames. For each iteration we call add_markers to add the frame to the plotly object. The slider keeps track of what frame should be visible when the user moves the pointer of the slider by creating a list of `visible` variables for each frame. The idea is to sequentially associate each frame to its correct position in that slider. Finally the slider is integrated into the plotly object by using the sliders option in [layout](https://plotly.com/r/reference/#layout-updatemenus). For more information, see plotly slider in R tutorial [here](https://plotly.com/r/sliders/).

When the user chooses to graph the whole bivariate plot, [ggplot](https://www.rdocumentation.org/packages/ggplot2/versions/3.3.2/topics/ggplot)(with geom_point option) is used over plotly because the overhead of plotly is not needed since we just want to show a simple static graph.

## Processed Data section

The data is for the entire year (instead of only one day), and has estimations every half hour. We also keep track of the season, and time for each data point.

### Variable vs Time Module
```r

#' lets the user select the variables to be graphed
#' param @id: to link ui and server
#' param @y_variables: y variables to select
#' param @x_variables: x variables to select
#'
full_time_plot_ui <- function(id, y_variables, x_variables)

#' returns a ggplot graph depending on which type of graph was selected
#' @param id: id that connects the ui to the server
#' @param data: the data to work with. a reactive variable
#' @param variables: the variables to graph to make nice axis
full_time_plot_server <- function(id, data, varlist)
```

The `full_time_plot_ui` function creates two select input ([selectInput](https://shiny.rstudio.com/reference/shiny/1.5.0/selectInput.html) options to allow the user to select the time variable and the Y variable to graph , and  one action button input ([actionButton()](https://shiny.rstudio.com/reference/shiny/1.5.0/actionButton.html)) to load the graph.

The `full_time_plot_server` function plots the Y variables through time based on the unit of time selected. [ggplot](https://www.rdocumentation.org/packages/ggplot2/versions/3.3.2/topics/ggplot) was used because it's faster than most other graphing library.(Specifically, in this case plotly).


### Regression Module

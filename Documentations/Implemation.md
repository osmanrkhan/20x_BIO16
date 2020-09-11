# EddyCovariance (Bio 16 Laboratory) Web Application Implementation
### Quang Mnguyen and Pierre Desvallons, DIFUSE September 2020.

## R Shiny flow
This is a quick overview on the flow of data structure through r shiny. This is meant to be introductory, please read the references for a clearer picture.

1. R shiny has many types input/output functions that programmers can call depending on their need. Each input/output function takes a string id that shiny uses to reference to that input/output. Because of that, Shiny requires the ids to be unique. For more on input/output function(r shiny link to i/o functions).
2. The ui function is responsible for creating the views for each input and output. Meanwhile, the inputs/outputs are passed in to the server as a named list, where the names are the ids used in the ui.
3. The id parameter is used to link the ui with its server. The id is also used to create a namespace for the module, so that the programmer doesn't have to worry about uniqueness of input/output ids between modules. For more information about that, please read shiny module(link).

## Raw Data section
It is assumed that the programmer knows the names of each column in the datasets and is able to create a global named list for each of the column of the dataset.
### Data loading Module
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
The `data_preview_var_ui` function is to create an ui that lets the user choose the dataset that they want to work with. The site and datasets variables are used to create 2 select input options (see selectInput link) that allows the user to select its dataset. The function return a list of shiny inputs to the main ui file.

 The `data_preview_var_server` function constructs the data path to the data and loads the data by calling readRDS. At the end the data is return to the server

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

The `histogram _ui` function creates two select input (see selectInput link) options, one slider input (see sliderInput link), and  one action button input (see actionButton link). The first select input option is to allow the user to select the variable to graph; The second input section allows the user to choose the season; The slider input allows the user to change the number of bins and lastly when the action button is pressed A new histogram will change according to the new input.

 The `histogram _ui` functions plot the histogram based on the variable chosen, number of bins, and season. The `eventReactive (link)` function is used on the action button in `histogram _ui` in order to ensure that the server doesn't change the output unless the user presses the action Button. Once the action button is pressed, a plotly object (plotly link) is initialized, then based on the input season we add an histogram(plotly add_histogram link) to the plotly object. Lastly, we add some formatting to the graph by calling the plotly function `layout(link)`. It is important to note that `barmode = "overlay"` argument allows two histograms to be overlaid.

### Variable vs Time Module

### Bivariate plots Module


## Processed Data section

### Variable vs Time Module

### Regression Module

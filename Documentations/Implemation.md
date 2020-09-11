# EddyCovariance (Bio 16 Laboratory) Web Application Implementation
### Quang Mnguyen and Pierre Desvallons, DIFUSE September 2020.

## R Shiny structure
This is a quick overview on the structure of r shiny. This is meant to be introductory, please read the references for a clearer picture.

- R shiny needs a ui and server function to work. The ui function is responsible for creating the views for each input (also called widgets) and output. The server app is responsible for building the output objects usually by using the widget values. [Shiny Tutorials](https://shiny.rstudio.com/tutorial/written-tutorial/lesson1) read through lesson 4.
- Input and output IDs in Shiny apps share a global namespace, meaning, each ID must be unique across the entire app. However, when modularizing the application, shiny gives you the possibility to create a namespace for each module. In consequence, the programmer doesn't have to worry about uniqueness of input/output ids between modules. For more information about modules, please read [shiny module](https://shiny.rstudio.com/articles/modules.html).

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
The `data_preview_var_ui` function is to create an ui that lets the user choose the dataset that they want to work with. The site and datasets variables are used to create 2 select input options [selectInput()](https://shiny.rstudio.com/reference/shiny/1.5.0/selectInput.html) that allows the user to select its dataset. The function return a list of shiny inputs to the main ui file.

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

The `histogram _ui` function creates two select input options, one slider input [sliderInput](https://shiny.rstudio.com/reference/shiny/1.5.0/sliderInput.html), and  one action button input [actionButton()](https://shiny.rstudio.com/reference/shiny/1.5.0/actionButton.html). The first select input option is to allow the user to select the variable to graph; The second input section allows the user to choose the season; The slider input allows the user to change the number of bins and lastly when the action button is pressed A new histogram will change according to the new input.

 The `histogram _ui` functions plot the histogram based on the variable chosen, number of bins, and season. The `[eventReactive](https://shiny.rstudio.com/reference/shiny/1.5.0/observeEvent.html)` function is used on the action button in `histogram _ui` in order to ensure that the server doesn't change the output unless the user presses the action Button. Once the action button is pressed, a plotly object [plotly](https://plotly.com/r/) is initialized, then based on the input season we add an histogram [add_histogram](https://plotly.com/r/histograms) to the plotly object. Lastly, we add some formatting to the graph by calling the plotly function `layout`. It is important to note that `barmode = "overlay"` argument allows two histograms to be overlaid.

### Variable vs Time Module

### Bivariate plots Module


## Processed Data section

### Variable vs Time Module

### Regression Module

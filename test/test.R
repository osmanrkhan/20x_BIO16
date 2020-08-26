library(tidyverse)
library(plotly)

number_of_charts <- seq(1,100)
fun_factor <- number_of_charts^3
plot_ly(x = number_of_charts, y = fun_factor) %>% layout(title = "Cubic Fit of # of charts vs fun factor",
                                                         xaxis = list(title = "Number of Charts"), 
                                                         yaxis = list(title = "Fun Factor"))

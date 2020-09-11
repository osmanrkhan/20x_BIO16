# EddyCovariance (Bio 16 Laboratory) Web Application Design
### Quang Mnguyen and Pierre Desvallons, DIFUSE September 2020.

This file will give a high level design for the shiny app that was developed for the BIO 16 EddyCovariance Lab.

## User Interface
From an User point of view the application is divided into two big sections: The raw data exploration and the processed section. Each section contains some modules, the first of which is the loading of the appropriate dataset. In each module, there are texts to help the user understand how each input options can manipulate the output.

## Architecture
The typical Shiny application has one UI (front end) function and server (back end) function each in their own separate files located in the same directory. However, as the application gets more complex, it is recommended to modularize the code to avoid congestion and increase readability. This is usually done by having one overachieving UI and server functions that call other smaller UI and server functions that are written in other files in a directory called R.
This application is modularized by grouping together the ui and server for each module in a file. In this way, the main ui and server functions don't need to worry about the implementation details for each module.

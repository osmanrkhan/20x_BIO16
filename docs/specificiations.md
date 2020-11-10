# Eddy Covariance Web Application Design  
## Quang Nguyen and Pierre Desvallons  
## DIFUSE Program   
## September 2020     

### This file will give a high level design for the shiny app that was developed for the BIO 16 Eddy Covariance Lab.  

## User Interface  
From an User point of view the application is divided into two big sections: The raw data exploration and the processed section. Each section contains some modules, the first of which is the loading of the appropriate dataset. In each module, there are texts to help the user understand how each input options can manipulate the output.  

## Architecture  
The typical Shiny application has one UI (front end) function and server (back end) function each in their own separate files located in the same directory. However, as the application gets more complex, it is recommended to modularize the code to avoid congestion and increase readability. This is usually done by having one main UI and server functions that call other smaller UI and server functions that are written in other files in a directory called R.
This application is modularized by grouping together the ui and server for each module in a file. In this way, the main ui and server functions don't need to worry about the implementation details for each module.  

## Deployment  
This app is deployed through shinyapps.io which is the most straightforward way to deploy shiny applications and it's easier with RStudio installed.    

1. Download the repository from github using `git clone`  
2. Install all dependencies through `renv::restore()`. Make sure `renv` is installed first (`install.packages('renv')`).  Ensure that `rsconnect` is installed as well (although if deploying through RStudio, there will be an automatic prompter which allows the option of installing `rsconnect`)   
3. Connect an account through RStudio. If using the GUI, Navigate to Tools > Global Options > Publishing > Connect where there will be instructions to connect your RStudio (and by extension, your R sessions) to your shinyapps.io account. If connecing through R terminal instead, use the command `rsconnect::setAccountInfo()`. Options can be further explored through `rsconnect` documentation.  
4. After connecting to an account, the application can be deployed. If using the RStudio GUI, navigate to the "publish" button on the top right hand corner where you can also find buttons for "insert" and "run". Click publish, or use the drop down menu for more options. In the subsequent deployment menu, you can select which files to include for the app. **Please only deploy the `data\`, `R\` folders and the files `ui.R` and `server.R`**. If using R Terminal instead, the command is: `rsconnect::deployApp(appFiles = c("data/","R/", "ui.R", "server.R"), appTitle = "Eddy Covariance Lab", account = "difuse-dartmouth")`. Make sure to set the working directory to the primary folder (where `ui.R` and `server.R` is housed) before running the command.  

# Eddy Covariance interactive lesson   
# Quang Nguyen & Pierre G. Desvallons  

This is an RShiny application developed for students to interact with Eddy Covariance data. In this module, students can visualize raw and processed Eddy Covariance data across time and season, view bivariate plots, and perform basic gapfilling by building a regression model. Currently this project is hosted at: [shinyapps.io](https://difuse-dartmouth.shinyapps.io/DIFUSE_2020_EddyCovariance/)  


### Folder structure   

`data/`: To hold any data needed. `processed_data/` housed processed data in `.rds` format. `data_wrangling.R` is a file that contains all data wranging instructions       
`R/`: All containing Rscripts labelled by function. `full` describes scripts associated with the processed data and `raw` describes scribes associated with the raw data.  
`Documentations/`: Any documents needed such as tutorials, data annotations, etc.  

`init.R`, `run.R` are files associated with deployment on Heroku. For shinyapps.io, those files should not be exported. Please double check that all dependencies are represented in `init.R` before deploying to Heroku.  

`server.R` and `ui.R` are the master files containing UI elements (`ui.R`) and server logic (`server.R`). Functionalities are broken down to modules represented by functions housed in scripts located in the `R` folder. For shiny applications, the app automatically sources all scripts in the `R` folder. Any scripts outside this folder will not be sourced and such functions will not be available to the application.  

### Dependencies    

All dependencies will be managed using `renv`. When developing the package, please install `renv` from CRAN, then run `renv::restore()` to create a local R package environment. Any installations to packages should be done via `renv::install()` to ensure package versions are compliant.  



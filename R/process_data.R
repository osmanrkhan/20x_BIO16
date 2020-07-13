library(tidyverse)
library(readr)
library(glue)

# This function takes a loaded data file and does the processing
process_dat <- function(data){
  data <- data %>% filter_all(all_vars(!is.na(.))) %>% slice(1:864000) # remove all rows with na and remove the odd observations
  data <- data %>% mutate(second = rep(1:(nrow(data)/10), each = 10)) %>%  # get second
    mutate(minute = rep(1:(nrow(data)/600), each = 600)) %>%  # get minute
    mutate(hour = rep(1:(nrow(data)/36000), each = 36000))  # get hour
  data <- data %>% group_by(second) %>% summarise_all(mean) #average by second
  return(data)
}

# Wrapper function to loop through all the files 
process_files <- function(filenames, season, site){
  for (i in filenames){
    data <- read_csv(i)
    data <- process_dat(data)
    time <- str_replace_all(strsplit(tail(strsplit(i, "/")[[1]],1),".", fixed = T)[[1]][1], " ", "_") 
    saveRDS(data,file = glue("data/processed_data/{site}_{season}_{time}.rds", time = time, season = season, site = site))
  }
}

summer_silas <- list.files("data/raw_data/Silas Little Eddy Covariance Data/Summer 2018 Datasets/raw sonic files/", 
                    full.names = T)
winter_silas <- list.files("data/raw_data/Silas Little Eddy Covariance Data/Winter 2018 Datasets/Raw sonic files/",
                           full.names = T)

process_files(summer_silas, season = "summer", site = "silas_little")
process_files(winter_silas, season = "winter", site = "silas_little")

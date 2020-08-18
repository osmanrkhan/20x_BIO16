library(tidyverse)
library(lubridate)

all_data <- map(list.files("data/processed_data/", full.names = T), readRDS)
names(all_data) <- strsplit(list.files("data/processed_data"), ".rds") %>% as_vector()

# processed each data set and then save the combined files 
get_combined_data <- function(summer, winter, dte_summer, dte_winter){
  summer <- summer %>% mutate(season = rep("summer", nrow(summer))) %>% 
    select(-c(minute, hour)) %>%
    mutate(time = calculate_time(dte_summer))
  winter <- winter %>% mutate(season = rep("winter", nrow(winter))) %>%
    select(-c(minute, hour)) %>%
    mutate(time = calculate_time(dte_winter))
  saveRDS(summer, glue("data/processed_data/silas_little_summer_{dte_summer}.rds",
                       dte_summer = dte_summer))
  saveRDS(winter, glue("data/processed_data/silas_little_summer_{dte_winter}.rds",
                       dte_winter = dte_winter))
  comb <- rbind(summer, winter)
  saveRDS(comb, file = glue("data/processed_data/silas_little_comb_{dte_summer}_{dte_winter}.rds", dte_summer = dte_summer, 
                            dte_winter = dte_winter))
  return(0)
}

# This function is used to calculate a date time column incremented by 1 second
calculate_time <- function(date){
  date_str <- glue("{date} 00:00:00", date = date)
  d1 <- as.POSIXct(date_str, tz = "America/New_York")
  d1 <- d1 + seconds(0:86399)
  return(d1)
}

get_combined_data(summer = all_data$silas_little_summer_June_5_2018,
                  winter = all_data$silas_little_winter_Jan_19_2018, 
                  dte_summer = "2018-06-05",
                  dte_winter = "2018-01-19")

get_combined_data(summer = all_data$silas_little_summer_June_6_2018,
                  winter = all_data$silas_little_winter_Jan_20_2018,
                  dte_summer = "2018-06-06",
                  dte_winter = "2018-01-20")

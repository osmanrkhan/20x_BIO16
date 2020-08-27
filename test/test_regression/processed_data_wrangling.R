library(tidyverse)
library(readxl)
library(glue)
library(lubridate)

data <- read.csv(file = "data/raw_data/Silas_2018.csv")

generate_hrmm <- function(date){
  minute <- ifelse(as.numeric(minute(date)) == 0, 0, 0.5) # if minute = 30, then it's half of an hour
  hour <- as.numeric(hour(date))
  return(hour + minute) # return numeric vector 
}

data <- data %>% mutate(HR.MIN = sprintf("%04d", HR.MIN)) %>% 
  mutate(date = glue("{year}-{md} {hm}", year = Year, md = mm.dd, hm = HR.MIN)) %>% # glue columns together
  mutate(date = as.POSIXct(strptime(date, format = "%Y-%d-%b %H%M"))) %>% # convert to POSIX time 
  mutate(hrmm = generate_hrmm(date)) %>% # generate a proper hr:min column
  mutate(season =  ifelse(JD>145 & JD<300,"GS", "W")) %>% # defined season 
  mutate(timeofday = ifelse(hrmm>8 & hrmm<20,"Day", "Night")) %>%
  select(date, JD, hrmm, NEE, PPFD_in, TA, RH, Vol.W.C, Soil.T, season, timeofday) %>%
  relocate(date) %>% filter(!is.na(PPFD_in))


saveRDS(data, file = "data/processed_data/silas_little_2018.rds")





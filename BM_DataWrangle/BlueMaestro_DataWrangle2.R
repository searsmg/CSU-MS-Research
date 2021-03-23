library(dplyr)
library(lubridate)
library(tidyverse)

rm(list = ls()) 

setwd("C:/Users/sears/Documents/Research/Snow_Hydro_Research/BM Sensors/Field_Data_011821/Processed/")

BM_reformat <- read.csv("C:/Users/sears/Documents/Research/Snow_Hydro_Research/BM Sensors/Field_Data_011821/Processed/BM_use.csv")

str(BM_reformat) #check date formats

#get all date and time formats into POSIXct
BM_reformat <- BM_reformat %>%
  mutate(datetime = mdy_hm(datetime)) %>%
  mutate(date = mdy(date)) %>%
  mutate(sunrise=mdy_hm(sunrise)) %>%
  mutate(solarNoon=mdy_hm(solarNoon)) %>%
  mutate(sunset=mdy_hm(sunset))
         
test <- BM_reformat %>%
  spread(ID, temperature)
  
  
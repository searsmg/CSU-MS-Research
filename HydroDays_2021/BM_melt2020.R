#############################################
#####        HYDRO DAYS 2021            #####
#############################################

library(ggplot2)
library(dplyr)
library(lubridate)

rm(list=ls())

## data prep

#sensor data from install to Jan 2021
BM <- read.csv("C:/Users/sears/Documents/Repos/CSU-MS-Research/HydroDays_2021/BMdata.csv") %>% 
  mutate(date = mdy(date)) %>% 
  mutate(time = hm(time))

#subset for melt 2020 season (May 1 [install] to July 1 2020)
melt20 <- BM %>%
  filter(date >= as.Date("2020-05-01")) %>%
  filter(date <= as.Date("2020-07-01"))

#read in elev data
elev <- read.csv("C:/Users/sears/Documents/Repos/CSU-MS-Research/HydroDays_2021/elev_data.csv")




melt20$mean <- rowMeans(melt20[,3:16], na.rm=TRUE)

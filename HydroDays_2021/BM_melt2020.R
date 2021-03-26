#############################################
#####        HYDRO DAYS 2021            #####
#############################################

library(ggplot2)
library(dplyr)
library(lubridate)

rm(list=ls())

## data prep

#read in elev data
elev <- read.csv("C:/Users/sears/Documents/Repos/CSU-MS-Research/HydroDays_2021/elev_data.csv")

slope <- read.csv("C:/Users/sears/Documents/Repos/CSU-MS-Research/HydroDays_2021/melt2020_slope.csv")%>%
  mutate(date.time = mdy(Datetime)) %>% 
  mutate(time = hms(time))


slope$time0 <- 0
ggplot(slope)+ geom_raster(aes(x=date.time, y=time, fill=slope))

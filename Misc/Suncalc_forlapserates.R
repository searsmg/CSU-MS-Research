library(dplyr)
library(suncalc)
library(lubridate)

rm(list = ls()) 

setwd("C:/Users/sears/Documents/Research/Snow_Hydro_Research/BM Sensors/Field_Data_011821/Processed/")

BM <- read.csv("C:/Users/sears/Documents/Research/Snow_Hydro_Research/BM Sensors/Field_Data_011821/Processed/MP_HD_All_Jan21.csv")%>%
  mutate(datetime = mdy_hm(datetime))

date <- seq.Date(from = as.Date("2020-04-19", tz="MST"),
                 to=as.Date("2021-01-14", tz="MST"),
                 by = 1)

sunlightdata <- getSunlightTimes(date=date,
                                 lat=40.544,
                                 lon=-105.887,
                                 tz="MST",
                                 keep=c("sunrise", "solarNoon", "sunset"))

BM_daily <- BM %>%
  mutate(day = as.Date(datetime, format="%Y-%m-%d")) %>%
  group_by(ID, Elevation,day) %>%
  summarise(daily_avgtemp = mean(temperature))

write.csv(BM_daily, "DailyAvgT.csv")


BM <- BM %>%
  mutate(date = as.Date(datetime, format="%Y-%m-%d"))
  
BM_light <- merge(BM, sunlightdata, by="date")

BM_light <- BM_light %>%
  mutate(time = (datetime, format="%H:%M:%S"))

BM_light <- BM_light %>%
  mutate(light=if_else(datetime>sunrise & datetime<sunset,"day", "night"))

write.csv(BM_light, "BM_light.csv")




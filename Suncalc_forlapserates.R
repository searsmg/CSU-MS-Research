library(dplyr)
library(suncalc)
library(lubridate)

rm(list = ls()) 

setwd("C:/Users/sears/Documents/Research/Snow_Hydro_Research/BM Sensors/Field_Data_011821/Processed/")

BM <- read.csv("C:/Users/sears/Documents/Research/Snow_Hydro_Research/BM Sensors/Field_Data_011821/Processed/MP_HD_All_Jan21.csv")%>%
  mutate(datetime = mdy_hm(datetime))

date <-


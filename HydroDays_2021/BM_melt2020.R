#############################################
#####        HYDRO DAYS 2021            #####
#############################################

library(ggplot2)
library(dplyr)
library(lubridate)

rm(list=ls())

## data prep

#read in elev data
melt <- read.csv("C:/Users/sears/Documents/Research/Snow_Hydro_Research/HydroDays21_figs/MeltSum_CumSum.csv")

melt$datetime = mdy_hm(paste(melt$Datetime, melt$time))

ggplot(melt)+geom_line(aes(x=datetime, y=ObsCumSum_Melt_mm))+
  geom_line(aes(x=datetime, y=ELRCumSum_Melt_mm))

ggplot(melt)+geom_point(aes(x=ELRMelt_mm, y=ObsMelt_mm))+
  geom_abline(intercept = 0, slope = 1)

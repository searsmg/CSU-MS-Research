# calibrating MELT FACTORS for melt 2021

library(ggplot2)
library(lubridate)
library(dplyr)

rm(list = ls()) 

#set working directory and csv file
setwd("C:/Users/sears/Documents/Research/Snow_Hydro_Research/Thesis/Data/SNOTEL")

sno21 <- read.csv("JW21.csv", header=TRUE) %>%
  mutate(Date = mdy_hm(Date))

#convert to metric units and get rid of the others
sno21 <- sno21 %>%
  mutate(Ta_C = (AirT_F-32)*(5/9),
         PrecipAcum_mm = PrecipAcum_in*25.4,
         Sd_mm = SnowDepth_in*25.4,
         SWE_mm = SWE_in*25.4) %>%
  select(-c(AirT_F, PrecipAcum_in, SnowDepth_in, SWE_in))


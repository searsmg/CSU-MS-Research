### Script to calculate Melt 21 data ###

library(ggplot2)
library(lubridate)
library(dplyr)
library(plotly)
library(tidyverse)

rm(list = ls()) 

#set working directory and csv file
setwd("C:/Users/sears/Documents/Research/Snow_Hydro_Research/Thesis/Data/Air Temp/For R")

#temp, elevation, and elevation band data
temp_elev_21 <- read.csv(file="Melt21.csv", header=TRUE) %>%
  mutate(Datetime = mdy_hm(Datetime))

#get a temp for each elev band (grouping bands and getting avg temp)
bandtemps <- temp_elev_21 %>%
  group_by(Datetime, Band) %>%
  summarize(bandT = mean(AirT_C, na.rm=TRUE))

bandT_long <- bandtemps %>%
  pivot_wider(names_from = Band, values_from = bandT)

write.csv(bandT_long, "bandT.csv")

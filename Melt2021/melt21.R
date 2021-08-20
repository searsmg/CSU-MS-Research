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
temp_elev_21 <- read.csv(file="Melt21_elev_noOUT.csv", header=TRUE) %>%
  mutate(Datetime = mdy_hm(Datetime))

#get a temp for each elev band (grouping bands and getting avg temp)
#bandtemps <- temp_elev_21 %>%
#  group_by(Datetime, Band) %>%
#  summarize(bandT = mean(AirT_C, na.rm=TRUE))

#bandT_long <- bandtemps %>%
#  pivot_wider(names_from = Band, values_from = bandT)

#write.csv(bandT_long, "bandT.csv")

#bring band T back in - have all Ta for melt 21
bandT <- read.csv(file="bands21_interpol.csv", header=TRUE) %>%
  mutate(Datetime = mdy_hm(Datetime))

#start working on rad - this is 10 min data and hourly avg is every 7 rows
rad21 <- read.csv(file="C:/Users/sears/Documents/Research/Snow_Hydro_Research/Thesis/Data/Radiation/For R/Rad_melt21.csv", 
                  header=TRUE) %>%
  mutate(Datetime = mdy_hm(Datetime))

#get hourly rad data
rad21 <- rad21 %>%
  filter(minute(Datetime) == 0) %>%
  filter(row_number() %% 2 != 1) ## Delete odd-rows

#break bands to own df
  


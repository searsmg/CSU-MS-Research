### Script to calculate Melt 21 data ###

library(ggplot2)
library(lubridate)
library(dplyr)
library(plotly)
library(tidyverse)

rm(list = ls()) 

#set working directory and csv file
setwd("C:/Users/sears/Documents/Research/Snow_Hydro_Research/Thesis/Data/Air Temp/For R")

##########################################################################
#### PROCESSING ####

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

#get rid of the minute on date time (e.g., 14:01 - want 14:00)
minute(bandT$Datetime) <- 0

#start working on rad - this is 10 min data and hourly avg is every 7 rows
rad21 <- read.csv(file="C:/Users/sears/Documents/Research/Snow_Hydro_Research/Thesis/Data/Radiation/For R/Rad_melt21.csv", 
                  header=TRUE) %>%
  mutate(Datetime = mdy_hm(Datetime))

#get hourly rad data
rad21 <- rad21 %>%
  filter(minute(Datetime) == 0) %>%
  filter(row_number() %% 2 != 1) ## Delete odd-rows

#add rad to bandT
bandT_rad <- merge(bandT, rad21, by = "Datetime")

#now break each band out so we can calculate rad and melt
b1 <- bandT_rad %>%
  select(Datetime, B1, Swin, Swout, Lwin, Lwout)

b2 <- bandT_rad %>%
  select(Datetime, B2, Swin, Swout, Lwin, Lwout)

b3 <- bandT_rad %>%
  select(Datetime, B3, Swin, Swout, Lwin, Lwout)

b4 <- bandT_rad %>%
  select(Datetime, B4, Swin, Swout, Lwin, Lwout)

b5 <- bandT_rad %>%
  select(Datetime, B5, Swin, Swout, Lwin, Lwout)

b6 <- bandT_rad %>%
  select(Datetime, B6, Swin, Swout, Lwin, Lwout)

b7 <- bandT_rad %>%
  select(Datetime, B7, Swin, Swout, Lwin, Lwout)

b8 <- bandT_rad %>%
  select(Datetime, B8, Swin, Swout, Lwin, Lwout)

b9 <- bandT_rad %>%
  select(Datetime, B9, Swin, Swout, Lwin, Lwout)

b10 <- bandT_rad %>%
  select(Datetime, B10, Swin, Swout, Lwin, Lwout)

b11 <- bandT_rad %>%
  select(Datetime, B11, Swin, Swout, Lwin, Lwout)

b12 <- bandT_rad %>%
  select(Datetime, B12, Swin, Swout, Lwin, Lwout)

b13 <- bandT_rad %>%
  select(Datetime, B13, Swin, Swout, Lwin, Lwout)

b14 <- bandT_rad %>%
  select(Datetime, B14, Swin, Swout, Lwin, Lwout)

#### PROCESSING DONE ####
##########################################################################

## DON'T NEED TO RUN ANY OF THE ABOVE LINES AS LONG AS RDATA FILE IS READ IN
load("C:/Users/sears/Documents/Repos/CSU-MS-Research/Melt2021/DFs.Rdata")

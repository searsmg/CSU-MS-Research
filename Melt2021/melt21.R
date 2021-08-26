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

RH_dewpt <- read.csv(file="RH_dewpt_all.csv", header=TRUE) %>%
  mutate(Datetime = mdy_hm(Datetime))

all21 <- merge(temp_elev_21, RH_dewpt, by=c("Datetime", "ID"))

#get a temp for each elev band (grouping bands and getting avg temp)
bands21 <- all21 %>%
  group_by(Datetime, Band) %>%
  summarize(bandT = mean(AirT_C, na.rm=TRUE),
            bandRH = mean(humidity),
            banddewpt = mean(dewpoint))

bandT_long <- bands21 %>%
  pivot_wider(names_from = Band, values_from = bandT)

write.csv(bandT_long, "bandT.csv")

bandRH_long <- bands21 %>%
  select(Datetime, bandRH,Band) %>%
  pivot_wider(names_from = Band, values_from=bandRH)

write.csv(bandRH_long, "bandRH.csv")

###################################################
#bring band T back in - have all Ta for melt 21
bandT <- read.csv(file="bands21_9.csv", header=TRUE) %>%
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
b1_2 <- bandT_rad %>%
  select(Datetime, b1_2, Swin, Swout, Lwin, Lwout)

b3 <- bandT_rad %>%
  select(Datetime, b3, Swin, Swout, Lwin, Lwout)

b4 <- bandT_rad %>%
  select(Datetime, b4, Swin, Swout, Lwin, Lwout)

b5 <- bandT_rad %>%
  select(Datetime, b5, Swin, Swout, Lwin, Lwout)

b6 <- bandT_rad %>%
  select(Datetime, b6, Swin, Swout, Lwin, Lwout)

b7 <- bandT_rad %>%
  select(Datetime, b7, Swin, Swout, Lwin, Lwout)

b8 <- bandT_rad %>%
  select(Datetime, b8, Swin, Swout, Lwin, Lwout)

b9 <- bandT_rad %>%
  select(Datetime, b9, Swin, Swout, Lwin, Lwout)

b10_14 <- bandT_rad %>%
  select(Datetime, b10_14, Swin, Swout, Lwin, Lwout)


#### PROCESSING DONE ####
##########################################################################

## DON'T NEED TO RUN ANY OF THE ABOVE LINES AS LONG AS RDATA FILE IS READ IN
load("C:/Users/sears/Documents/Repos/CSU-MS-Research/Melt2021/DFs.Rdata")

## for each band compute LWin
# first find sat vapor press (ea), then back out Cc. fix Cc for >1 or <1,
#
b1_2 <- b1_2 %>%
  mutate(VP = 6.112*exp((17.62*b1_2)/(243.12+b1_2))) %>%
  mutate(Cc = (((Lwin/((5.67*10^-8)*b1_2^4))/(0.53+0.065*VP))-1)/0.4) %>%
  mutate(Cc_fix = if_else(Cc<0,0,if_else(Cc>1,1,Cc))) %>%
  mutate(Lwin_fix = (0.53+(0.065*VP))*(1+(0.4*Cc_fix))*(5.67*10^-8)*(b1_2^4))



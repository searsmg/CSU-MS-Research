### Script to calculate Melt 21 data ###

library(ggplot2)
library(lubridate)
library(dplyr)
library(plotly)
library(tidyverse)
library(RNRCS)

rm(list = ls()) 

#set working directory and csv file
setwd("C:/Users/sears/Documents/Research/Snow_Hydro_Research/Thesis/Data/Air Temp/For R/2021")

##########################################################################
#### PROCESSING ####

#temp, elevation, and elevation band data
temp_elev_21 <- read.csv(file="Melt21_elev_noOUT.csv", header=TRUE) %>%
  mutate(Datetime = mdy_hm(Datetime))

RH_dewpt <- read.csv(file="RH_dewpt_all.csv", header=TRUE) %>%
  mutate(Datetime = mdy_hm(Datetime))

all21 <- merge(temp_elev_21, RH_dewpt, by=c("Datetime", "ID"))

#get a temp for each elev band (grouping bands and getting avg temp)
#bands21 <- all21 %>%
#  group_by(Datetime, Band) %>%
#  summarize(bandT = mean(AirT_C, na.rm=TRUE),
#            bandRH = mean(humidity),
#            banddewpt = mean(dewpoint))

#bandT_long <- bands21 %>%
#  pivot_wider(names_from = Band, values_from = bandT)

#write.csv(bandT_long, "bandT.csv")

#bandRH_long <- bands21 %>%
#  select(Datetime, bandRH,Band) %>%
#  pivot_wider(names_from = Band, values_from=bandRH)

#write.csv(bandRH_long, "bandRH.csv")

###################################################
#bring band T back in - have all Ta for melt 21
bandT <- read.csv(file="bands21_9.csv", header=TRUE) %>%
  mutate(Datetime = mdy_hm(Datetime))

#bring RH back in
bandRH <- read.csv(file="bandRH.csv", header=TRUE) %>%
  mutate(Datetime = mdy_hm(Datetime))

#get rid of the minute on date time (e.g., 14:01 - want 14:00)
minute(bandT$Datetime) <- 0
minute(bandRH$Datetime) <- 0

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

#add RH to bandT+rad
band_all <- merge(bandT_rad, bandRH, by="Datetime")

#now break each band out so we can calculate rad and melt
b1_2 <- band_all %>%
  select(Datetime, b1_2, Swin, Swout, Lwin, Lwout, b1_2_RH)

b3 <- band_all %>%
  select(Datetime, b3, Swin, Swout, Lwin, Lwout, b3_RH)

b4 <- band_all %>%
  select(Datetime, b4, Swin, Swout, Lwin, Lwout, b4_RH)

b5 <- band_all %>%
  select(Datetime, b5, Swin, Swout, Lwin, Lwout, b5_RH)

b6 <- band_all %>%
  select(Datetime, b6, Swin, Swout, Lwin, Lwout, b6_RH)

b7 <- band_all %>%
  select(Datetime, b7, Swin, Swout, Lwin, Lwout, b7_RH)

b8 <- band_all %>%
  select(Datetime, b8, Swin, Swout, Lwin, Lwout, b8_RH)

b9 <- band_all %>%
  select(Datetime, b9, Swin, Swout, Lwin, Lwout, b9_RH)

b10_14 <- band_all %>%
  select(Datetime, b10_14, Swin, Swout, Lwin, Lwout, b10_RH)


#### PROCESSING DONE ####
##########################################################################

## DON'T NEED TO RUN ANY OF THE ABOVE LINES AS LONG AS RDATA FILE IS READ IN
#load("C:/Users/sears/Documents/Repos/CSU-MS-Research/Melt2021/DFs.Rdata")

## for each band compute LWin
# first find sat vapor press (ea), then back out Cc. fix Cc for >1 or <1, then calc LW in for all bands

#stefan boltz constant
stef <- 5.67 * 10^-8

b1_2 <- b1_2 %>%
  mutate(esat = (6.112*exp((17.62*b1_2)/(243.12+b1_2)))) %>%
  mutate(ea = (b1_2_RH * esat)/100) %>%
  mutate(Cc_pt1 = Lwin/((stef)*(b1_2+273.15)^4)) %>%
  mutate(Cc_pt2 = Cc_pt1 /(0.53+(0.065*ea))) %>%
  mutate(Cc = (Cc_pt2 -1)/0.4) %>%
  mutate(Cc_fix = if_else(Cc<0,0,if_else(Cc>1,1,Cc))) %>%
  mutate(Lwin_fix = (0.53+(0.065*ea))*(1+(0.4*Cc_fix))*(5.67*10^-8)*((b1_2+273.15)^4)) %>%
  select(-c(Cc_pt1, Cc_pt2, Cc))

b3 <- b3 %>%
  mutate(esat = (6.112*exp((17.62*b3)/(243.12+b3)))) %>%
  mutate(ea = (b3_RH * esat)/100) %>%
  mutate(Cc_pt1 = Lwin/((stef)*(b3+273.15)^4)) %>%
  mutate(Cc_pt2 = Cc_pt1 /(0.53+(0.065*ea))) %>%
  mutate(Cc = (Cc_pt2 -1)/0.4) %>%
  mutate(Cc_fix = if_else(Cc<0,0,if_else(Cc>1,1,Cc))) %>%
  mutate(Lwin_fix = (0.53+(0.065*ea))*(1+(0.4*Cc_fix))*(5.67*10^-8)*((b3+273.15)^4)) %>%
  select(-c(Cc_pt1, Cc_pt2, Cc))

b4 <- b4 %>%
  mutate(esat = (6.112*exp((17.62*b4)/(243.12+b4)))) %>%
  mutate(ea = (b4_RH * esat)/100) %>%
  mutate(Cc_pt1 = Lwin/((stef)*(b4+273.15)^4)) %>%
  mutate(Cc_pt2 = Cc_pt1 /(0.53+(0.065*ea))) %>%
  mutate(Cc = (Cc_pt2 -1)/0.4) %>%
  mutate(Cc_fix = if_else(Cc<0,0,if_else(Cc>1,1,Cc))) %>%
  mutate(Lwin_fix = (0.53+(0.065*ea))*(1+(0.4*Cc_fix))*(5.67*10^-8)*((b4+273.15)^4)) %>%
  select(-c(Cc_pt1, Cc_pt2, Cc))

b5 <- b5 %>%
  mutate(esat = (6.112*exp((17.62*b5)/(243.12+b5)))) %>%
  mutate(ea = (b5_RH * esat)/100) %>%
  mutate(Cc_pt1 = Lwin/((stef)*(b5+273.15)^4)) %>%
  mutate(Cc_pt2 = Cc_pt1 /(0.53+(0.065*ea))) %>%
  mutate(Cc = (Cc_pt2 -1)/0.4) %>%
  mutate(Cc_fix = if_else(Cc<0,0,if_else(Cc>1,1,Cc))) %>%
  mutate(Lwin_fix = (0.53+(0.065*ea))*(1+(0.4*Cc_fix))*(5.67*10^-8)*((b5+273.15)^4)) %>%
  select(-c(Cc_pt1, Cc_pt2, Cc))

b6 <- b6 %>%
  mutate(esat = (6.112*exp((17.62*b6)/(243.12+b6)))) %>%
  mutate(ea = (b6_RH * esat)/100) %>%
  mutate(Cc_pt1 = Lwin/((stef)*(b6+273.15)^4)) %>%
  mutate(Cc_pt2 = Cc_pt1 /(0.53+(0.065*ea))) %>%
  mutate(Cc = (Cc_pt2 -1)/0.4) %>%
  mutate(Cc_fix = if_else(Cc<0,0,if_else(Cc>1,1,Cc))) %>%
  mutate(Lwin_fix = (0.53+(0.065*ea))*(1+(0.4*Cc_fix))*(5.67*10^-8)*((b6+273.15)^4)) %>%
  select(-c(Cc_pt1, Cc_pt2, Cc))

b7 <- b7 %>%
  mutate(esat = (6.112*exp((17.62*b7)/(243.12+b7)))) %>%
  mutate(ea = (b7_RH * esat)/100) %>%
  mutate(Cc_pt1 = Lwin/((stef)*(b7+273.15)^4)) %>%
  mutate(Cc_pt2 = Cc_pt1 /(0.53+(0.065*ea))) %>%
  mutate(Cc = (Cc_pt2 -1)/0.4) %>%
  mutate(Cc_fix = if_else(Cc<0,0,if_else(Cc>1,1,Cc))) %>%
  mutate(Lwin_fix = (0.53+(0.065*ea))*(1+(0.4*Cc_fix))*(5.67*10^-8)*((b7+273.15)^4)) %>%
  select(-c(Cc_pt1, Cc_pt2, Cc))

b8 <- b8 %>%
  mutate(esat = (6.112*exp((17.62*b8)/(243.12+b8)))) %>%
  mutate(ea = (b8_RH * esat)/100) %>%
  mutate(Cc_pt1 = Lwin/((stef)*(b8+273.15)^4)) %>%
  mutate(Cc_pt2 = Cc_pt1 /(0.53+(0.065*ea))) %>%
  mutate(Cc = (Cc_pt2 -1)/0.4) %>%
  mutate(Cc_fix = if_else(Cc<0,0,if_else(Cc>1,1,Cc))) %>%
  mutate(Lwin_fix = (0.53+(0.065*ea))*(1+(0.4*Cc_fix))*(5.67*10^-8)*((b8+273.15)^4)) %>%
  select(-c(Cc_pt1, Cc_pt2, Cc))

b9 <- b9 %>%
  mutate(esat = (6.112*exp((17.62*b9)/(243.12+b9)))) %>%
  mutate(ea = (b9_RH * esat)/100) %>%
  mutate(Cc_pt1 = Lwin/((stef)*(b9+273.15)^4)) %>%
  mutate(Cc_pt2 = Cc_pt1 /(0.53+(0.065*ea))) %>%
  mutate(Cc = (Cc_pt2 -1)/0.4) %>%
  mutate(Cc_fix = if_else(Cc<0,0,if_else(Cc>1,1,Cc))) %>%
  mutate(Lwin_fix = (0.53+(0.065*ea))*(1+(0.4*Cc_fix))*(5.67*10^-8)*((b9+273.15)^4)) %>%
  select(-c(Cc_pt1, Cc_pt2, Cc))

b10_14 <- b10_14 %>%
  mutate(esat = (6.112*exp((17.62*b10_14)/(243.12+b10_14)))) %>%
  mutate(ea = (b10_RH * esat)/100) %>%
  mutate(Cc_pt1 = Lwin/((stef)*(b10_14+273.15)^4)) %>%
  mutate(Cc_pt2 = Cc_pt1 /(0.53+(0.065*ea))) %>%
  mutate(Cc = (Cc_pt2 -1)/0.4) %>%
  mutate(Cc_fix = if_else(Cc<0,0,if_else(Cc>1,1,Cc))) %>%
  mutate(Lwin_fix = (0.53+(0.065*ea))*(1+(0.4*Cc_fix))*(5.67*10^-8)*((b10_14+273.15)^4)) %>%
  select(-c(Cc_pt1, Cc_pt2, Cc))


###########################################################################################
#running band 4 with observed b4 temp and ELR temp from SNOTEL

#pull in hourly SNOTEL data for temp
JW21_hr <- grabNRCS.data(network = "SNTL", site_id = 551, timescale = "hourly", DayBgn = '2021-04-01', DayEnd = '2021-07-01') %>%
  mutate(Datetime = ymd_hm(Date))


JW21_hr <- JW21_hr %>%
  mutate(Ta_jw = (Air.Temperature.Observed..degF.-32)*(5/9)) %>%
  select(c(Datetime, Ta_jw))


b4small <- merge(b4, JW21_hr, by = "Datetime")

mf <- 0.2
bt <- -2

melt <- b4small %>%
  mutate(melt_Tobs = if_else(mf*(b4-bt)<0,0,mf*(b4-bt))) %>%
  mutate(Tlap = Ta_jw+(-0.0065*(3175-3089.86))) %>%
  mutate(melt_Tlap = if_else(mf*(Tlap-bt)<0,0,mf*(Tlap-bt))) %>%
  mutate(melt_sum_Tlap = cumsum(melt_Tlap),
         melt_sum_Tobs = cumsum(melt_Tobs))

ggplot(melt)+geom_line(aes(x=Datetime, y=rev(melt_sum_Tlap), color="Tlapse")) +
  geom_line(aes(x=Datetime, y=rev(melt_sum_Tobs), color="Tobs")) + geom_point(data=swe17, aes(x=Datetime, y=SWE))+
  labs(y="melt (mm)")

swe17 <- read.csv("swe17.csv") %>%
  mutate(Datetime=mdy_hm(Datetime))
            
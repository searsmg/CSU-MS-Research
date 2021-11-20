#Script to model net shortwave using albedo [first order decay model]
#also modeling LWin using effective atmospheric emissivity 
#data from CAIC for 2020 and applied onto Joe Wright watershed

library(ggplot2)
library(lubridate)
library(dplyr)
library(plotly)
library(tidyverse)
library(RNRCS)
library(esquisse)
library(RColorBrewer)
library(gridExtra)

rm(list = ls())

#set working directory and csv file
setwd("C:/Users/sears/Documents/Research/Snow_Hydro_Research/Thesis/Data/Radiation/For R")

#hourly rad data
rad20 <- read.csv(file="rad20.csv", 
                  header=TRUE) %>%
  mutate(Datetime = mdy_hm(Datetime)) %>%
  filter(Datetime > ymd_hms("2020-05-01 00:00:00")) %>%
  filter(Datetime < ymd_hms("2020-07-01 00:00:00"))

caic_t <- read.csv(file="t20.csv") %>%
  mutate(Datetime = mdy_hm(Datetime)) %>%
  select(c(Datetime, Ta_F, RH...)) %>%
  mutate(Ta_C = (Ta_F-32)*(5/9)) %>%
  select(c(-Ta_F))

JW20daily <- read.csv("C:/Users/sears/Documents/Research/Snow_Hydro_Research/Thesis/Data/SNOTEL/JW20daily.csv") %>%
  mutate(Date = ymd(Date))

############################################################################
#check rad data first

#get hourly rad first
rad_hr <- rad20 %>%
  mutate(dt_agg = floor_date(Datetime, unit = "hour")) %>%
  group_by(dt_agg) %>%
  summarize(avgSWin = mean(Swin),
            avgLWin = mean(Lwin),
            avgLWout = mean(Lwout),
            avgSWout=mean(Swout)) %>%
  rename(Datetime = dt_agg) %>%
  mutate(Datetime = ymd_hms(Datetime))

#plotting the rad data - missing so much
ggplot(rad_hr) +
  geom_line(aes(x=Datetime, y=avgSWin))

ggplot(rad_hr) +
  geom_line(aes(x=Datetime, y=avgSWout))

ggplot(rad_hr) +
  geom_line(aes(x=Datetime, y=avgLWin))

ggplot(rad_hr) +
  geom_line(aes(x=Datetime, y=avgLWout))

###############################################################################
#estimate cloud cover from the data that is there
stef <- 5.67 * 10^-8 #stef boltz constant

rad_hr20 <- merge(caic_t, rad_hr, by="Datetime")

# model CC in the JWrad_hr. Apply CC to all models plus calibration using SNOTEL
rad_hr20 <- rad_hr20 %>%
  mutate(avgLWin = avgLWin/10) %>% 
  mutate(esat = (6.112*exp((17.62*Ta_C)/(243.12+Ta_C)))) %>%
  mutate(ea = (RH... * esat)/100) %>%
  mutate(Cc_pt1 = avgLWin/((stef)*(Ta_C+273.15)^4)) %>%
  mutate(Cc_pt2 = Cc_pt1 /(0.53+(0.065*ea))) %>%
  mutate(Cc = (Cc_pt2 -1)/0.4) %>%
  mutate(Cc_fix = if_else(Cc<0,0,if_else(Cc>1,1,Cc))) %>%
  select(-c(esat, ea, Cc_pt1, Cc_pt2, Cc))

mean(rad_hr20$Cc_fix)
avgCC <- 0.0061
###############################################################################
#getting albedo. min equals 0.5 since we are only modeling for when there is snow
JW20_al <- JW20daily %>%
  mutate(Ta_C = (Ta_F-32)*(5/9),
         PrecipAcum_mm = precip_accum_in*25.4,
         Sd_mm = Sd_in*25.4,
         SWE_mm = SWE_in*25.4) %>%
  select(-c(Ta_F, precip_accum_in, Sd_in, SWE_in)) %>%
  mutate(precip_mm = PrecipAcum_mm - lag(PrecipAcum_mm)) %>%
  mutate(precip_mm = if_else(precip_mm<2.54,0,precip_mm)) %>%
  mutate(swe_mm = SWE_mm - lag(SWE_mm),
         sdepth = Sd_mm - lag(Sd_mm)) %>%
  mutate(precip_mm = if_else(precip_mm<2.54,0,precip_mm)) %>%
  mutate(swe_mm = SWE_mm - lag(SWE_mm),
         sdepth = Sd_mm - lag(Sd_mm)) %>%
  drop_na() %>%
  mutate(freshsnow = if_else(sdepth > 10 & swe_mm > 2.54, precip_mm, 0)) %>%
  mutate(al_min = if_else(Ta_C <= 0, 0.7, 0.5)) %>%
  mutate(actual_al = ifelse(freshsnow > 0, 0.84, NA))

JW20_al[1,"actual_al"] <- 0.5

for(i in 2:nrow(JW20_al)){
  if(is.na(JW20_al$actual_al[i])){
    JW20_al$actual_al[i] = ((JW20_al$actual_al[i-1]-JW20_al$al_min[i])*exp(-0.01*24)) + JW20_al$al_min[i]
  }
}

ggplot(JW20_al)+geom_line(aes(Date, actual_al))
#albedo looks good

#get albedo daily to hrly, NEED hourly dataset first
al_hr <- JW20_al %>%
  select(Date, actual_al) %>%
  mutate(doy = yday(Date))

################################################################################
#need to bring in mp4 data to add albedo too and other rad
mp4hr <- read.csv(file="C:/Users/sears/Documents/Research/Snow_Hydro_Research/Thesis/Data/Dewpt Temp/For R/melt20.csv", 
                         header=TRUE) %>%
  mutate(Datetime = ymd_hms(Datetime))

#bring in JW hrly for temp
jwhr <- read.csv("C:/Users/sears/Documents/Research/Snow_Hydro_Research/Thesis/Data/SNOTEL/JW20hr.csv") %>%
  mutate(Datetime = ymd_hm(Datetime))

#get it into deg C from hourly SNOTEL
jwtemp <- jwhr %>%
  mutate(Tjw = (Ta_F-32)*(5/9))

#next several lines are pulling mp4 then merging  back w/ rad data
mp4 <- mp4hr %>%
  filter(ID == "MP4") %>%
  filter(Datetime > ymd_hms("2020-05-01 00:00:00")) %>%
  filter(Datetime < ymd_hms("2020-07-01 00:00:00"))
  
#get rid of :01 on mp4 time
minute(mp4$Datetime) <- 0

mp4 <- mp4 %>%
  select(-c(X, Band))

#need HD1 RH and Ta for varying saturated vap press
mp2 <- mp4hr %>%
  filter(ID == "MP2") %>% 
  filter(Datetime > ymd_hms("2020-05-01 00:00:00")) %>%
  filter(Datetime < ymd_hms("2020-07-01 00:00:00"))

#get rid of :01 on mp2 time
minute(mp2$Datetime) <- 0

#rename columns so when merged with mp4
mp2 <- mp2 %>%
  select(-c(X, ID, Elevation, Band)) %>%
  rename(mp2rh = humidity,
         mp2ta = AirT_C,
         mp2td = dewpoint)

#add mp2 data to mp4
mp4 <- merge(mp4, mp2, "Datetime")

jwtemp <- jwtemp %>%
  select(-c(Site.Id, Date, Time, Ta_F))

#merge mp4 (plus HD1) with jw rad data by hr
mp4 <- merge(mp4, jwtemp, "Datetime") 

#now get albedo to mp4
mp4 <- mp4 %>%
  mutate(doy = yday(Datetime))

mp4 <- merge(al_hr, mp4, by="doy") 

###############################################################################
#next model incoming SW and use SWin and albedo to get SWnet
# get "J" which is day of year, then determine day angle in radians.
mp4 <- mp4 %>%
  mutate(DayAngle = (2*pi*(doy-1))/365) %>%
  mutate(Eo = 1.000110 + (0.34221 * cos(DayAngle)) + 
           (0.001280 * sin(DayAngle)) + (0.000719 * cos (2*DayAngle))
         + (0.000077 * sin(2*DayAngle)),
         Dec = 0.006918 - (0.39912 * cos(DayAngle)) + 
           (0.070257 * sin(DayAngle)) - (0.006758 * cos(2*DayAngle)) + 
           (0.000907 * sin(2*DayAngle)) - (0.002697 * cos(3*DayAngle)) +
           (0.00148 * sin(3*DayAngle))) %>%
  mutate(Dec_deg = (Dec*180)/pi,
         Et = 0.000292 + (0.007264 *  cos(DayAngle)) - 
           (0.12474*sin(DayAngle)) - (0.05684 * cos(2*DayAngle)) - 
           (0.15886 * sin(2*DayAngle)))

# longitude correction= (-114 - 8)/15 then convert to radians to get -0.1419. -114 = longitude of Kalispell MT
mp4$hour <- as.numeric(hour(mp4$Datetime))

mp4 <- mp4 %>%
  mutate(Tsn = (hour + (-0.1419) + Et) - 12)

# now actually calculate SWin
mp4 <- mp4 %>%
  mutate(avgSWin = 1367*Eo*(cos(Dec)*cos(-1.98968)*cos(0.2618*Tsn)
                            +sin(Dec)*sin(-1.98968))*(0.355+(0.68*(1-avgCC)))) %>%
  mutate(avgSW = avgSWin*(1-actual_al))

###LONGWAVE IN AND OUT
#assign the Ess (emissivity of snow surface)
Ess <- 1

#convert temp to K, determine Tss, and Hl_out (longwave rad out)
mp4 <- mp4 %>%
  mutate(Ta_K = AirT_C + 273.15) %>%
  mutate(Tss = if_else(AirT_C < 0, Ta_K, 273.15)) %>%
  mutate(avgLWout = stef * Ess * (Tss^4))

#find Hl_in using AirTemp_K, SB constant, and emissivity of atmos. 
mp4 <- mp4 %>%
  mutate(Eatmos = (0.53+(0.065*ea))*(1+(0.4*avgCC))) %>%
  mutate(avgLWin = Eatmos*stef*(Ta_K^4))

#Hl_total
mp4<- mp4 %>%
  mutate(Hl_total = Hl_in - Hl_out)
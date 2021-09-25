#Script to model net shortwave using albedo [first order decay model]
#also modeling LWin using effective atmospheric emissivity 
#data from CAIC for 2021 and applied oto Joe Wright watershed

library(ggplot2)
library(lubridate)
library(dplyr)
library(plotly)
library(tidyverse)
library(RNRCS)
library(esquisse)

#set working directory and csv file
setwd("C:/Users/sears/Documents/Research/Snow_Hydro_Research/Thesis/Data/Radiation/For R")

#hourly rad data
rad21 <- read.csv(file="Rad_melt21.csv", 
                  header=TRUE) %>%
  mutate(Datetime = mdy_hm(Datetime))

#pull in hourly SNOTEL data for precip
JW21daily <- grabNRCS.data(network = "SNTL", site_id = 551, timescale = "daily", DayBgn = '2021-04-01', DayEnd = '2021-07-01') %>%
  mutate(Date = ymd(Date))

############################################################################

JW21_al <- JW21daily %>%
  rename(Ta_F = Air.Temperature.Average..degF., 
         precip_accum_in = Precipitation.Accumulation..in..Start.of.Day.Values,
         Sd_in = Snow.Depth..in..Start.of.Day.Values,
         SWE_in = Snow.Water.Equivalent..in..Start.of.Day.Values) %>%
  select(-c(Air.Temperature.Maximum..degF., Air.Temperature.Minimum..degF.,
            Air.Temperature.Observed..degF..Start.of.Day.Values)) %>%
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

JW21_al[1,"actual_al"] <- 0.5

for(i in 2:nrow(JW21_al)){
  if(is.na(JW21_al$actual_al[i])){
    JW21_al$actual_al[i] = ((JW21_al$actual_al[i-1]-JW21_al$al_min[i])*exp(-0.01)) + JW21_al$al_min[i]
  }
}

JW21_al <- JW21_al %>%
  mutate(actual_al = ifelse(Date > "2021-06-11", 0.2, actual_al))

ggplot(JW21_al)+geom_line(aes(Date, actual_al))

################################################################
#hourly rad data to daily rad
rad_daily <- rad21 %>%
  group_by(Date = format(Datetime, "%Y-%m-%d")) %>%
  summarize(avgSWin = mean(Swin),
            avgLWin = mean(Lwin),
            avgLWout = mean(Lwout)) %>%
  mutate(Date = ymd(Date))

JWrad_daily <- merge(JW21_al, rad_daily, by="Date")

JWrad_daily <- JWrad_daily %>%
  mutate(SWnet = avgSWin*(1-actual_al),
         LWnet = avgLWin-avgLWout) %>%
  mutate(nr = SWnet+LWnet) %>%
  select(c(Date, SWnet, avgLWin, avgLWout, LWnet, nr, Ta_C))

ggplot(JWrad_daily)+geom_line(aes(Date, SWnet)) +
  geom_line(aes(Date, LWnet), color="purple") +
  geom_line(aes(Date, nr), color="red")


####################################################################
#model LWin for MP4 so bring in MP4 data
temp_elev_21 <- read.csv(file="C:/Users/sears/Documents/Research/Snow_Hydro_Research/Thesis/Data/Air Temp/For R/2021/Melt21_elev_noOUT.csv", 
                         header=TRUE) %>%
  mutate(Datetime = mdy_hm(Datetime))

RH_dewpt <- read.csv(file="C:/Users/sears/Documents/Research/Snow_Hydro_Research/Thesis/Data/Air Temp/For R/2021/RH_dewpt_all.csv", 
                     header=TRUE) %>%
  mutate(Datetime = mdy_hm(Datetime))

mp4 <- temp_elev_21 %>%
  filter(ID == "MP4")

mp4_rh <- RH_dewpt %>%
  filter(ID == "MP4")

mp4 <- merge(mp4_rh, mp4, by="Datetime")

#get rid of :01 on mp4 time
minute(mp4$Datetime) <- 0

stef <- 5.67 * 10^-8

mp4 <- mp4 %>%
  select(-c(ID.x, ID.y, Elevation, Band)) %>%
  group_by(Date = format(Datetime, "%Y-%m-%d")) %>%
  summarize(temp = mean(AirT_C, na.rm=T),
         rh = mean(humidity),
         dewpt = mean(dewpoint)) %>%
  mutate(Date = ymd(Date))

mp4 <- merge(mp4, JWrad_daily, "Date")

#model NR for MP4 using obs T
mp4obs <- mp4 %>%
  mutate(esat = (6.112*exp((17.62*temp)/(243.12+temp)))) %>%
  mutate(ea = (rh * esat)/100) %>%
  mutate(Cc_pt1 = avgLWin/((stef)*(temp+273.15)^4)) %>%
  mutate(Cc_pt2 = Cc_pt1 /(0.53+(0.065*ea))) %>%
  mutate(Cc = (Cc_pt2 -1)/0.4) %>%
  mutate(Cc_fix = if_else(Cc<0,0,if_else(Cc>1,1,Cc))) %>%
  mutate(Lwin_fix = (0.53+(0.065*ea))*(1+(0.4*Cc_fix))*(5.67*10^-8)*((temp+273.15)^4)) %>%
  select(-c(Cc_pt1, Cc_pt2, Cc)) %>%
  mutate(nrfix = SWnet + (Lwin_fix-avgLWout))

#model NR for MP4 using lapsed T from SNOTEL
mp4elr <- mp4 %>%
  mutate(Tlap = Ta_C+(-0.0065*(3197.48-3089.86))) %>%
  mutate(esat = (6.112*exp((17.62*Tlap)/(243.12+Tlap)))) %>%
  mutate(ea = (rh * esat)/100) %>%
  mutate(Cc_pt1 = avgLWin/((stef)*(Tlap+273.15)^4)) %>%
  mutate(Cc_pt2 = Cc_pt1 /(0.53+(0.065*ea))) %>%
  mutate(Cc = (Cc_pt2 -1)/0.4) %>%
  mutate(Cc_fix = if_else(Cc<0,0,if_else(Cc>1,1,Cc))) %>%
  mutate(Lwin_fix = (0.53+(0.065*ea))*(1+(0.4*Cc_fix))*(5.67*10^-8)*((Tlap+273.15)^4)) %>%
  select(-c(Cc_pt1, Cc_pt2, Cc)) %>%
  mutate(nrfix = SWnet + (Lwin_fix-avgLWout))

ggplot() + geom_line(data=mp4obs, aes(Date, nrfix)) +
  geom_line(data=mp4elr, aes(Date, nrfix), color="purple")

write.csv(mp4obs, "mp4obs.csv")
write.csv(mp4elr, "mp4elr.csv")

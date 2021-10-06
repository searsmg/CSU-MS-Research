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
    JW21_al$actual_al[i] = ((JW21_al$actual_al[i-1]-JW21_al$al_min[i])*exp(-0.01*24)) + JW21_al$al_min[i]
  }
}

ggplot(JW21_al)+geom_line(aes(Date, actual_al))

################################################################
#hourly rad data 
rad_hr <- rad21 %>%
  mutate(dt_agg = floor_date(Datetime, unit = "hour")) %>%
  group_by(dt_agg) %>%
  summarize(avgSWin = mean(Swin),
            avgLWin = mean(Lwin),
            avgLWout = mean(Lwout)) #%>%
#  mutate(Datetime = ymd_hm(Datetime))

#get actual_al from day to hourly - assume albedo is constant through day
al_hr <- JW21_al %>%
  select(Date, actual_al) %>%
  mutate(doy = yday(Date))

rad_hr <- rad_hr %>%
  mutate(doy = yday(dt_agg))

JWrad_hr <- merge(al_hr, rad_hr, by="doy") 

##############################################################
#now calculate hourly rad data

JWrad_hr <- JWrad_hr %>%
  mutate(avgLWout = if_else(avgLWout > 315.64, 315.64, avgLWout)) %>%
  mutate(SWnet = avgSWin*(1-actual_al),
         LWnet = avgLWin-avgLWout) %>%
  mutate(nr = SWnet+LWnet) %>%
  select(c(dt_agg, avgSWin, SWnet, avgLWin, avgLWout, LWnet, nr)) %>%
  rename(Datetime = dt_agg)

ggplot(JWrad_hr)+geom_line(aes(Datetime,nr)) +
  geom_line(aes(Datetime, avgSWin), color="purple") +
  geom_line(aes(Datetime, nr), color="red")

####################################################################
#model LWin for MP4 so bring in MP4 data
temp_elev_21 <- read.csv(file="C:/Users/sears/Documents/Research/Snow_Hydro_Research/Thesis/Data/Air Temp/For R/2021/Melt21_elev_noOUT.csv", 
                         header=TRUE) %>%
  mutate(Datetime = mdy_hm(Datetime))

RH_dewpt <- read.csv(file="C:/Users/sears/Documents/Research/Snow_Hydro_Research/Thesis/Data/Air Temp/For R/2021/RH_dewpt_all.csv", 
                     header=TRUE) %>%
  mutate(Datetime = mdy_hm(Datetime))

#need to bring in hourly SNOTEL temp for ELR
JW21_temphr <- grabNRCS.data(network = "SNTL", site_id = 551, timescale = "hourly", DayBgn = '2021-04-01', DayEnd = '2021-07-01') %>%
  mutate(Datetime = ymd_hm(Date))

#get it into deg C from hourly SNOTEL
jwtemp <- JW21_temphr %>%
  rename(Tjw = Air.Temperature.Observed..degF.) %>%
  mutate(Tjw = (Tjw-32)*(5/9)) %>%
  select(c(Datetime, Tjw))

#add jwtemp to the JWrad_hr data
JWrad_hr <- merge(jwtemp, JWrad_hr, by="Datetime")

#next several lines are pulling mp4 then merging  back w/ rad data
mp4 <- temp_elev_21 %>%
  filter(ID == "MP4")

mp4_rh <- RH_dewpt %>%
  filter(ID == "MP4")

mp4 <- merge(mp4_rh, mp4, by="Datetime")

#get rid of :01 on mp4 time
minute(mp4$Datetime) <- 0

stef <- 5.67 * 10^-8

mp4 <- mp4 %>%
  select(-c(ID.x, ID.y, Elevation, Band))

mp4 <- merge(mp4, JWrad_hr, "Datetime")

#model NR for MP4 using obs T
mp4obs <- mp4 %>%
  mutate(esat = (6.112*exp((17.62*AirT_C)/(243.12+AirT_C)))) %>%
  mutate(ea = (humidity * esat)/100) %>%
  mutate(Cc_pt1 = avgLWin/((stef)*(AirT_C+273.15)^4)) %>%
  mutate(Cc_pt2 = Cc_pt1 /(0.53+(0.065*ea))) %>%
  mutate(Cc = (Cc_pt2 -1)/0.4) %>%
  mutate(Cc_fix = if_else(Cc<0,0,if_else(Cc>1,1,Cc))) %>%
  mutate(Lwin_fix = (0.53+(0.065*ea))*(1+(0.4*Cc_fix))*(5.67*10^-8)*((AirT_C+273.15)^4)) %>%
  select(-c(Cc_pt1, Cc_pt2, Cc)) %>%
  mutate(nrfix = SWnet + (Lwin_fix-avgLWout)) %>%
  mutate(LWnet = Lwin_fix - avgLWout)

#model NR for MP4 using lapsed T from SNOTEL
mp4elr <- mp4 %>%
  mutate(Tlap = Tjw+(-0.0065*(3197.48-3089.86))) %>% #playing with ELR
  mutate(esat = (6.112*exp((17.62*Tlap)/(243.12+Tlap)))) %>%
  mutate(ea = (humidity * esat)/100) %>%
  mutate(Cc_pt1 = avgLWin/((stef)*(Tlap+273.15)^4)) %>%
  mutate(Cc_pt2 = Cc_pt1 /(0.53+(0.065*ea))) %>%
  mutate(Cc = (Cc_pt2 -1)/0.4) %>%
  mutate(Cc_fix = if_else(Cc<0,0,if_else(Cc>1,1,Cc))) %>%
  mutate(Lwin_fix = (0.53+(0.065*ea))*(1+(0.4*Cc_fix))*(5.67*10^-8)*((Tlap+273.15)^4)) %>%
  select(-c(Cc_pt1, Cc_pt2, Cc)) %>%
  mutate(nrfix = SWnet + (Lwin_fix-avgLWout)) %>%
  mutate(LWnet = Lwin_fix - avgLWout)

#make plots showing the diff between obs and elr rad data and TEMP
#first, get the diff between obs and elr
mp4obs <- mp4obs %>%
  mutate(nrdif = nrfix - mp4elr$nrfix,
         swdif = SWnet - mp4elr$SWnet,
         lwdif = LWnet - mp4elr$LWnet,
         tdif = AirT_C - mp4elr$Tlap) %>%
  mutate(tcum_ob = cumsum(AirT_C),
         tcum_lap = cumsum(mp4elr$Tlap),
         nrcum_ob = cumsum(nrfix),
         nrcum_lap = cumsum(mp4elr$nrfix),
         tcum_obpos = ifelse(AirT_C>0, AirT_C, 0),
         tcum_ob0 = cumsum(tcum_obpos),
         tcum_lappos = ifelse(mp4elr$Tlap>0, mp4elr$Tlap, 0),
         tcum_lap0 = cumsum(tcum_lappos))


ggplot(mp4obs) + geom_line(aes(Datetime, tcum_ob), size=1) +
  geom_line(aes(Datetime, tcum_lap), color="purple", size=1) +
  labs(y= "Cumulative T")

ggplot(mp4obs) + geom_line(aes(Datetime, nrcum_ob), size=1) +
  geom_line(aes(Datetime, nrcum_lap), color="purple", size=1) +
  labs(y= "Cumulative NR")

ggplot(mp4obs) + geom_line(aes(Datetime, tcum_ob0), size=1) +
  geom_line(aes(Datetime, tcum_lap0), color="purple", size=1) +
  labs(y= "Cumulative T > 0")


write.csv(mp4obs, "mp4obs.csv")
write.csv(mp4elr, "mp4elr.csv")
write.csv(JWrad_hr, "JWrad_hr.csv")



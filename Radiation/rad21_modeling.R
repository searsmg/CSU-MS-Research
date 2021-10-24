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
library(RColorBrewer)
library(gridExtra)

rm(list = ls())

#set working directory and csv file
setwd("C:/Users/sears/Documents/Research/Snow_Hydro_Research/Thesis/Data/Radiation/For R")

#hourly rad data
rad21 <- read.csv(file="Rad_melt21.csv", 
                  header=TRUE) %>%
  mutate(Datetime = mdy_hm(Datetime))

#pull in hourly SNOTEL data for precip
#JW21daily <- grabNRCS.data(network = "SNTL", site_id = 551, timescale = "daily", DayBgn = '2021-04-01', DayEnd = '2021-07-01') %>%
#  mutate(Date = ymd(Date))

JW21daily <- read.csv("C:/Users/sears/Documents/Research/Snow_Hydro_Research/Thesis/Data/SNOTEL/dailyjw.csv") %>%
  mutate(Date = mdy(Date))

############################################################################

#getting albedo. min equals 0.5 since we are only modeling for when there is snow
JW21_al <- JW21daily %>%
#  rename(Ta_F = Air.Temperature.Average..degF., 
#         precip_accum_in = Precipitation.Accumulation..in..Start.of.Day.Values,
#         Sd_in = Snow.Depth..in..Start.of.Day.Values,
#         SWE_in = Snow.Water.Equivalent..in..Start.of.Day.Values) %>%
#  select(-c(Air.Temperature.Maximum..degF., Air.Temperature.Minimum..degF.,
#            Air.Temperature.Observed..degF..Start.of.Day.Values)) %>%
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
            avgLWout = mean(Lwout),
            Ta = mean(Ta_C),
            RH = mean(RH)) #%>%
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
  mutate(SWnet = avgSWin*(1-actual_al)) %>%
  select(c(dt_agg, avgSWin, SWnet, avgLWin, avgLWout, Ta, RH)) %>%
  rename(Datetime = dt_agg)

stef <- 5.67 * 10^-8 #stef boltz constant

# model CC in the JWrad_hr. Apply CC to all models plus calibration using SNOTEL
JWrad_hr <- JWrad_hr %>%
  mutate(esat = (6.112*exp((17.62*Ta)/(243.12+Ta)))) %>%
  mutate(ea = (RH * esat)/100) %>%
  mutate(Cc_pt1 = avgLWin/((stef)*(Ta+273.15)^4)) %>%
  mutate(Cc_pt2 = Cc_pt1 /(0.53+(0.065*ea))) %>%
  mutate(Cc = (Cc_pt2 -1)/0.4) %>%
  mutate(Cc_fix = if_else(Cc<0,0,if_else(Cc>1,1,Cc))) %>%
  select(-c(esat, ea, Cc_pt1, Cc_pt2, Cc))

####################################################################
#model LWin for MP4 so bring in MP4 data
temp_elev_21 <- read.csv(file="C:/Users/sears/Documents/Research/Snow_Hydro_Research/Thesis/Data/Air Temp/For R/2021/Melt21_elev_noOUT.csv", 
                         header=TRUE) %>%
  mutate(Datetime = mdy_hm(Datetime))

RH_dewpt <- read.csv(file="C:/Users/sears/Documents/Research/Snow_Hydro_Research/Thesis/Data/Air Temp/For R/2021/RH_dewpt_all.csv", 
                     header=TRUE) %>%
  mutate(Datetime = mdy_hm(Datetime))

#need to bring in hourly SNOTEL temp for ELR
#JW21_temphr <- grabNRCS.data(network = "SNTL", site_id = 551, timescale = "hourly", DayBgn = '2021-04-01', DayEnd = '2021-07-01') %>%
#  mutate(Datetime = ymd_hm(Date))

#bring in JW hrly for temp
JW21_temphr <- read.csv("C:/Users/sears/Documents/Research/Snow_Hydro_Research/Thesis/Data/SNOTEL/jwhr.csv") %>%
  mutate(Datetime = mdy_hm(Datetime))

#get it into deg C from hourly SNOTEL
jwtemp <- JW21_temphr %>%
#  rename(Tjw = Air.Temperature.Observed..degF.) %>%
  select(c(Datetime, Tjw))

#add jwtemp to the JWrad_hr data
JWrad_hr <- merge(jwtemp, JWrad_hr, by="Datetime")

#write.csv(JWrad_hr, "JWrad_hr.csv") # use this in the MF script as well

#next several lines are pulling mp4 then merging  back w/ rad data
mp4 <- temp_elev_21 %>%
  filter(ID == "MP4")

mp4_rh <- RH_dewpt %>%
  filter(ID == "MP4")

mp4 <- merge(mp4_rh, mp4, by="Datetime")

#get rid of :01 on mp4 time
minute(mp4$Datetime) <- 0

mp4 <- mp4 %>%
  select(-c(ID.x, ID.y, Elevation, Band))

#need HD1 RH and Ta for varying saturated vap press
mp2 <- temp_elev_21 %>%
  filter(ID == "MP2")

mp2_rh <- RH_dewpt %>%
  filter(ID == "MP2")

mp2 <- merge(mp2_rh, mp2, by="Datetime")

#get rid of :01 on mp4 time
minute(mp2$Datetime) <- 0

#rename columns so when merged with mp4
mp2 <- mp2 %>%
  select(-c(ID.x, ID.y, Elevation, Band)) %>%
  rename(mp2rh = humidity,
         mp2ta = AirT_C,
         mp2td = dewpoint)

#add mp2 data to mp4
mp4 <- merge(mp4, mp2, "Datetime")

#merge mp4 (plus HD1) with jw rad data by hr
mp4 <- merge(mp4, JWrad_hr, "Datetime")  

#model NR for MP4 using obs T and obs ea
mp4a <- mp4 %>%
  mutate(ea = (6.112*exp((17.62*dewpoint)/(243.12+dewpoint)))) %>%
  mutate(Lwin_fix = (0.53+(0.065*ea))*(1+(0.4*Cc_fix))*(5.67*10^-8)*((AirT_C+273.15)^4)) %>%
  mutate(nrfix_a = SWnet + (Lwin_fix-avgLWout)) %>%
  mutate(LWnet = Lwin_fix - avgLWout) %>%
  mutate(tpos_a = ifelse(AirT_C > 4.58, AirT_C, 0),
         tcum_a = cumsum(tpos_a),
         nrcum_a = cumsum(nrfix_a)) %>%
  select(c(Datetime, tcum_a, nrcum_a, nrfix_a, tpos_a))

#model NR for MP4 using Ta lapse (ELR) and obs ea
mp4b1 <- mp4 %>%
  mutate(Tlap = Tjw+(-0.0065*(3197.48-3089.86))) %>% 
  mutate(ea = (6.112*exp((17.62*dewpoint)/(243.12+dewpoint)))) %>%
  mutate(Lwin_fix = (0.53+(0.065*ea))*(1+(0.4*Cc_fix))*(5.67*10^-8)*((Tlap+273.15)^4)) %>%
  mutate(nrfix_b1 = SWnet + (Lwin_fix-avgLWout)) %>%
  mutate(LWnet = Lwin_fix - avgLWout) %>%
  mutate(tpos_b1 = ifelse(Tlap > 4.58, Tlap, 0),
         tcum_b1 = cumsum(tpos_b1),
         nrcum_b1 = cumsum(nrfix_b1)) %>%
  select(c(Datetime, tcum_b1, nrcum_b1, nrfix_b1, tpos_b1))

#model NR for MP4 using Ta lapse (L&E) and obs ea
mp4b2 <- mp4 %>%
  mutate(Tlap = Tjw+(-0.00815*(3197.48-3089.86))) %>% 
  mutate(ea = (6.112*exp((17.62*dewpoint)/(243.12+dewpoint)))) %>%
  mutate(Lwin_fix = (0.53+(0.065*ea))*(1+(0.4*Cc_fix))*(5.67*10^-8)*((Tlap+273.15)^4)) %>%
  mutate(nrfix_b2 = SWnet + (Lwin_fix-avgLWout)) %>%
  mutate(LWnet = Lwin_fix - avgLWout) %>%
  mutate(tpos_b2 = ifelse(Tlap > 4.58, Tlap, 0),
         tcum_b2 = cumsum(tpos_b2),
         nrcum_b2 = cumsum(nrfix_b2)) %>%
  select(c(Datetime, tcum_b2, nrcum_b2, tpos_b2, nrfix_b2))

#model NR for MP4 using obs T and ea lapse (L&E)
mp4c <- mp4 %>%
  mutate(Tdlap = mp2td+(-0.0051*(3197.48-3092.27))) %>%
  mutate(ea = (6.112*exp((17.62*Tdlap)/(243.12+Tdlap)))) %>%
  mutate(Lwin_fix = (0.53+(0.065*ea))*(1+(0.4*Cc_fix))*(5.67*10^-8)*((AirT_C+273.15)^4)) %>%
  mutate(nrfix_c = SWnet + (Lwin_fix-avgLWout)) %>%
  mutate(LWnet = Lwin_fix - avgLWout) %>%
  mutate(tpos_c = ifelse(AirT_C > 4.58, AirT_C, 0),
         tcum_c = cumsum(tpos_c),
         nrcum_c = cumsum(nrfix_c)) %>%
  select(c(Datetime, tcum_c, nrcum_c, tpos_c, nrfix_c))

#model NR for MP4 using lapse T (elr or l&e) and obs ea
mp4d<- mp4 %>%
  mutate(Tlap = Tjw+(-0.00815*(3197.48-3089.86))) %>% 
  mutate(Tdlap = mp2td+(-0.0051*(3197.48-3092.27))) %>%
  mutate(ea = (6.112*exp((17.62*Tdlap)/(243.12+Tdlap)))) %>%
  mutate(Lwin_fix = (0.53+(0.065*ea))*(1+(0.4*Cc_fix))*(5.67*10^-8)*((Tlap+273.15)^4)) %>%
  mutate(nrfix_d = SWnet + (Lwin_fix-avgLWout)) %>%
  mutate(LWnet = Lwin_fix - avgLWout) %>%
  mutate(tpos_d = ifelse(Tlap > 4.58, Tlap, 0),
         tcum_d = cumsum(tpos_d),
         nrcum_d = cumsum(nrfix_d)) %>%
  select(c(Datetime, tcum_d, nrcum_d, tpos_d, nrfix_d))


#model E1 using obs T (TI model only)
mp4e1 <- mp4 %>% 
  mutate(tpos_e1 = ifelse(AirT_C > 0, AirT_C, 0),
       tcum_e1 = cumsum(tpos_e1)) %>%
  select(c(Datetime, tcum_e1, tpos_e1))

#model E2 using Ta lapsed using L&E value
mp4e2 <- mp4 %>% 
  mutate(Tlap = Tjw+(-0.00815*(3197.48-3089.86))) %>%
  mutate(tpos_e2 = ifelse(Tlap > 0, Tlap, 0),
         tcum_e2 = cumsum(tpos_e2)) %>%
  select(c(Datetime, tcum_e2,tpos_e2))


#write all the results for Ta, ea, and rad to CSVs
write.csv(mp4a, "mp4a.csv")
write.csv(mp4b1, "mp4b1.csv")
write.csv(mp4b2, "mp4b2.csv")
write.csv(mp4c, "mp4c.csv")
write.csv(mp4d, "mp4d.csv")
write.csv(mp4e1, "mp4e1.csv")
write.csv(mp4e2, "mp4e2.csv")

############################################################################
#now for plotting T and NR for each model scenario

#need Tcum and NRcum for each scenario
allmod <- cbind(mp4a, mp4b1, mp4b2, mp4c, mp4d)

allmod <- allmod[-c(6,11,16,21)]

ggplot(allmod, aes(x=Datetime)) +
  geom_line(aes(y=tcum_a, color="a,c,e1 [obs]"), size=1) +
  geom_line(aes(y=tcum_b1, color = "b1 [elr]"), size=1) +
  geom_line(aes(y=tcum_b2, color ="b2,d,e2 [L&E]"), size=1) +
  labs(x="", y="Cumulative air temperure (C)", color="model") +
  scale_color_brewer(palette="Dark2")

ggplot(allmod, aes(x=Datetime)) +
  geom_line(aes(y=nrcum_a, color="a"), size=1) +
  geom_line(aes(y=nrcum_b1, color = "b1"), size=1) +
  geom_line(aes(y=nrcum_b2, color ="b2"), size=1) +
  geom_line(aes(y=nrcum_c, color="c"), size=1) +
  geom_line(aes(y=nrcum_d, color="d"), size=1) +
  labs(x="", y="Cumulative net radiation (W/m2)", color="model") +
  scale_color_brewer(palette="Dark2")

#compare tcum to each ta observed
ggplot(allmod, aes(x=Datetime)) +
  geom_line(aes(y=tcum_a-tcum_b1, color="ELR diff")) +
  geom_line(aes(y=tcum_a-tcum_b2, color="L&E diff")) +
  labs(x="", y="Cumulative Ta difference (C)", color="model") +
  scale_color_brewer(palette="Dark2")

#compare nr cum to model a
ggplot(allmod, aes(x=Datetime)) +
  geom_line(aes(y=nrcum_a-nrcum_b1, color="b1 diff")) +
  geom_line(aes(y=nrcum_a-nrcum_b2, color="b2 diff")) +
  geom_line(aes(y=nrcum_a-nrcum_c, color="c diff")) +
  geom_line(aes(y=nrcum_a-nrcum_d, color="d diff")) +
  scale_color_brewer(palette="Dark2") +
  labs(x="", y="Cumulative NR difference (W/m2)", color="model")

#show ta with 1:1
ggplot(allmod, aes(x=tcum_a, y=tcum_b1)) + 
  geom_line() +
  geom_abline(intercept = 0, slope = 1, size=1, color="red") #1:1
  
ggplot(allmod, aes(x=tcum_a, y=tcum_b2)) + 
  geom_line() +
  geom_abline(intercept = 0, slope = 1, size=1, color="red") #1:1

#compare nrcum to each scenario (4)
b1nr <- ggplot(allmod, aes(x=nrcum_a, y=nrcum_b1)) + 
  geom_line() +
  geom_abline(intercept = 0, slope = 1, size=1, color="red") #1:1

b2nr <- ggplot(allmod, aes(x=nrcum_a, y=nrcum_b2)) + 
  geom_line() +
  geom_abline(intercept = 0, slope = 1, size=1, color="red") #1:1

cnr <- ggplot(allmod, aes(x=nrcum_a, y=nrcum_c)) + 
  geom_line() +
  geom_abline(intercept = 0, slope = 1, size=1, color="red") #1:1

dnr <- ggplot(allmod, aes(x=nrcum_a, y=nrcum_d)) + 
  geom_line() +
  geom_abline(intercept = 0, slope = 1, size=1, color="red") #1:1

grid.arrange(b1nr, b2nr, cnr, dnr, nrow=2)

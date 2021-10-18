# calibrating MELT FACTORS for melt 2021

library(ggplot2)
library(lubridate)
library(dplyr)
library(plotly)
library(optimr)
library(RNRCS)
library(hydroGOF)

rm(list = ls()) 

#set working directory
setwd("C:/Users/sears/Documents/Research/Snow_Hydro_Research/Thesis/Data/SNOTEL")

######################################################

#bring in rad data using CAIC (brought in from rad21_modeling.R)
rad21 <- read.csv(file="C:/Users/sears/Documents/Research/Snow_Hydro_Research/Thesis/Data/Radiation/For R/JWrad_hr.csv", 
                  header=TRUE) %>%
  mutate(Datetime = ymd_hms(Datetime))

#model LWin for SNOTEL so bring in MP2 data
temp_elev_21 <- read.csv(file="C:/Users/sears/Documents/Research/Snow_Hydro_Research/Thesis/Data/Air Temp/For R/2021/Melt21_elev_noOUT.csv", 
                         header=TRUE) %>%
  mutate(Datetime = mdy_hm(Datetime))

RH_dewpt <- read.csv(file="C:/Users/sears/Documents/Research/Snow_Hydro_Research/Thesis/Data/Air Temp/For R/2021/RH_dewpt_all.csv", 
                     header=TRUE) %>%
  mutate(Datetime = mdy_hm(Datetime))

######################################################
#pull in daily SNOTEL data -- THIS IS DAILY
#JW21_daily <- grabNRCS.data(network = "SNTL", site_id = "551", timescale = "daily", DayBgn = '2021-04-01', DayEnd = '2021-07-01') %>%
#  mutate(Date = ymd(Date))

JW21_daily <- read.csv("C:/Users/sears/Documents/Research/Snow_Hydro_Research/Thesis/Data/SNOTEL/dailyjw.csv") %>%
  mutate(Date = mdy(Date))

JW21_daily <- JW21_daily %>%
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
  select(-c(Ta_F, precip_accum_in, Sd_in, SWE_in))


#edit LWin for SNOTEL site so need RH from a sensor
#next several lines are pulling mp4 then merging  back w/ rad data
mp2 <- temp_elev_21 %>%
  filter(ID == "MP2")

mp2_rh <- RH_dewpt %>%
  filter(ID == "MP2")

mp2 <- merge(mp2_rh, mp2, by="Datetime")

#get rid of :01 on mp4 time
minute(mp2$Datetime) <- 0

mp2 <- mp2 %>%
  select(-c(ID.x, ID.y, Elevation, Band)) %>%
  rename(mp2RH = humidity,
         mp2Td = dewpoint,
         mp2Ta = AirT_C)
  
rad21 <- merge(rad21, mp2, by="Datetime")

#now finalyl calculate LWin
rad21 <- rad21 %>%
  mutate(ea = (6.112*exp((17.62*mp2Td)/(243.12+mp2Td)))) %>%
  mutate(Lwin_fix = (0.53+(0.065*ea))*(1+(0.4*Cc_fix))*(5.67*10^-8)*((Tjw+273.15)^4)) %>%
  mutate(nr_fix = (SWnet)+ (Lwin_fix-avgLWout))


#get the cumulative T sum and rad sums for the modeled melt
JWhrsum <- rad21 %>%
  mutate(Tpos = if_else(Tjw <0,0,Tjw)) %>%
  group_by(Date = format(Datetime, "%Y-%m-%d")) %>%
  summarize(Tcum = sum(Tpos)*1/24,
            radcum = sum(nr_fix)*1/24) %>%
  mutate(Date = as.Date(Date))


JW21_daily <- merge(JW21_daily, JWhrsum, by="Date")

######################################################
#find observed melt

JW21_melt <- JW21_daily %>%
  mutate(precip_mmday = PrecipAcum_mm - lag(PrecipAcum_mm)) %>%
  mutate(melt_mmday = ifelse(precip_mmday > 0,ifelse(lag(Sd_mm)>Sd_mm, (lag(SWE_mm)-SWE_mm)-precip_mmday,
                                                 precip_mmday-(SWE_mm-lag(SWE_mm))),
                              lag(SWE_mm)-SWE_mm),
         freshsnow = ifelse(lag(Sd_mm)<Sd_mm, precip_mmday, 0)) %>%
  mutate(melt_obs = ifelse(melt_mmday < 0, 0, melt_mmday))

JW21_melt[is.na(JW21_melt)] <- 0

write.csv(JW21_melt, "JW21_melt.csv")

JW21_melt <- JW21_melt %>%
  filter(Date <= "2021-06-11")

######################################################
#optimize 

melt_op <- function(data, par){
  with(data, sum(((par[1]*(Ta_C.x-par[2])+par[3]*nr)-melt_obs)^2))
}

MFs <- optim(par=c(0.11,0,0.2), fn=melt_op, data=JW21_melt)

#write.csv(JW21_melt, "JW21_melt.csv")

######################################################
#see how SNOTEL looks with new MFs
mft <- 1.77463
tref <- 4.57663
mfr <- 0.15009

JW21_melt_test <- JW21_melt %>%
  mutate(melt_mod = if_else(Tcum<=tref,mfr*radcum,
                                    mft*(Tcum-tref)+mfr*radcum)) %>%
  mutate(melt_mod = pmax(melt_mod, 0)) %>%
  mutate(melt_mod_cum = cumsum(melt_mod),
         melt_obs_cum = cumsum(melt_obs))

compare <- ggplot(JW21_melt_test)+geom_line(aes(x=Date, y=melt_mod_cum))+geom_point(aes(x=Date, y=melt_obs_cum))

ggplotly(compare)

ggplot(JW21_melt_test)+geom_point(aes(x=melt_obs_cum, y=melt_mod_cum))+
  geom_abline(intercept = 0, slope = 1, size=1.5, color="red")

NSE(JW21_melt_test$melt_mod_cum, JW21_melt_test$melt_obs_cum)

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
#first find MFs using SNOTEL data (practice) - THIS IS HOURLY

sno21 <- read.csv("JW21.csv", header=TRUE) %>%
  mutate(Date = mdy_hm(Date))

#convert to metric units and get rid of the others
sno21 <- sno21 %>%
  mutate(Ta_C = (AirT_F-32)*(5/9),
         PrecipAcum_mm = PrecipAcum_in*25.4,
         Sd_mm = SnowDepth_in*25.4,
         SWE_mm = SWE_in*25.4) %>%
  select(-c(AirT_F, PrecipAcum_in, SnowDepth_in, SWE_in)) %>%
  mutate(d_SWE = lag(SWE_mm, default=first(SWE_mm)) - SWE_mm)

#bring in rad data using CAIC (brought in from melt21.R)
rad21 <- read.csv(file="C:/Users/sears/Documents/Research/Snow_Hydro_Research/Thesis/Data/Radiation/For R/Rad_melt21.csv", 
                  header=TRUE) %>%
  mutate(Datetime = mdy_hm(Datetime))

#get hourly rad data
rad21 <- rad21 %>%
  filter(minute(Datetime) == 0) %>%
  filter(row_number() %% 2 != 1) %>% #delete odd rows
  rename(Date = Datetime)

sno21 <- merge(sno21, rad21, by="Date")

sno21 <- sno21 %>%
  mutate(NR = (Swin-Swout)+(Lwin-Lwout))

######################################################
#pull in daily SNOTEL data -- THIS IS DAILY
JW21_daily <- grabNRCS.data(network = "SNTL", site_id = 551, timescale = "daily", DayBgn = '2021-04-01', DayEnd = '2021-07-01') %>%
  mutate(Date = ymd(Date))

JW21_daily <- JW21_daily %>%
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
  select(-c(Ta_F, precip_accum_in, Sd_in, SWE_in))
  

#pull in hourly rad data to join w/ daily SNOTEL
rad_daily <- sno21 %>%
  select(Date, NR) %>%
  group_by(Date = format(Date, "%Y-%m-%d")) %>%
  summarize(avgNR = mean(NR)) %>%
  mutate(Date = ymd(Date))
  

JW21_daily <- merge(JW21_daily, rad_daily, by="Date")

######################################################
#find observed melt

JW21_daily_test <- JW21_daily %>%
  mutate(precip_mmday = PrecipAcum_mm - lag(PrecipAcum_mm)) %>%
  mutate(melt_mmday = ifelse(precip_mmday > 0,ifelse(lag(Sd_mm)>Sd_mm, (lag(SWE_mm)-SWE_mm)-precip_mmday,
                                                 precip_mmday-(SWE_mm-lag(SWE_mm))),
                              lag(SWE_mm)-SWE_mm)) %>%
  mutate(melt_obs = ifelse(melt_mmday < 0, 0, melt_mmday))

JW21_daily_test[is.na(JW21_daily_test)] <- 0

JW21_daily_test <- JW21_daily_test %>%
  filter(Date <= "2021-06-11")

######################################################
#optimize 

melt_op <- function(data, par){
  with(data, sum(((par[1]*(Ta_C-0)+par[2]*avgNR)-melt_obs)^2))
}

MFs <- optim(par=c(1,0.01), fn=melt_op, data=JW21_daily_test)
 
######################################################
#see how SNOTEL looks with new MFs
JW21_daily_test <- JW21_daily_test %>%
  mutate(melt_mod = pmax(1.171*(Ta_C-0)+(-0.0497)*(avgNR),0)) %>%
  mutate(melt_mod_cum = cumsum(melt_mod_fix),
         melt_obs_cum = cumsum(melt_obs))

compare <- ggplot(JW21_daily_test)+geom_line(aes(x=Date, y=melt_mod_cum))+geom_point(aes(x=Date, y=melt_obs_cum))

ggplotly(compare)

ggplot(JW21_daily_test)+geom_point(aes(x=melt_obs_cum, y=melt_mod_cum))+
  geom_abline(intercept = 0, slope = 1, size=1.5, color="red")

NSE(JW21_daily_test$melt_mod_cum, JW21_daily_test$melt_obs_cum)

######################################################



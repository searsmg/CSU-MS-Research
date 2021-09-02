# calibrating MELT FACTORS for melt 2021

library(ggplot2)
library(lubridate)
library(dplyr)
library(plotly)
library(optimr)
library(RNRCS)

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

######################################################
#optimize 

melt <- function(data, par){
  with(data, sum(((par[1]*(Ta_C-0)+par[2]*NR)-d_SWE)^2))
}

MFs <- optim(par=c(0,0), fn=melt, data=sno21)
 
######################################################
#see how SNOTEL looks with new MFs
sno21 <- sno21 %>%
  mutate(melt_mod = 0.0277317240*(Ta_C-0)+-0.0009764329*(NR))

compare <- ggplot(sno21)+geom_line(aes(x=Date, y=melt_mod))+geom_point(aes(x=Date, y=d_SWE))

ggplotly(compare)

######################################################




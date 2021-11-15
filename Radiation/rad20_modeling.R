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
  mutate(Datetime = mdy_hm(Datetime))

#JW21daily <- read.csv("C:/Users/sears/Documents/Research/Snow_Hydro_Research/Thesis/Data/SNOTEL/dailyjw.csv") %>%
#  mutate(Date = mdy(Date))

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
  mutate(Datetime = ymd_hms(Datetime)) %>%
  filter(Datetime > ymd_hms("2020-04-30 23:00:00")) %>%
  filter(Datetime < ymd_hms("2020-07-01 00:00:00"))

ggplot(rad_hr) +
  geom_line(aes(x=Datetime, y=avgSWin))

ggplot(rad_hr) +
  geom_line(aes(x=Datetime, y=avgSWout))

ggplot(rad_hr) +
  geom_line(aes(x=Datetime, y=avgLWin))

ggplot(rad_hr) +
  geom_line(aes(x=Datetime, y=avgLWout))


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
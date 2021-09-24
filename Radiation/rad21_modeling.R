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

rad21 <- read.csv(file="Rad_melt21.csv", 
                  header=TRUE) %>%
  mutate(Datetime = mdy_hm(Datetime))

#pull in hourly SNOTEL data for precip
JW21_hr <- grabNRCS.data(network = "SNTL", site_id = 551, timescale = "hourly", DayBgn = '2021-04-01', DayEnd = '2021-07-01') %>%
  mutate(Datetime = ymd_hm(Date))


JW21 <- JW21_hr %>%
  rename(Ta_F = Air.Temperature.Observed..degF., 
         precip_accum_in = Precipitation.Accumulation..in.,
         Sd_in = Snow.Depth..in.,
         SWE_in = Snow.Water.Equivalent..in.) %>%
  mutate(Ta_C = (Ta_F-32)*(5/9),
         PrecipAcum_mm = precip_accum_in*25.4,
         Sd_mm = Sd_in*25.4,
         SWE_mm = SWE_in*25.4) %>%
  select(-c(Ta_F, precip_accum_in, Sd_in, SWE_in, Date)) %>%
  mutate(precip_mm = PrecipAcum_mm - lag(PrecipAcum_mm)) %>%
  drop_na()

JW21_hr_al <- JW21 %>%
  mutate(precip_mm = if_else(precip_mm<2.54,0,precip_mm)) %>%
  mutate(swe_mm = SWE_mm - lag(SWE_mm),
         sdepth = Sd_mm - lag(Sd_mm)) %>%
  slice(-1) %>%
  mutate(freshsnow = if_else(sdepth > 10 & swe_mm > 2.54, precip_mm, 0)) %>%
  mutate(fresh_al = ifelse(freshsnow > 0, 0.84, NA),
         al_min = if_else(Ta_C < 0, 0.7, 0.5)) %>%
  mutate(snowcum = cumsum(if_else(is.na(freshsnow),0,freshsnow))) %>%
  mutate(actual_al = ifelse(snowcum == 0, al_min, NA)) %>%
  mutate(actual_al = ifelse(freshsnow > 0, 0.84, actual_al))
  
albedo = JW21_hr_al$actual_al

for(i in 2:nrow(JW21_hr_al)){
  if(is.na(albedo[i])){
    albedo[i] = ((albedo[i-1]-JW21_hr_al$al_min[i])*exp(-0.01)) + JW21_hr_al$al_min[i]
  }
}

ggplot(JW21_hr_al)+geom_line(aes(Datetime, actual_al))

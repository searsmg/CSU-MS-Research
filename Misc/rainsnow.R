#pulling JW SNOTEL data in with sensor MP2 to determine rain snow threshold

library(ggplot2)
library(lubridate)
library(dplyr)

#set working directory
setwd("C:/Users/sears/Documents/Research/Snow_Hydro_Research/Thesis/Data/SNOTEL")

#pull in JW data for Apr 2020 to June 2021
jw <- read.csv("JWforrainsnow.csv") %>%
  mutate(Datetime = mdy_hm(Datetime))

#bring in MP2 RH data
RH_dewpt <- read.csv(file="C:/Users/sears/Documents/Research/Snow_Hydro_Research/Thesis/Data/Air Temp/For R/2021/RH_dewpt_all.csv", 
                     header=TRUE) %>%
  mutate(Datetime = mdy_hm(Datetime))

rh <- RH_dewpt %>%
  filter(ID == "MP2")

minute(rh$Datetime) <- 0

jw <- merge(rh, jw, by="Datetime")

jw <- jw %>%
  mutate(Sd_in = ifelse(Sd_in<0,0,Sd_in),
         Precip_In = ifelse(Precip_In<0,0,Precip_In),
         precip_in = Precip_In-lag(Precip_In)) %>%
  mutate(precip_in = ifelse(precip_in < 0.1, 0, precip_in)) %>%
  mutate(ptype = ifelse(precip_in >0 & Sd_in>lag(Sd_in), 
                        "snow", ifelse(precip_in & (Sd_in==lag(Sd_in)| Sd_in<lag(Sd_in)),
                                       "rain", "none"))) %>%
  filter(!ptype == "none")


write.csv(jw, "rainsnow.csv")

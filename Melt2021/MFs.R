# calibrating MELT FACTORS for melt 2021

library(ggplot2)
library(lubridate)
library(dplyr)

rm(list = ls()) 

#set working directory and csv file
setwd("C:/Users/sears/Documents/Research/Snow_Hydro_Research/Thesis/Data/SNOTEL")

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

#find observed melt


# function for melt
melt <- function(MFt, Ta, Tt, MFh, NR){
  melt_mm <- MFt*(Ta-Tt)+MFh*(NR)
  return(melt_mm)
}

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

#define constants
MFt <- 1
Tt <- 1
MFh <- 1

# modelet melt
sno21$melt_mod <- melt(MFt, sno21$Ta_C, Tt, MFh, sno21$NR)



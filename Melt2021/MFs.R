# calibrating MELT FACTORS for melt 2021

library(ggplot2)
library(lubridate)
library(dplyr)
library(plotly)
library(optimr)

rm(list = ls()) 

#set working directory
setwd("C:/Users/sears/Documents/Research/Snow_Hydro_Research/Thesis/Data/SNOTEL")

######################################################
#first find MFs using SNOTEL data (practice)

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
######################################################
#Find MFs using snow pit from melt observed



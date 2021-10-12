#dew pt temp for melt season 2020 and 2021 - finding slopes, R2, and p values

library(broom)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(dplyr)
library(plotly)
library(gridExtra)
library(scales)
library(RColorBrewer)
library(plotly)
library(tidyr)
library(tibble)

rm(list = ls())

#set working directory and csv file
setwd("C:/Users/sears/Documents/Research/Snow_Hydro_Research/Thesis/Data/Dewpt Temp/For R/")

#############################################################################
#set up data

#get temp, elev data etc.
T20 <- read.csv(file = "AirT_elev20_noOUT.csv",
                header=TRUE) %>%
  mutate(Datetime = mdy_hm(Datetime))

T21 <- read.csv(file="Melt21_elev_noOUT.csv",
                header=TRUE) %>%
  mutate(Datetime = mdy_hm(Datetime))

#bring in all the RH and dewpt temp data
RH_dewpt <- read.csv(file="RH_dewpt_all.csv", header=TRUE) %>%
  mutate(Datetime = mdy_hm(Datetime))

#now merge T and dewpt/RH data
all21 <- merge(T21, RH_dewpt, by=c("Datetime", "ID"))

all20 <- merge(T20, RH_dewpt, by=c("Datetime", "ID"))

#get rid of the 01 minutes and make it 00
minute(all20$Datetime) <- 0
minute(all21$Datetime) <- 0

############################################################################
#data are set up - run models now for 2020 (get slope, r2, and p-value)

#slope, r2, deg of free, and pval for 2020 hourly
fit_model <- function(all20) lm(dewpoint ~ Elevation, data = all20) #linear model
get_slope <- function(mod) tidy(mod)$estimate[2] #pull out the slope
pearson <- function(all20) cor.test(all20$Elevation, all20$dewpoint, data=all20) #pearson cor test
pval <- function(pear) tidy(pear)$p.value #p value from cor test
df <- function(pear) tidy(pear)$parameter[1] #deg of freedom from cor test

#calculate r2
all20_r <- all20 %>%
  select(-c(ID)) %>%
  group_by(Datetime) %>%
  summarize(r2 = cor(Elevation, dewpoint, use="complete.obs")^2)

#run lm, pull out slope, pearson cor test, pull out pval + deg of free
slope20 <- all20 %>%
  group_nest(Datetime) %>%
  mutate(model = map(data, fit_model)) %>%
  mutate(slope = map_dbl(model, get_slope)) %>%
  mutate(slope_Ckm = slope*1000) %>%
  mutate(pear = map(data, pearson)) %>%
  mutate(pval = map_dbl(pear, pval)) %>%
  mutate(df = map_dbl(pear, df))

#add derived r2 to slope 20 (with rest of dadta)
slope20 <- slope20 %>%
  add_column(r2 = all20_r$r2)

############################################################################
#run models now for 2021 (get slope, r2, and p-value)

#slope, r2, deg of free, and pval for 2021 hourly
fit_model <- function(all21) lm(dewpoint ~ Elevation, data = all21) #linear model
get_slope <- function(mod) tidy(mod)$estimate[2] #pull out the slope
pearson <- function(all21) cor.test(all21$Elevation, all21$dewpoint, data=all21) #pearson cor test
pval <- function(pear) tidy(pear)$p.value #p value from cor test
df <- function(pear) tidy(pear)$parameter[1] #deg of freedom from cor test

#calculate r2
all21_r <- all21 %>%
  select(-c(ID)) %>%
  group_by(Datetime) %>%
  summarize(r2 = cor(Elevation, dewpoint, use="complete.obs")^2)

#run lm, pull out slope, pearson cor test, pull out pval + deg of free
slope21 <- all21 %>%
  group_nest(Datetime) %>%
  mutate(model = map(data, fit_model)) %>%
  mutate(slope = map_dbl(model, get_slope)) %>%
  mutate(slope_Ckm = slope*1000) %>%
  mutate(pear = map(data, pearson)) %>%
  mutate(pval = map_dbl(pear, pval)) %>%
  mutate(df = map_dbl(pear, df))

#add derived r2 to slope 21 (with rest of data)
slope21 <- slope21 %>%
  add_column(r2 = all21_r$r2)
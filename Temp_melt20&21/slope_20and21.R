#air T for 2020 and 2021

library(broom)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(dplyr)
library(plotly)
library(gridExtra)
library(scales)
library(RColorBrewer)

rm(list = ls()) 

#set working directory and csv file
setwd("C:/Users/sears/Documents/Research/Snow_Hydro_Research/Thesis/Data/Air Temp")

#bring in 20 and 21 temp datasets
T20 <- read.csv(file = "C:/Users/sears/Documents/Research/Snow_Hydro_Research/Thesis/Data/Air Temp/For R/2020/AirT_elev20_noOUT.csv",
                header=TRUE) %>%
  mutate(Datetime = mdy_hm(Datetime))

T21 <- read.csv(file="C:/Users/sears/Documents/Research/Snow_Hydro_Research/Thesis/Data/Air Temp/For R/2021/Melt21_elev_noOUT.csv",
                header=TRUE) %>%
  mutate(Datetime = mdy_hm(Datetime))

######################################################################
#2020 slope
fit_model <- function(T20) lm(AirT_C ~ Elevation, data = T20)
get_slope <- function(mod) tidy(mod)$estimate[2]


T20_r <- T20 %>%
  select(-c(ID, Band)) %>%
  group_by(Datetime) %>%
  summarize(r2 = cor(Elevation, AirT_C, use="complete.obs")^2)

slope20 <- T20 %>%
  group_nest(Datetime) %>%
  mutate(model = map(data, fit_model)) %>%
  mutate(slope = map_dbl(model, get_slope)) %>%
  mutate(slope_Ckm = slope*1000)

slope20 <- slope20 %>%
  add_column(r2 = T20_r$r2)

######################################################################
#2021 slope
fit_model <- function(T21) lm(AirT_C ~ Elevation, data = T21)
get_slope <- function(mod) tidy(mod)$estimate[2]

T21_r <- T21 %>%
  select(-c(ID, Band)) %>%
  group_by(Datetime) %>%
  summarize(r2 = cor(Elevation, AirT_C, use="complete.obs")^2)

slope21 <- T21 %>%
  group_nest(Datetime) %>%
  mutate(model = map(data, fit_model)) %>%
  mutate(slope = map_dbl(model, get_slope)) %>%
  mutate(slope_Ckm = slope*1000)

slope21 <- slope21 %>%
  add_column(r2 = T21_r$r2)

#########################################################################
#wide data format
T20_wide <- T20 %>%
  select(Datetime, ID, AirT_C) %>%
  pivot_wider(names_from = ID, values_from=AirT_C) %>%
  add_column(slope_Ckm = slope20$slope_Ckm,
             r2 = T20_r$r2)

T21_wide <- T21 %>%
  select(Datetime, ID, AirT_C) %>%
  pivot_wider(names_from = ID, values_from=AirT_C) %>%
  add_column(slope_Ckm = slope21$slope_Ckm,
             r2 = T21_r$r2)

#########################################################################

  

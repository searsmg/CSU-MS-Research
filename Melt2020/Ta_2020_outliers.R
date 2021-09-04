# look for Ta outliers for melt 20 season

library(ggplot2)
library(lubridate)
library(dplyr)
library(stringr)
library(outliers)
library(plotly)
library(tidyverse)

rm(list = ls())
######################

setwd("C:/Users/sears/Documents/Research/Snow_Hydro_Research/Thesis/Data/Air Temp/For R/2020") #update wd

airT_20 <- read.csv(file="AirT_Melt2020_noOUT.csv", header=TRUE) %>%
  mutate(date = mdy_hm(date))

looking <- plot_ly(data=airT_20, x=~date, y=~temperature, color=~ID)
looking

Ta20_noOUT <- read.csv(file="AirT_Melt2020_noOUT_checking.csv", header=TRUE) %>%
  mutate(Datetime = mdy_hm(Datetime))

Ta20_noOUT <- Ta20_noOUT %>%
  pivot_longer(!Datetime, names_to = "ID", values_to = "AirT_C")

looking2 <- plot_ly(data=Ta20_noOUT, x=~Datetime, y=~AirT_C, color=~ID)
looking2

write.csv(Ta20_noOUT, "AirT_Melt20_noOUT.csv")

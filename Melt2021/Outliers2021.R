
library(ggplot2)
library(lubridate)
library(dplyr)
library(stringr)
library(outliers)
library(plotly)

rm(list = ls()) 

############DATA PREP#############

#set working directory and csv file
setwd("C:/Users/sears/Documents/Research/Snow_Hydro_Research/Thesis/Data/Air Temp") #update wd

outlier21 <- read.csv(file="AirT_outliers_21.csv", header=TRUE)
outlier21 <- subset(outlier21, select = -X)

test <- dixon.test(outlier21$X4.10.21.12.01)
test

allout <- lapply(outlier21,dixon.test)

allout

airT_21 <- read.csv(file="AirT_Melt2021.csv", header=TRUE) %>%
  mutate(date = mdy_hm(date))

looking <- plot_ly(data=airT_21, x=airT_21$date, y=airT_21$temperature, color=airT_21$ID)
looking

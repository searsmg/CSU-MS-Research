#lapse rate analysis for melt season 2020

rm(list = ls())

library(ggplot2)
library(lubridate)
library(dplyr)
library(stringr)
library(tidyverse)
library(RColorBrewer)
library(reshape2)

############DATA PREP#############

#set working directory and csv file
setwd("C:/Users/sears/Documents/Research/Snow_Hydro_Research/Field_Data_071120/Processed/Calcs and Plots/")

MP_HD_All <- read.csv("C:/Users/sears/Documents/Research/Snow_Hydro_Research/Field_Data_071120/Processed/Calcs and Plots/MP_HD_All.csv") %>%
  mutate(date = mdy_hms(date))

############PLOTS##########################

#Height and width for plots
PlotWidth = 15
PlotHeight = 9

#ggplot theme to control formatting parameters for plots with month on the x-axis
PlotTheme = theme(axis.text=element_text(size=20),    #Text size for axis tick mark labels
                  axis.title.x=element_text(size=24, hjust=0.5, margin=margin(t=20, r=20, b=20, l=20)),               #Text size and alignment for x-axis label
                  axis.title.y=element_text(size=24, vjust=0.5,  margin=margin(t=20, r=20, b=20, l=20)),              #Text size and alignment for y-axis label
                  plot.title=element_text(size=26,face="bold",hjust=0.5, margin=margin(t=20, r=20, b=20, l=20)),      #Text size and alignment for plot title
                  legend.title=element_blank(),                                                                    #Text size of legend category labels
                  legend.text=element_text(size=20),                                                                   #Text size of legend title
                  legend.position = "bottom")        

###############################################################################

#lapse rates - ALL MP

#MP Stats
MP <- MP_HD_All %>%
  filter(Side == as.character("MP"))

MP_Daily <- MP %>%
  mutate(day = as.Date(date, format = "%Y-%m-%d"))

MP_DailyStats <- MP_Daily %>%
  group_by(day, Elevation, ID, Side) %>%
  summarize(AvgT = mean(temperature), MaxT = max(temperature), MinT = min(temperature),
            AvgDewT = mean(dewpoint), MaxDewT = max(dewpoint), MinDewT = min(dewpoint))

#write.csv(MP_DailyStats, "MP_DailyStats.csv")

MP_DailyAvgT <- read.csv("C:/Users/sears/Documents/Research/Snow_Hydro_Research/Field_Data_071120/Processed/Calcs and Plots/MP_DailyAvgTemp.csv") %>%
    mutate(Date = mdy(Date))

PLOT = "MP Daily Avg T_all"
ggplot(MP_DailyAvgT) + geom_line(aes(x=Date, y=MP1, color="MP1"), size=1) + geom_line(aes(x=Date, y=MP2, color="MP2"), size=1) + geom_line(aes(x=Date, y=MP3, color="MP3"), size=1) + geom_line(aes(x=Date, y=MP4, color="MP4"), size=1) + geom_line(aes(x=Date, y=MP5, color="MP5"), size=1) + geom_line(aes(x=Date, y=MP6, color="MP6"), size=1) + geom_line(aes(x=Date, y=MP7, color="MP7"), size=1) + PlotTheme + scale_colour_brewer(palette="RdPu") + labs(x="Date", y="Daily Avg Temp (deg C")

ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)

PLOT = "MP Daily Avg T"
ggplot(MP_DailyAvgT) + geom_line(aes(x=Date, y=MP1, color="MP1"), size=1) + geom_line(aes(x=Date, y=MP2, color="MP2"), size=1) + geom_line(aes(x=Date, y=MP4, color="MP4"), size=1) + geom_line(aes(x=Date, y=MP5, color="MP5"), size=1) + geom_line(aes(x=Date, y=MP6, color="MP6"), size=1) + geom_line(aes(x=Date, y=MP7, color="MP7"), size=1) + scale_colour_brewer(palette="Spectral") + theme_classic() + PlotTheme + labs(x="Date", y="Daily Avg Temp (deg C") 

ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)

#MP MAX
MP_DailyMaxT <- read.csv("C:/Users/sears/Documents/Research/Snow_Hydro_Research/Field_Data_071120/Processed/Calcs and Plots/MP_DailyMaxTemp.csv") %>%
  mutate(Date = mdy(Date))

PLOT = "MP Daily Max T"
ggplot(MP_DailyMaxT) + geom_line(aes(x=Date, y=MP1, color="MP1"), size=1) + geom_line(aes(x=Date, y=MP2, color="MP2"), size=1) + geom_line(aes(x=Date, y=MP4, color="MP4"), size=1) + geom_line(aes(x=Date, y=MP5, color="MP5"), size=1) + geom_line(aes(x=Date, y=MP6, color="MP6"), size=1) + geom_line(aes(x=Date, y=MP7, color="MP7"), size=1) + scale_colour_brewer(palette="Spectral") + theme_classic() + PlotTheme + labs(x="Date", y="Daily Max Temp (deg C)") 

ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)

#MP MAX
MP_DailyMinT <- read.csv("C:/Users/sears/Documents/Research/Snow_Hydro_Research/Field_Data_071120/Processed/Calcs and Plots/MP_DailyMinTemp.csv") %>%
  mutate(Date = mdy(Date))

PLOT = "MP Daily Min T"
ggplot(MP_DailyMinT) + geom_line(aes(x=Date, y=MP1, color="MP1"), size=1) + geom_line(aes(x=Date, y=MP2, color="MP2"), size=1) + geom_line(aes(x=Date, y=MP4, color="MP4"), size=1) + geom_line(aes(x=Date, y=MP5, color="MP5"), size=1) + geom_line(aes(x=Date, y=MP6, color="MP6"), size=1) + geom_line(aes(x=Date, y=MP7, color="MP7"), size=1) + scale_colour_brewer(palette="Spectral") + theme_classic() + PlotTheme + labs(x="Date", y="Daily Min Temp (deg C)") 

ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)

#########################################################################################################

#HD Stats
HD <- MP_HD_All %>%
  filter(Side == as.character("HD"))

HD_Daily <- HD %>%
  mutate(day = as.Date(date, format = "%Y-%m-%d"))

HD_DailyStats <- HD_Daily %>%
  group_by(day, Elevation, ID, Side) %>%
  summarize(AvgT = mean(temperature), MaxT = max(temperature), MinT = min(temperature),
            AvgDewT = mean(dewpoint), MaxDewT = max(dewpoint), MinDewT = min(dewpoint))

#####################################################

DailyStats <- rbind(HD_DailyStats, MP_DailyStats)

##################################
AvgT_all <- MP_HD_All %>%
  group_by(ID, Elevation, Side) %>%
  summarize(AvgT = mean(temperature))
  
PLOT="Avg T - All vs. Elevation"
ggplot(AvgT_all) + geom_point(aes(x=Elevation, y=AvgT, shape=Side), size=3) + PlotTheme + labs(x="Elevation (m)", y="Avg Temp (deg C)")

ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)

####################################
MinT_all <- MP_HD_All %>%
  group_by(ID, Elevation, Side) %>%
  summarize(MinT = min(temperature))

PLOT="Avg Min T - All vs. Elevation"
ggplot(MinT_all) + geom_point(aes(x=Elevation, y=MinT, shape=Side), size=3) + PlotTheme + labs(x="Elevation (m)", y="Avg Min Temp (deg C)")

ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)

###########################################
MaxT_all <- MP_HD_All %>%
  group_by(ID, Elevation, Side) %>%
  summarize(MaxT = max(temperature))

PLOT="Avg Max T - All vs. Elevation"
ggplot(MaxT_all) + geom_point(aes(x=Elevation, y=MaxT, shape=Side), size=3) + PlotTheme + labs(x="Elevation (m)", y="Avg Max Temp (deg C)")

ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)

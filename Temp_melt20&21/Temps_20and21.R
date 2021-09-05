# looking at air temps for melt season 2020 and 2021

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
################################################
PlotFormat = theme(axis.text=element_text(size=20),
                   axis.title.x=element_text(size=24, hjust=0.5, margin=margin(t=20, r=20, b=20, l=20)),              
                   axis.title.y=element_text(size=24, vjust=0.5,  margin=margin(t=20, r=20, b=20, l=20)),              
                   plot.title=element_text(size=26,face="bold",hjust=0.5, margin=margin(t=20, r=20, b=20, l=20)),      
                   legend.title=element_blank(),                                                                    
                   legend.text=element_text(size=20),                                                                   
                   legend.position = "bottom", 
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   strip.text = element_text(size=25))


######################################################################

#remvoe the 01 from the minutes
minute(T20$Datetime) <- 0
minute(T21$Datetime) <- 0

PLOT = "Temp Plot"
t20 <- ggplot() + geom_point(data=T20, aes(x=Elevation, y=AirT_C, colour=ID))
t20
t21 <- ggplot() + geom_point(data=T21, aes(x=Elevation, y=AirT_C,colour=ID))
grid.arrange(t20,t21, ncol=2)

#ggsave(paste(PLOT,".png",sep=""), width = 15, height = 9)

##########################

T20_long <- T20 %>%
  select(Datetime, ID, AirT_C) %>%
  pivot_wider(names_from = ID, values_from=AirT_C)

write.csv(T20_long, "T20.csv")

T21_long <- T21 %>%
  select(Datetime, ID, AirT_C) %>%
  pivot_wider(names_from = ID, values_from=AirT_C)

write.csv(T21_long, "T21.csv")

####
T20_slope <- read.csv(file = "C:/Users/sears/Documents/Research/Snow_Hydro_Research/Thesis/Data/Air Temp/T20_nstge.csv",
                header=TRUE) %>%
  mutate(Datetime = mdy_hm(Datetime))

T21_slope <- read.csv(file = "C:/Users/sears/Documents/Research/Snow_Hydro_Research/Thesis/Data/Air Temp/T21_nstge.csv",
                      header=TRUE) %>%
  mutate(Datetime = mdy_hm(Datetime))

#########

slope20 <- ggplot() + geom_point(data=T20_slope, aes(x=Datetime, y=Slope_degCkm)) +
  ylim(-30,30) +
  geom_hline(yintercept=6.5) +
  scale_x_datetime(date_labels = "%b") +
  PlotFormat + 
  labs(x="Date", y="NSTGE (deg C/km)")
slope20

slope21 <- ggplot() + geom_point(data=T21_slope, aes(x=Datetime, y=Slope_degCkm)) +
  geom_hline(yintercept=6.5)+
  scale_x_datetime(date_labels = "%b") +
  PlotFormat + 
  labs(x="Date", y="NSTGE (deg C/km)")
slope21

grid.arrange(slope20, slope21, ncol=2)
###############

T20_slope <- T20_slope %>%
  mutate(year = "2020")

T21_slope <- T21_slope %>%
  mutate(year = "2021")

slope <- rbind(T20_slope, T21_slope)

PLOT = "NSTGE_20&21"
ggplot(slope, aes(x=Datetime, y=Slope_degCkm)) +
  geom_point() + 
  geom_hline(yintercept=-6.5, size=1, color="Red") +
  ylim(-30,30) +
  labs(x= "Date", y="NSTGE (deg C/km)") +
  scale_x_datetime(date_labels = "%b", date_break = "1 month") +
  facet_wrap(~year, scales="free_x") + PlotFormat

ggsave(paste(PLOT,".png",sep=""), width = 15, height = 9)

##########################

slope_edit <- slope %>%
  mutate(hour = hour(Datetime)) %>%
  mutate(doy = yday(Datetime)) %>%
  filter(Slope_degCkm < 40) %>%
  filter(Slope_degCkm > -32)

PLOT="heatmap_slope"
slope <- ggplot(slope_edit, aes(x=doy, y=hour, fill=Slope_degCkm)) +
  geom_tile() + facet_grid(~year, scale="free_x") +
  scale_fill_distiller(palette = 'RdYlBu')+
  labs(fill="C/km")

ggsave(paste(PLOT,".png",sep=""), width = 15, height = 9)

PLOT = "heatmap_R2"
r2 <- ggplot(slope_edit, aes(x=doy, y=hour, fill=R2)) +
  geom_tile() + facet_grid(~year, scale="free_x") +
  labs(fill="R^2") +
  scale_fill_distiller(palette="Greens", trans="reverse")

ggsave(paste(PLOT,".png",sep=""), width = 15, height = 9)

grid.arrange(slope, r2, nrow=2)

###################################
#find average slope and average R2 by day 

daily_stat <- slope %>%
  summarize(slope_avg = mean(Slope_degCkm),
            slope_sd = sd(Slope_degCkm),
            R2_avg = mean(R2),
            R2_sd = sd(R2))

#find avg slope and R2 by time of day. split between sun hours and non sun hours
am_stat <- slope %>%
  mutate(hour = hour(Datetime)) %>%
  filter(hour %in% (8:18)) %>%
  summarize(slope_avg = mean(Slope_degCkm),
             slope_sd = sd(Slope_degCkm),
             R2_avg = mean(R2),
             R2_sd = sd(R2))

pm_stat <- slope %>%
  mutate(hour = hour(Datetime)) %>%
  filter(!hour %in% (8:18)) %>%
  summarize(slope_avg = mean(Slope_degCkm),
            slope_sd = sd(Slope_degCkm),
            R2_avg = mean(R2),
            R2_sd = sd(R2))

####now do all of the above but split by year
#2020 first
daily20_stat <- T20_slope %>%
  summarize(slope_avg = mean(Slope_degCkm),
            slope_sd = sd(Slope_degCkm),
            R2_avg = mean(R2),
            R2_sd = sd(R2))

#find avg slope and R2 by time of day. split between sun hours and non sun hours
am20_stat <- T20_slope %>%
  mutate(hour = hour(Datetime)) %>%
  filter(hour %in% (8:18)) %>%
  summarize(slope_avg = mean(Slope_degCkm),
            slope_sd = sd(Slope_degCkm),
            R2_avg = mean(R2),
            R2_sd = sd(R2))

pm20_stat <- T20_slope %>%
  mutate(hour = hour(Datetime)) %>%
  filter(!hour %in% (8:18)) %>%
  summarize(slope_avg = mean(Slope_degCkm),
            slope_sd = sd(Slope_degCkm),
            R2_avg = mean(R2),
            R2_sd = sd(R2))

#2021 next
daily21_stat <- T21_slope %>%
  summarize(slope_avg = mean(Slope_degCkm),
            slope_sd = sd(Slope_degCkm),
            R2_avg = mean(R2),
            R2_sd = sd(R2))

#find avg slope and R2 by time of day. split between sun hours and non sun hours
am21_stat <- T21_slope %>%
  mutate(hour = hour(Datetime)) %>%
  filter(hour %in% (8:18)) %>%
  summarize(slope_avg = mean(Slope_degCkm),
            slope_sd = sd(Slope_degCkm),
            R2_avg = mean(R2),
            R2_sd = sd(R2))

pm21_stat <- T21_slope %>%
  mutate(hour = hour(Datetime)) %>%
  filter(!hour %in% (8:18)) %>%
  summarize(slope_avg = mean(Slope_degCkm),
            slope_sd = sd(Slope_degCkm),
            R2_avg = mean(R2),
            R2_sd = sd(R2))

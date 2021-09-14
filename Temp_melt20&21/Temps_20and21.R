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
PlotFormat = theme(axis.text=element_text(size=16, color="black"),
                   axis.title.x=element_text(size=18, hjust=0.5, margin=margin(t=20, r=20, b=20, l=20), color="black"),              
                   axis.title.y=element_text(size=18, vjust=0.5,  margin=margin(t=20, r=20, b=20, l=20), color="black"),              
                   plot.title=element_text(size=26,face="bold",hjust=0.5, margin=margin(t=20, r=20, b=20, l=20)),      
                   legend.title=element_text(size=16, color="black"),                                                                    
                   legend.text=element_text(size=16, color="black"),                                                                   
                   legend.position = "right", 
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

#T20_long <- T20 %>%
#  select(Datetime, ID, AirT_C) %>%
#  pivot_wider(names_from = ID, values_from=AirT_C)

#write.csv(T20_long, "T20.csv")

#T21_long <- T21 %>%
#  select(Datetime, ID, AirT_C) %>%
#  pivot_wider(names_from = ID, values_from=AirT_C)

#write.csv(T21_long, "T21.csv")

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
  geom_point(aes(colour=R2)) + 
  geom_hline(aes(yintercept=-6.5, linetype="ELR"), color="Red", size=1) +
  ylim(-20,30) +
  labs(x= "Date", y=expression("NSTGE " (degree*C/km)), color=expression(paste("R"^2))) +
  scale_x_datetime(date_labels = "%b", date_break = "1 month") +
  facet_wrap(~year, scales="free_x") + PlotFormat +
  scale_color_gradient(low='grey', high='black')+
  scale_linetype_manual(name ="", values = c('solid')) 


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
  labs(fill=expression(degree*C/km), x="Day of year", y="Hour") + PlotFormat +
  scale_y_continuous(breaks=seq(0, 23, 4))
slope
  
ggsave(paste(PLOT,".png",sep=""), width = 15, height = 9)

PLOT = "heatmap_R2"
r2 <- ggplot(slope_edit, aes(x=doy, y=hour, fill=R2)) +
  geom_tile() + facet_grid(~year, scale="free_x") +
  labs(fill=expression("R"^2), x="Day of year", y="Hour") +
  scale_fill_gradientn(colors=brewer.pal(name="Greys", n=3)) +
  scale_y_continuous(breaks=seq(0, 23, 4)) + PlotFormat
r2
  
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

################################################################
#analyze between HD and MP sides for slope and R2

slope20_hd <- read.csv(file = "T20_hd.csv") %>%
  mutate(Datetime = mdy_hm(Datetime))

slope20_m <- read.csv(file = "T20_m.csv") %>%
  mutate(Datetime = mdy_hm(Datetime))

slope21_hd <- read.csv(file = "T21_hd.csv") %>%
  mutate(Datetime = mdy_hm(Datetime))

slope21_m <- read.csv(file = "T21_m.csv") %>%
  mutate(Datetime = mdy_hm(Datetime))

slope20_hd <- slope20_hd %>%
  select(Datetime, Slope_km, R2) %>%
  mutate(year = "2020") %>%
  mutate(side ="hd")

slope20_m <- slope20_m %>%
  select(Datetime, Slope_km, R2) %>%
  mutate(year = "2020") %>%
  mutate(side ="m")

slope_20side <- rbind(slope20_hd, slope20_m)

PLOT = "NSTGE_byside_20"
ggplot(slope_20side, aes(x=Datetime, y=Slope_km)) +
  geom_point(aes(colour=R2)) + 
  geom_hline(yintercept=-6.5, size=1, color="Red") +
  ylim(-30,30) +
  labs(x= "Date", y=expression("NSTGE " (degree*C/km)), color=expression(paste("R"^2))) +
  scale_x_datetime(date_labels = "%b", date_break = "1 month") +
  facet_wrap(~side, scales="free_x", labeller=labeller(side = c("hd" = "Hot Dog", "m" = "Montgomery"))) + PlotFormat +
  scale_color_gradient(low='grey', high='black')

ggsave(paste(PLOT,".png",sep=""), width = 15, height = 9)


slope21_hd <- slope21_hd %>%
  select(Datetime, Slope_km, R2) %>%
  mutate(year = "2021") %>%
  mutate(side ="hd")

slope21_m <- slope21_m %>%
  select(Datetime, Slope_km, R2) %>%
  mutate(year = "2021") %>%
  mutate(side ="m")

slope_21side <- rbind(slope21_hd, slope21_m)

PLOT = "NSTGE_byside_21"
ggplot(slope_21side, aes(x=Datetime, y=Slope_km)) +
  geom_point(aes(colour=R2)) + 
  ylim(-31,31) +
  geom_hline(yintercept=-6.5, size=1, color="Red") +
  labs(x= "Date", y=expression("NSTGE " (degree*C/km)), color=expression(paste("R"^2))) +
  scale_x_datetime(date_labels = "%b", date_break = "1 month") +
  facet_wrap(~side, scales="free_x", labeller=labeller(side = c("hd" = "Hot Dog", "m" = "Montgomery"))) + PlotFormat +
  scale_color_gradient(low='grey', high='black')

ggsave(paste(PLOT,".png",sep=""), width = 15, height = 9)

slope_side <- rbind(slope_21side, slope_20side)

PLOT = "NSTGE_byside"
ggplot(slope_side, aes(x=Datetime, y=Slope_km)) +
  geom_point(aes(colour=R2)) + 
  geom_hline(aes(yintercept=-6.5, linetype="ELR"), color="Red", size=1) +
  ylim(-31,31) +
  #geom_hline(yintercept=-6.5, size=1, color="Red") +
  labs(x= "Date", y=expression("NSTGE " (degree*C/km)), color=expression(paste("R"^2))) +
  scale_x_datetime(date_labels = "%b", date_break = "1 month") +
  facet_grid(side ~ year, scales="free_x", labeller=labeller(side = c("hd" = "Hot Dog", "m" = "Montgomery"))) +
  scale_color_gradient(low='grey', high='black') + 
  PlotFormat +
  scale_linetype_manual(name ="", values = c('solid'), guide=guide_legend(reverse=FALSE)) +
  geom_hline(aes(yintercept=-0))

ggsave(paste(PLOT,".png",sep=""), width = 15, height = 9)

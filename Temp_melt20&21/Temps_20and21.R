# looking at air temps for melt season 2020 and 2021

library(ggplot2)
library(lubridate)
library(dplyr)
library(plotly)
library(gridExtra)
library(scales)
library(RColorBrewer)
library(tidyverse)

rm(list = ls()) 

#set working directory and csv file
setwd("C:/Users/sears/Documents/Research/Snow_Hydro_Research/Thesis/Data/Air Temp")

T20_slope <- read.csv(file = "C:/Users/sears/Documents/Research/Snow_Hydro_Research/Thesis/Data/Air Temp/T20_nstge.csv",
                      header=TRUE) %>%
  mutate(Datetime = mdy_hm(Datetime))

T21_slope <- read.csv(file = "C:/Users/sears/Documents/Research/Snow_Hydro_Research/Thesis/Data/Air Temp/T21_nstge.csv",
                      header=TRUE) %>%
  mutate(Datetime = mdy_hm(Datetime))

###############################################################################
#FORMATTING FOR PLOTS

PlotFormat = theme(axis.text=element_text(size=20, color="black"),
                   axis.title.x=element_text(size=22, hjust=0.5, margin=margin(t=20, r=20, b=20, l=20), color="black"),              
                   axis.title.y=element_text(size=22, vjust=0.5,  margin=margin(t=20, r=20, b=20, l=20), color="black"),              
                   plot.title=element_text(size=26,face="bold",hjust=0.5, margin=margin(t=20, r=20, b=20, l=20)),      
                   legend.title=element_text(size=18, color="black"),                                                                    
                   legend.text=element_text(size=18, color="black"),                                                                   
                   legend.position = "right", 
                   #panel.grid.major = element_line(colour = "grey80"),
                   #panel.grid.minor = element_line(colour = "grey80"),
                   #panel.grid.major = element_blank(), 
                   #panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   #axis.line = element_line(colour = "black"),
                   strip.text = element_text(size=28),
                   panel.border = element_rect(colour = "black", fill=NA, size=1),
                   legend.key=element_blank())

# a really long way to create minor tick axis tick marks
every_nth <- function(x, nth, empty = TRUE, inverse = FALSE) 
{
  if (!inverse) {
    if(empty) {
      x[1:nth == 1] <- ""
      x
    } else {
      x[1:nth != 1]
    }
  } else {
    if(empty) {
      x[1:nth != 1] <- ""
      x
    } else {
      x[1:nth == 1]
    }
  }
}
######################################################################

#filter 2020 and 2021 to go from May 1 to July 1
T20_slope <- T20_slope %>%
  mutate(year = "2020") %>%
  filter(Datetime > ymd_hms("2020-04-30 23:00:00")) %>%
  filter(Datetime < ymd_hms("2020-07-01 00:00:00"))
  

T21_slope <- T21_slope %>%
  mutate(year = "2021") %>%
  filter(Datetime > ymd_hms("2021-04-30 23:00:00")) %>%
  filter(Datetime < ymd_hms("2021-07-01 00:00:00"))

slope <- rbind(T20_slope, T21_slope)
###############################################################
slope_day <- slope %>%
  mutate(date = as.Date(format(Datetime, "%Y/%m/%d"))) %>%
  group_by(date, year) %>%
  summarize(avgslope = mean(Slope_degCkm),
            avgR2 = mean(R2),
            medslope = median(Slope_degCkm)) %>%
  mutate(avgr2_fix = format(round(avgR2, 2), nsmall=2))

PLOT = "TEG daily_20&21"
ggplot(slope_day, aes(x=date, y=medslope)) +
  geom_hline(aes(yintercept=0), color="black") +
  geom_point(aes(colour=avgR2), size=3) + 
  ylim(-10,10) +
  scale_x_date(date_labels = "%b", breaks="1 month") +
  labs(x= "", y=expression("Daily median TEG " (degree*C/km)), color=expression(paste("R"^2))) +
  #scale_x_date(date_labels = "%b", date_break = "1 month") +
  geom_hline(aes(yintercept=-6.5, linetype="ELR"), color="Red", size=1.25) +
  facet_wrap(~year, scales="free_x") + 
  scale_color_gradient(low='grey', high='black')+
  scale_linetype_manual(name ="", values = c('solid')) +
  PlotFormat +
  theme(panel.spacing = unit(0.5, "cm")) +
  guides(linetype = guide_legend(order=1))


ggsave(paste(PLOT,".png",sep=""), width = 15, height = 9)

#more stats
slope_day_stats <- slope_day %>%
  group_by(year) %>%
  summarize(avgslope = mean(avgslope),
            avgR2 = mean(avgR2),
            medslope = median(medslope))

slope_day_edit <- slope_day %>%
  filter(avgR2 > 0.2)

PLOT = "TEG daily_20&21_r2"
ggplot(slope_day_edit, aes(x=date, y=medslope)) +
  geom_point(aes(colour=avgR2), size=3) + 
  ylim(-10,10) +
  scale_x_date(date_labels = "%b", breaks="1 month") +
  labs(x= "", y=expression("Daily median TEG " (degree*C/km)), color=expression(paste("R"^2))) +
  #scale_x_date(date_labels = "%b", date_break = "1 month") +
  geom_hline(aes(yintercept=-6.5, linetype="ELR"), color="Red", size=1.25) +
  facet_wrap(~year, scales="free_x") + 
  scale_color_gradient(low='grey', high='black')+
  scale_linetype_manual(name ="", values = c('solid')) +
  PlotFormat +
  theme(panel.spacing = unit(0.5, "cm")) +
  guides(linetype = guide_legend(order=1))

ggsave(paste(PLOT,".png",sep=""), width = 15, height = 9)


slope_day_byr2 <- slope_day_edit %>%
  group_by(year) %>%
  summarize(avgslope = mean(avgslope),
            avgR2 = mean(avgR2),
            medslope = median(medslope))

############################################################################
#plotting TEG 2020 and 2021 with R2
PLOT = "TEG_20&21"
ggplot(slope, aes(x=Datetime, y=Slope_degCkm)) +
  geom_point(aes(colour=R2)) + 
  ylim(-25,25) +
  labs(x= "", y=expression("TEG " (degree*C/km)), color=expression(paste("R"^2))) +
  scale_x_datetime(date_labels = "%b", date_break = "1 month") +
  geom_hline(aes(yintercept=-6.5, linetype="ELR"), color="Red", size=1.25) +
  facet_wrap(~year, scales="free_x") + 
  scale_color_gradient(low='grey', high='black')+
  scale_linetype_manual(name ="", values = c('solid')) +
  theme_bw() + PlotFormat

ggsave(paste(PLOT,".png",sep=""), width = 15, height = 9)

#############################################################################

slope_edit <- slope %>%
  mutate(hour = hour(Datetime)) %>%
  mutate(doy = yday(Datetime)) %>%
  filter(Slope_degCkm < 40) %>%
  filter(Slope_degCkm > -32) 

slope_edit$date <- as.Date(format(slope_edit$Datetime, format = "%Y-%m-%d"))

slope_hour <- slope_edit %>%
  group_by(hour) %>%
  summarize(avgslope = mean(Slope_degCkm),
            avgR2 = mean(R2)) %>%
  mutate(sign = ifelse(avgslope < 0, "negative", "positive"))


slope_hr_stats <- slope_hour %>%
  group_by(sign) %>%
  summarize(avgslope_sign = mean(avgslope),
            avgr2sign = mean(avgR2))

PLOT = "TEG hr"
ggplot(slope_hour, aes(x=hour, y=avgslope)) +
  geom_point(aes(colour=avgR2), size=3) + 
  ylim(-10,10) +
  #scale_x_date(date_labels = "%b", breaks="1 month") +
  labs(x= "", y=expression("Average TEG " (degree*C/km)), color=expression(paste("R"^2))) +
  #scale_x_date(date_labels = "%b", date_break = "1 month") +
  geom_hline(aes(yintercept=-6.5, linetype="ELR"), color="Red", size=1.25) +
  scale_color_gradient(low='grey', high='black')+
  scale_linetype_manual(name ="", values = c('solid')) +
  PlotFormat +
  guides(linetype = guide_legend(order=1))

ggsave(paste(PLOT,".png",sep=""), width = 15, height = 9)


PLOT="heatmap_slope"
slopefig <- ggplot(slope_edit, aes(x=date, y=hour, fill=Slope_degCkm)) +
  geom_tile() + facet_grid(~year, scale="free_x") +
  scale_fill_distiller(palette = 'RdYlBu')+
  labs(fill="TEG", x="", y="Hour") + PlotFormat +
  scale_y_continuous(breaks=seq(0, 23, 4)) +
  scale_x_date(date_labels = "%b", date_break = "1 month") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
slopefig
  
ggsave(paste(PLOT,".png",sep=""), width = 15, height = 9)

PLOT = "heatmap_R2"
r2 <- ggplot(slope_edit, aes(x=date, y=hour, fill=R2)) +
  geom_tile() + facet_grid(~year, scale="free_x") +
  labs(fill=expression("R"^2), x="", y="Hour") +
  scale_fill_gradientn(colors=brewer.pal(name="Greys", n=3)) +
  scale_y_continuous(breaks=seq(0, 23, 4)) + PlotFormat +
  scale_x_date(date_labels = "%b", date_break = "1 month") +
  theme(strip.text = element_blank())
r2
  
ggsave(paste(PLOT,".png",sep=""), width = 15, height = 9)

heatmaps <- grid.arrange(slopefig, r2, nrow=2)
ggsave(file="heatmaps.png", heatmaps, width = 15, height = 9)


##############################################################################
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
  mutate(side ="east") %>%
  filter(Datetime > ymd_hms("2020-04-30 23:00:00")) %>%
  filter(Datetime < ymd_hms("2020-07-01 00:00:00"))

slope20_m <- slope20_m %>%
  select(Datetime, Slope_km, R2) %>%
  mutate(year = "2020") %>%
  mutate(side ="west") %>%
  filter(Datetime > ymd_hms("2020-04-30 23:00:00")) %>%
  filter(Datetime < ymd_hms("2020-07-01 00:00:00"))

slope_20side <- rbind(slope20_hd, slope20_m)

PLOT = "TEG_byside_20"
ggplot(slope_20side, aes(x=Datetime, y=Slope_km)) +
  geom_point(aes(colour=R2)) + 
  geom_hline(yintercept=-6.5, size=1, color="Red") +
  ylim(-30,30) +
  labs(x= "Date", y=expression("TEG " (degree*C/km)), color=expression(paste("R"^2))) +
  scale_x_datetime(date_labels = "%b", date_break = "1 month") +
  facet_wrap(~side, scales="free_x") + PlotFormat +
  scale_color_gradient(low='grey', high='black') +
  ylim(-25,25) 

ggsave(paste(PLOT,".png",sep=""), width = 15, height = 9)

slope21_hd <- slope21_hd %>%
  select(Datetime, Slope_km, R2) %>%
  mutate(year = "2021") %>%
  mutate(side ="east") %>%
  filter(Datetime > ymd_hms("2021-04-30 23:00:00")) %>%
  filter(Datetime < ymd_hms("2021-07-01 00:00:00"))

slope21_m <- slope21_m %>%
  select(Datetime, Slope_km, R2) %>%
  mutate(year = "2021") %>%
  mutate(side ="west") %>%
  filter(Datetime > ymd_hms("2021-04-30 23:00:00")) %>%
  filter(Datetime < ymd_hms("2021-07-01 00:00:00"))

slope_21side <- rbind(slope21_hd, slope21_m)

PLOT = "NSTGE_byside_21"
ggplot(slope_21side, aes(x=Datetime, y=Slope_km)) +
  geom_point(aes(colour=R2)) + 
  ylim(-31,31) +
  geom_hline(yintercept=-6.5, size=1, color="Red") +
  labs(x= "Date", y=expression("NSTGE " (degree*C/km)), color=expression(paste("R"^2))) +
  scale_x_datetime(date_labels = "%b", date_break = "1 month") +
  facet_wrap(~side, scales="free_x", labeller=labeller(side = c("east" = "east", "west" = "west"))) + PlotFormat +
  scale_color_gradient(low='grey', high='black')

ggsave(paste(PLOT,".png",sep=""), width = 15, height = 9)

slope_side <- rbind(slope_21side, slope_20side)

PLOT = "NSTGE_byside"
ggplot(slope_side, aes(x=Datetime, y=Slope_km)) +
  geom_point(aes(colour=R2)) + 
  geom_hline(aes(yintercept=-6.5, linetype="ELR"), color="Red", size=1) +
  ylim(-25,25)  +
  #geom_hline(yintercept=-6.5, size=1, color="Red") +
  labs(x= "Date", y=expression("TEG " (degree*C/km)), color=expression(paste("R"^2))) +
  scale_x_datetime(date_labels = "%b", date_break = "1 month") +
  facet_grid(side ~ year, scales="free_x", labeller=labeller(side = c("east" = "east", "west" = "west"))) +
  scale_color_gradient(low='grey', high='black') + 
  PlotFormat +
  scale_linetype_manual(name ="", values = c('solid'), guide=guide_legend(reverse=FALSE))

ggsave(paste(PLOT,".png",sep=""), width = 15, height = 9)

#side stats
sideslope <- rbind(slope_20side, slope_21side)

avg_sides <- sideslope %>%
  group_by(side) %>%
  summarize(avgslope = mean(Slope_km, na.rm=T),
         avgr2 = mean(R2, na.rm=T),
         Ssd = sd(Slope_km, na.rm=T),
         Rsd = sd(R2, na.rm=T))

avg_sides_yr <- sideslope %>%
  group_by(side, year) %>%
  summarize(avgslope = mean(Slope_km, na.rm=T),
            avgr2 = mean(R2, na.rm=T),
            Ssd = sd(Slope_km, na.rm=T),
            Rsd = sd(R2, na.rm=T))

############################################################################
#NOW LOOK AT WIND DATA

#hourly rad data
rad21 <- read.csv(file="C:/Users/sears/Documents/Research/Snow_Hydro_Research/Thesis/Data/Radiation/For R/Rad_melt21.csv", 
                  header=TRUE) %>%
  mutate(Datetime = mdy_hm(Datetime))

uz <- rad21 %>%
  mutate(dt_agg = floor_date(Datetime, unit = "hour")) %>%
  group_by(dt_agg) %>%
  summarize(uz = mean(WS_ms)) %>%
  filter(dt_agg > "2021-04-01 00:00") %>%
  rename(Datetime = dt_agg)

uz21 <- merge(uz, T21_slope, by="Datetime")

uz21_new <- uz21 %>%
  filter(R2 > 0.2) %>%
  mutate(bin=cut_width(uz, width=1, boundary=0)) %>%
  mutate(sign = ifelse(Slope_degCkm > 0, "positive TEG",
                       "negative TEG"))

ggplot(uz21_new, aes(x=uz, y=Slope_degCkm)) +
  geom_point(aes(colour=R2)) + PlotFormat +
  labs(x= "Windspeed (m/s)", y=expression("TEG " (degree*C/km)), 
       color=expression(paste("R"^2))) +
  scale_color_gradient(low='grey', high='black')

ggplot(uz21_new, aes(x=bin, y=Slope_degCkm)) +
  geom_boxplot()+
  facet_grid(sign~.) 

#2021 with day vs night
am_uz21 <- uz21 %>%
  mutate(hour = hour(Datetime)) %>%
  filter(hour %in% (8:18))

ggplot(am_uz21, aes(x=uz, y=Slope_degCkm)) +
  geom_point(aes(colour=R2)) +
  labs(title="am melt 2021")

pm_uz21 <- uz21 %>%
  mutate(hour = hour(Datetime)) %>%
  filter(!hour %in% (8:18))

ggplot(pm_uz21, aes(x=uz, y=Slope_degCkm)) +
  geom_point(aes(colour=R2)) + 
  labs(title="pm melt 2021")

#do with 2020 now
uz_most <- read.csv(file="C:/Users/sears/Documents/Research/Snow_Hydro_Research/Thesis/Data/Radiation/For R/uz_all.csv", 
                    header=TRUE) %>%
  mutate(Datetime = mdy_hm(Datetime))

uz_most <- uz_most %>%
  mutate(dt_agg = floor_date(Datetime, unit = "hour")) %>%
  group_by(dt_agg) %>%
  summarize(uz = mean(WS_ms)) %>%
  rename(Datetime = dt_agg)

uz20 <- merge(uz_most, T20_slope, by="Datetime")

uz20_new <- uz20 %>%
  filter(R2 > 0.20) %>%
  mutate(bin=cut_width(uz, width=1, boundary=0)) %>%
  mutate(sign = ifelse(Slope_degCkm > 0, "positive TEG",
                       "negative TEG"))

ggplot(uz20_new, aes(x=uz, y=Slope_degCkm)) +
  geom_point(aes(colour=R2)) + PlotFormat +
  labs(x= "Windspeed (m/s)", y=expression("TEG " (degree*C/km)), 
       color=expression(paste("R"^2))) +
  scale_color_gradient(low='grey', high='black')

ggplot(uz20, aes(x=uz, y=Slope_degCkm)) +
  geom_point(aes(colour=R2)) + ylim(-20,30)

ggplot(uz20_new, aes(x=bin, y=Slope_degCkm)) +
  geom_boxplot()+
  facet_grid(sign~.)

uzboth <- rbind(uz20, uz21)

uz_new <- rbind(uz21_new, uz20_new)

uz_new <- uz_new %>%
  mutate(year = year(Datetime))

PLOT="uz_boxplot"
ggplot(uz_new, aes(x=bin, y=Slope_degCkm)) +
   geom_boxplot()+
  facet_grid(sign~year) +
  #PlotFormat +
  scale_x_discrete(labels = c("0-1", "1-2", "2-3","3-4", "4-5", "5-6", "6-7", "7-8", "8-9")) +
  PlotFormat+
  labs(x="Windspeed (m/s)", y=expression("TEG " (degree*C/km))) 

ggsave(paste(PLOT,".png",sep=""), width = 15, height = 9)

#get some wind stats
uz_stats_yr <- uz_new %>%
  group_by(bin) %>%
  summarize(count = n(),
            meanTEG = mean(Slope_degCkm),
            sdTEG = sd(Slope_degCkm))

uz_stats_yr <- uz_new %>%
  group_by(bin, year) %>%
  summarize(count = n(),
            meanTEG = mean(Slope_degCkm),
            sdTEG = sd(Slope_degCkm))

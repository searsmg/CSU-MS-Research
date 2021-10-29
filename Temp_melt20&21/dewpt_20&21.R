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

#add a plot format for later
PlotFormat = theme(axis.text=element_text(size=20, color="black"),
                   axis.title.x=element_text(size=22, hjust=0.5, margin=margin(t=20, r=20, b=20, l=20), color="black"),              
                   axis.title.y=element_text(size=22, vjust=0.5,  margin=margin(t=20, r=20, b=20, l=20), color="black"),              
                   plot.title=element_text(size=26,face="bold",hjust=0.5, margin=margin(t=20, r=20, b=20, l=20)),      
                   legend.title=element_text(size=18, color="black"),                                                                    
                   legend.text=element_text(size=18, color="black"),                                                                   
                   legend.position = "right", 
                   #panel.grid.major = element_blank(), 
                   #panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   #axis.line = element_line(colour = "black"),
                   strip.text = element_text(size=28),
                   panel.border = element_rect(colour = "black", fill=NA, size=1),
                   legend.key=element_blank())

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

#write.csv(all21, "melt21.csv")
#write.csv(all20, "melt20.csv")

#get rid of the 01 minutes and make it 00
minute(all20$Datetime) <- 0
minute(all21$Datetime) <- 0

############################################################################
#data are set up - run models now for 2020 (get slope, r2, and p-value)

#slope, r2, deg of free, and pval for 2020 hourly
fit_model <- function(all20) lm(dewpoint ~ Elevation, data = all20) #linear model
get_slope <- function(model) tidy(model)$estimate[2] #pull out the slope
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
get_slope <- function(model) tidy(model)$estimate[2] #pull out the slope
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

##########################################################################

dew20 <- slope20 %>%
  mutate(year = "2020") %>%
  select(-c(data, model, pear))

dew21 <- slope21  %>%
  mutate(year = "2021") %>%
  select(-c(data, model, pear))

slope <- rbind(dew20, dew21)

slope <- slope %>%
  filter(slope_Ckm > -30,
         slope_Ckm < 30)

PLOT = "Dewpt gradient_20&21 by r2"
ggplot(slope, aes(x=Datetime, y=slope_Ckm)) +
  geom_point(aes(colour=r2), size=2) + 
  labs(x= "Date", y=expression("DTEG " (degree*C/km)), color=expression(paste("R"^2))) +
  scale_x_datetime(date_labels = "%b", date_break = "1 month") +
  facet_wrap(~year, scales="free_x") + theme_bw() + PlotFormat +
  scale_color_gradient(low='grey', high='black')+
  scale_linetype_manual(name ="", values = c('solid')) +
  scale_y_continuous(breaks=seq(-30, 30, 10))

ggsave(paste(PLOT,".png",sep=""), width = 15, height = 9)


PLOT = "Dewpt gradient_20&21 by pval"
ggplot(slope, aes(x=Datetime, y=slope_Ckm)) +
  geom_point(aes(colour=cut(pval, c(-Inf, 0.05, Inf))), size=2) + 
  scale_x_datetime(date_labels = "%b", date_break = "1 month") +
  facet_wrap(~year, scales="free_x") + PlotFormat +
  scale_color_manual(name = "pval",
                     values = c("black","gray"),
                     labels = c("S", "NS")) +
  labs(x="", y=expression("Dewpoint Tempeature Gradient " (degree*C/km))) +
  guides(colour=guide_legend(title="p-value"))

ggsave(paste(PLOT,".png",sep=""), width = 15, height = 9)

############################################################################
#plot by time of day - heat maps

slope_edit <- slope %>%
  mutate(hour = hour(Datetime)) %>%
  mutate(doy = yday(Datetime)) 

PLOT="heatmap_slope"
slopefig <- ggplot(slope_edit, aes(x=doy, y=hour, fill=slope_Ckm)) +
  geom_tile() + facet_grid(~year, scale="free_x") +
  scale_fill_distiller(palette = 'RdYlBu')+
  labs(fill=expression(degree*C/km), x="Day of year", y="Hour") + PlotFormat +
  scale_y_continuous(breaks=seq(0, 23, 4))
slopefig

ggsave(paste(PLOT,".png",sep=""), width = 15, height = 9)

PLOT = "heatmap_R2"
r2 <- ggplot(slope_edit, aes(x=doy, y=hour, fill=r2)) +
  geom_tile() + facet_grid(~year, scale="free_x") +
  labs(fill=expression("R"^2), x="Day of year", y="Hour") +
  scale_fill_gradientn(colors=brewer.pal(name="Greys", n=3)) +
  scale_y_continuous(breaks=seq(0, 23, 4)) + PlotFormat
r2

ggsave(paste(PLOT,".png",sep=""), width = 15, height = 9)

PLOT = "heatmap_p"
p <- ggplot(slope_edit, aes(x=doy, y=hour, fill=cut(pval, c(-Inf, 0.05, Inf)))) +
  geom_tile() + facet_grid(~year, scale="free_x") +
  labs(fill="p-value", x="Day of year", y="Hour") +
  #scale_fill_gradientn(colors=brewer.pal(name="Greys", n=5)) +
  scale_y_continuous(breaks=seq(0, 23, 4)) + PlotFormat +
  scale_fill_manual(name = "pval",
                     values = c("black","gray"),
                     labels = c("S", "NS"))
p

ggsave(paste(PLOT,".png",sep=""), width = 15, height = 9)
#############################################################################
#############################################################################
#add in wind real quick
#hourly rad data
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
  facet_grid(year~sign) +
  #PlotFormat +
  scale_x_discrete(labels = c("0-1", "1-2", "2-3","3-4", "4-5", "5-6", "6-7", "7-8", "8-9")) +
  theme_bw() + PlotFormat+
  labs(x="Windspeed (m/s)", y=expression("TEG " (degree*C/km))) 

ggsave(paste(PLOT,".png",sep=""), width = 15, height = 9)














###############################################################################
###############################################################################

#ea analyses
all20 <- all20 %>%
  mutate(ea = (6.112*exp((17.62*dewpoint)/(243.12+dewpoint))))

#data are set up - run models now for 2020 (get slope, r2, and p-value)

#slope, r2, deg of free, and pval for 2020 hourly
fit_model <- function(all20) lm(ea ~ Elevation, data = all20) #linear model
get_slope <- function(model) tidy(model)$estimate[2] #pull out the slope
pearson <- function(all20) cor.test(all20$Elevation, all20$ea, data=all20) #pearson cor test
pval <- function(pear) tidy(pear)$p.value #p value from cor test
df <- function(pear) tidy(pear)$parameter[1] #deg of freedom from cor test

#calculate r2
all20_r <- all20 %>%
  select(-c(ID)) %>%
  group_by(Datetime) %>%
  summarize(r2 = cor(Elevation, ea, use="complete.obs")^2)

#run lm, pull out slope, pearson cor test, pull out pval + deg of free
easlope20 <- all20 %>%
  group_nest(Datetime) %>%
  mutate(model = map(data, fit_model)) %>%
  mutate(slope = map_dbl(model, get_slope)) %>%
  mutate(slope_km = slope*1000) %>%
  mutate(pear = map(data, pearson)) %>%
  mutate(pval = map_dbl(pear, pval)) %>%
  mutate(df = map_dbl(pear, df))

#add derived r2 to slope 20 (with rest of dadta)
easlope20 <- easlope20 %>%
  add_column(r2 = all20_r$r2)

############################################################################
#run models now for 2021 (get slope, r2, and p-value)

all21 <- all21 %>%
  mutate(ea = (6.112*exp((17.62*dewpoint)/(243.12+dewpoint))))

#slope, r2, deg of free, and pval for 2021 hourly
fit_model <- function(all21) lm(ea ~ Elevation, data = all21) #linear model
get_slope <- function(model) tidy(model)$estimate[2] #pull out the slope
pearson <- function(all21) cor.test(all21$Elevation, all21$ea, data=all21) #pearson cor test
pval <- function(pear) tidy(pear)$p.value #p value from cor test
df <- function(pear) tidy(pear)$parameter[1] #deg of freedom from cor test

#calculate r2
all21_r <- all21 %>%
  select(-c(ID)) %>%
  group_by(Datetime) %>%
  summarize(r2 = cor(Elevation, ea, use="complete.obs")^2)

#run lm, pull out slope, pearson cor test, pull out pval + deg of free
easlope21 <- all21 %>%
  group_nest(Datetime) %>%
  mutate(model = map(data, fit_model)) %>%
  mutate(slope = map_dbl(model, get_slope)) %>%
  mutate(slope_km = slope*1000) %>%
  mutate(pear = map(data, pearson)) %>%
  mutate(pval = map_dbl(pear, pval)) %>%
  mutate(df = map_dbl(pear, df))

#add derived r2 to slope 21 (with rest of data)
easlope21 <- easlope21 %>%
  add_column(r2 = all21_r$r2)

##########################################################################

ea20 <- easlope20 %>%
  mutate(year = "2020") %>%
  select(-c(data, model, pear))

ea21 <- easlope21  %>%
  mutate(year = "2021") %>%
  select(-c(data, model, pear))

easlope <- rbind(ea20, ea21)

PLOT = "ea gradient_20&21 by r2"
ggplot(easlope, aes(x=Datetime, y=slope_km)) +
  geom_point(aes(colour=r2), size=2) + 
  labs(x= "Date", y="Elevational vapor pressure", color=expression(paste("R"^2))) +
  scale_x_datetime(date_labels = "%b", date_break = "1 month") +
  facet_wrap(~year, scales="free_x") + PlotFormat +
  scale_color_gradient(low='grey', high='black')+
  scale_linetype_manual(name ="", values = c('solid')) +
  scale_y_continuous(breaks=seq(-100, 50, 10))

ggsave(paste(PLOT,".png",sep=""), width = 15, height = 9)


PLOT = "ea gradient_20&21 by pval"
ggplot(easlope, aes(x=Datetime, y=slope_km)) +
  geom_point(aes(colour=cut(pval, c(-Inf, 0.05, Inf))), size=2) + 
  scale_x_datetime(date_labels = "%b", date_break = "1 month") +
  facet_wrap(~year, scales="free_x") + PlotFormat +
  scale_color_manual(name = "pval",
                     values = c("black","gray"),
                     labels = c("S", "NS")) +
  labs(x="", y="Elevational vapor pressure") +
  guides(colour=guide_legend(title="p-value"))

ggsave(paste(PLOT,".png",sep=""), width = 15, height = 9)

############################################################################
#plot by time of day - heat maps

easlope_edit <- easlope %>%
  mutate(hour = hour(Datetime)) %>%
  mutate(doy = yday(Datetime)) 

PLOT="heatmap_slope_ea"
slopefig <- ggplot(easlope_edit, aes(x=doy, y=hour, fill=slope_km)) +
  geom_tile() + facet_grid(~year, scale="free_x") +
  scale_fill_distiller(palette = 'RdYlBu')+
  labs(fill=expression(degree*C/km), x="Day of year", y="Hour") + PlotFormat +
  scale_y_continuous(breaks=seq(0, 23, 4))
slopefig

ggsave(paste(PLOT,".png",sep=""), width = 15, height = 9)

PLOT = "heatmap_R2_ea"
r2 <- ggplot(easlope_edit, aes(x=doy, y=hour, fill=r2)) +
  geom_tile() + facet_grid(~year, scale="free_x") +
  labs(fill=expression("R"^2), x="Day of year", y="Hour") +
  scale_fill_gradientn(colors=brewer.pal(name="Greys", n=3)) +
  scale_y_continuous(breaks=seq(0, 23, 4)) + PlotFormat
r2

ggsave(paste(PLOT,".png",sep=""), width = 15, height = 9)

PLOT = "heatmap_p_ea"
p <- ggplot(easlope_edit, aes(x=doy, y=hour, fill=cut(pval, c(-Inf, 0.05, Inf)))) +
  geom_tile() + facet_grid(~year, scale="free_x") +
  labs(fill="p-value", x="Day of year", y="Hour") +
  #scale_fill_gradientn(colors=brewer.pal(name="Greys", n=5)) +
  scale_y_continuous(breaks=seq(0, 23, 4)) + PlotFormat +
  scale_fill_manual(name = "pval",
                    values = c("black","gray"),
                    labels = c("S", "NS"))
p

ggsave(paste(PLOT,".png",sep=""), width = 15, height = 9)

##########################################################################
all20 <- all20 %>%
  mutate(year = "2020")

all21 <- all21 %>%
  mutate(year = "2021")

all <- rbind(all20, all21)

allmp4 <- all %>%
  filter(ID == "MP4")

PLOT = "Ta vs Tdew for mp4"
ggplot(allmp4, aes(x=AirT_C, y=dewpoint)) +
  geom_point() +
  geom_abline(slope = 1, size=1, color="red") + 
  facet_wrap(~year, scales="free_x")

ggsave(paste(PLOT,".png",sep=""), width = 15, height = 9)

alledit <- all %>%
  filter(dewpoint > -20)

PLOT = "Ta vs Tdew for all"
ggplot(alledit, aes(x=AirT_C, y=dewpoint)) +
  geom_point() +
  geom_abline(slope = 1, size=1, color="red") + 
  facet_wrap(~year, scales="free_x")

ggsave(paste(PLOT,".png",sep=""), width = 15, height = 9)

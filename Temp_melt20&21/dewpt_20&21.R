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


all_mayjun21 <- all21 %>%
  filter(Datetime > ymd_hms("2021-04-30 23:00:00")) %>%
  filter(Datetime < ymd_hms("2021-07-01 00:00:00"))

td <- ggplot(all_mayjun21) +
  geom_line(aes(x=Datetime, y=dewpoint, color=ID))

ggplotly(td)
all21 <- all21 %>%
  filter(!str_detect(ID, "MP9")) %>%
  filter(!str_detect(ID, "MP8")) %>%
  filter(!str_detect(ID, "HD9")) %>%
  filter(!str_detect(ID, "HD8"))

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
  select(-c(data, model, pear)) %>%
  filter(Datetime > ymd_hms("2020-04-30 23:00:00")) %>%
  filter(Datetime < ymd_hms("2020-07-01 00:00:00"))

dew21 <- slope21  %>%
  mutate(year = "2021") %>%
  select(-c(data, model, pear)) %>%
  filter(Datetime > ymd_hms("2021-04-30 23:00:00")) %>%
  filter(Datetime < ymd_hms("2021-07-01 00:00:00"))

slope <- rbind(dew20, dew21)

slope_stat <- slope %>%
  summarize(slope = mean(slope_Ckm, na.rm=T),
         slopesd = sd(slope_Ckm, na.rm=T),
         r2 = mean(r2, na.rm=T),
         r2stdev = sd(r2, na.rm=T))

slope_stat_yr <- slope %>%
  group_by(year) %>%
  summarize(slope = mean(slope_Ckm, na.rm=T),
            slopesd = sd(slope_Ckm, na.rm=T),
            r2 = mean(r2, na.rm=T),
            r2stdev = sd(r2, na.rm=T))

slope <- slope %>%
  filter(slope_Ckm > -30,
         slope_Ckm < 30)

slope_day <- slope %>%
  mutate(date = as.Date(format(Datetime, "%Y/%m/%d"))) %>%
  group_by(date, year) %>%
  summarize(avgslope = mean(slope_Ckm),
            avgR2 = mean(r2),
            medslope = median(slope_Ckm),
            meanp = mean(pval)) %>%
  filter(avgR2 >0.2)



PLOT = "DTEG daily_20&21_byr2"
ggplot(slope_day, aes(x=date, y=medslope)) +
  geom_point(aes(colour=avgR2), size=3) + 
  ylim(-10,10) +
  scale_x_date(date_labels = "%b", breaks="1 month") +
  labs(x= "", y=expression("Daily median DTEG " (degree*C/km)), color=expression(paste("R"^2))) +
  #scale_x_date(date_labels = "%b", date_break = "1 month") +
  geom_hline(aes(yintercept=-5.1, linetype="Kunkel (1989)"), color="Red", size=1.25) +
  facet_wrap(~year, scales="free_x") + 
  scale_color_gradient(low='grey', high='black')+
  scale_linetype_manual(name ="", values = c('solid')) +
  PlotFormat +
  theme(panel.spacing = unit(0.5, "cm")) +
  guides(linetype = guide_legend(order=1))

ggsave(paste(PLOT,".png",sep=""), width = 15, height = 9)

PLOT = "DTEG daily_20&21_byp"
ggplot(slope_day, aes(x=date, y=medslope)) +
  geom_point(aes(colour=cut(meanp, c(-Inf, 0.05, Inf))), size=2) + 
  scale_x_date(date_labels = "%b", breaks="1 month") +
  facet_wrap(~year, scales="free_x") + PlotFormat +
  scale_color_manual(name = "pval",
                     values = c("black","gray"),
                     labels = c("S", "NS")) +
  labs(x="", y=expression("Daily DTEG " (degree*C/km))) +
  guides(colour=guide_legend(title="p-value"))


ggsave(paste(PLOT,".png",sep=""), width = 15, height = 9)


#####################################################################
PLOT = "Dewpt gradient_20&21 by r2"
ggplot(slope, aes(x=Datetime, y=slope_Ckm)) +
  geom_point(aes(colour=r2), size=2) + 
  geom_hline(aes(yintercept=-5.1, linetype="Kunkel 1989"), color="Red", size=1) +
  labs(x= "Date", y=expression("DTEG " (degree*C/km)), color=expression(paste("R"^2))) +
  scale_x_datetime(date_labels = "%b", date_break = "1 month") +
  facet_wrap(~year, scales="free_x") + PlotFormat +
  scale_color_gradient(low='grey', high='black')+
  scale_linetype_manual(name ="", values = c('solid')) +
  scale_y_continuous(breaks=seq(-30, 30, 10)) +
  guides(linetype = guide_legend(order=1))
  
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

slope_edit$date <- as.Date(format(slope_edit$Datetime, format = "%Y-%m-%d"))


PLOT="heatmap_slope"
slopefig <- ggplot(slope_edit, aes(x=date, y=hour, fill=slope_Ckm)) +
  geom_tile() + facet_grid(~year, scale="free_x") +
  scale_fill_distiller(palette = 'RdYlBu')+
  labs(fill="DTEG", x="Day of year", y="Hour") + PlotFormat +
  scale_x_date(date_labels = "%b", date_break = "1 month") +
  scale_y_continuous(breaks=seq(0, 23, 4)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
slopefig

ggsave(paste(PLOT,".png",sep=""), width = 15, height = 9)

PLOT = "heatmap_R2"
r2 <- ggplot(slope_edit, aes(x=date, y=hour, fill=r2)) +
  geom_tile() + facet_grid(~year, scale="free_x") +
  scale_x_date(date_labels = "%b", date_break = "1 month") +
  labs(fill=expression("R"^2), x="Day of year", y="Hour") +
  scale_fill_gradientn(colors=brewer.pal(name="Greys", n=3)) +
  scale_y_continuous(breaks=seq(0, 23, 4)) + PlotFormat +
  theme(strip.text = element_blank())
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

uz21 <- merge(uz, dew21, by="Datetime")

uz21_new <- uz21 %>%
  filter(r2 > 0.2) %>%
  mutate(bin=cut_width(uz, width=1, boundary=0)) %>%
  mutate(sign = ifelse(slope_Ckm > 0, "positive DTEG",
                       "negative DTEG"))

ggplot(uz21_new, aes(x=uz, y=slope_Ckm)) +
  geom_point(aes(colour=r2)) + PlotFormat +
  labs(x= "Windspeed (m/s)", y=expression("DTEG " (degree*C/km)), 
       color=expression(paste("R"^2))) +
  scale_color_gradient(low='grey', high='black')

ggplot(uz21_new, aes(x=bin, y=slope_Ckm)) +
  geom_boxplot()+
  facet_grid(sign~.) 

#do with 2020 now
uz_most <- read.csv(file="C:/Users/sears/Documents/Research/Snow_Hydro_Research/Thesis/Data/Radiation/For R/uz_all.csv", 
                    header=TRUE) %>%
  mutate(Datetime = mdy_hm(Datetime))

uz_most <- uz_most %>%
  mutate(dt_agg = floor_date(Datetime, unit = "hour")) %>%
  group_by(dt_agg) %>%
  summarize(uz = mean(WS_ms)) %>%
  rename(Datetime = dt_agg)

uz20 <- merge(uz_most, dew20, by="Datetime")

uz20_new <- uz20 %>%
  filter(r2 > 0.20) %>%
  mutate(bin=cut_width(uz, width=1, boundary=0)) %>%
  mutate(sign = ifelse(slope_Ckm > 0, "positive DTEG",
                       "negative DTEG"))

ggplot(uz20_new, aes(x=uz, y=slope_Ckm)) +
  geom_point(aes(colour=r2)) + PlotFormat +
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
  mutate(year = year(Datetime)) %>%
  filter(slope_Ckm > -30,
         slope_Ckm < 30)

PLOT="uz_boxplot_dew"
ggplot(uz_new, aes(x=bin, y=slope_Ckm)) +
  geom_boxplot()+
  facet_grid(sign~year) +
  #PlotFormat +
  scale_x_discrete(labels = c("0-1", "1-2", "2-3","3-4", "4-5", "5-6", "6-7", "7-8", "8-9")) +
  PlotFormat+
  labs(x="Windspeed (m/s)", y=expression("DTEG " (degree*C/km))) 

ggsave(paste(PLOT,".png",sep=""), width = 15, height = 9)

#get some wind stats
#get some wind stats
uz_stats_yr <- uz_new %>%
  group_by(bin) %>%
  summarize(count = n(),
            meanDTEG = mean(slope_Ckm),
            sdDTEG = sd(slope_Ckm))

uz_stats_sign <- uz_new %>%
  group_by(bin, sign) %>%
  summarize(count = n(),
            meanDTEG = mean(slope_Ckm),
            sdDTEG = sd(slope_Ckm))



###############################################################################
###############################################################################
#need to look at dewpoint temp by transect

all20$side = substr(all20$ID,1,2)

#2020 HD
dew20_hd <- all20 %>%
  filter(side == "HD") %>%
  group_by(Datetime) %>%
  mutate(NAcount = sum(is.na(dewpoint))) %>% 
  filter(!any(NAcount > 2)) %>%
  filter(Datetime > ymd_hms("2020-05-1 11:00:00")) %>%
  filter(Datetime < ymd_hms("2020-07-01 00:00:00"))
  

#slope and r2 for 2020 daily - HD
fit_model <- function(dew20_hd) lm(dewpoint ~ Elevation, data = dew20_hd)
get_slope <- function(mod) tidy(mod)$estimate[2]
pearson <- function(dew20_hd) cor.test(dew20_hd$Elevation, dew20_hd$dewpoint, data=dew20_hd) #pearson cor test
pval <- function(pear) tidy(pear)$p.value #p value from cor test
df <- function(pear) tidy(pear)$parameter[1] #deg of freedom from cor test

dew20_r_hd <- dew20_hd %>%
  select(-c(ID)) %>%
  group_by(Datetime) %>%
  summarize(r2 = cor(Elevation, dewpoint, use="complete.obs")^2)

slope20_hd <- dew20_hd %>%
  group_nest(Datetime) %>%
  mutate(model = map(data, fit_model)) %>%
  mutate(slope = map_dbl(model, get_slope)) %>%
  mutate(slope_Ckm = slope*1000) %>%
  mutate(pear = map(data, pearson)) %>%
  mutate(pval = map_dbl(pear, pval)) %>%
  mutate(df = map_dbl(pear, df))

slope20_hd <- slope20_hd %>%
  add_column(r2 = dew20_r_hd$r2) %>%
  mutate(side="east")

#2020 MP
dew20_mp <- all20 %>%
  filter(side == "MP") %>%
  group_by(Datetime) %>%
  mutate(NAcount = sum(is.na(dewpoint))) %>% 
  filter(!any(NAcount > 2)) %>%
  filter(Datetime > ymd_hms("2020-05-01 00:00:00")) %>%
  filter(Datetime < ymd_hms("2020-07-01 00:00:00"))


#slope and r2 for 2020 daily - MP
fit_model <- function(dew20_mp) lm(dewpoint ~ Elevation, data = dew20_mp)
get_slope <- function(mod) tidy(mod)$estimate[2]
pearson <- function(dew20_mp) cor.test(dew20_mp$Elevation, dew20_mp$dewpoint, data=dew20_mp) #pearson cor test
pval <- function(pear) tidy(pear)$p.value #p value from cor test
df <- function(pear) tidy(pear)$parameter[1] #deg of freedom from cor test

dew20_r_mp <- dew20_mp %>%
  select(-c(ID)) %>%
  group_by(Datetime) %>%
  summarize(r2 = cor(Elevation, dewpoint, use="complete.obs")^2)

slope20_mp <- dew20_mp %>%
  group_nest(Datetime) %>%
  mutate(model = map(data, fit_model)) %>%
  mutate(slope = map_dbl(model, get_slope)) %>%
  mutate(slope_Ckm = slope*1000) %>%
  mutate(pear = map(data, pearson)) %>%
  mutate(pval = map_dbl(pear, pval)) %>%
  mutate(df = map_dbl(pear, df))

slope20_mp <- slope20_mp %>%
  add_column(r2 = dew20_r_mp$r2) %>%
  mutate(side="west")

slope20_mp <- slope20_mp %>%
  select(-c(data, model))

slope20_hd <- slope20_hd %>%
  select(-c(data, model))

slope20_side <- rbind(slope20_hd, slope20_mp)

#####
#now do transect for 2021

all21$side = substr(all21$ID,1,2)

#2021 HD
dew21_hd <- all21 %>%
  filter(side == "HD") %>%
  group_by(Datetime) %>%
  mutate(NAcount = sum(is.na(dewpoint))) %>% 
  filter(!any(NAcount > 2)) %>%
  filter(Datetime > ymd_hms("2021-05-1 11:00:00")) %>%
  filter(Datetime < ymd_hms("2021-07-01 00:00:00"))


#slope and r2 for 2020 daily - HD
fit_model <- function(dew21_hd) lm(dewpoint ~ Elevation, data = dew21_hd)
get_slope <- function(mod) tidy(mod)$estimate[2]
pearson <- function(dew21_hd) cor.test(dew21_hd$Elevation, dew21_hd$dewpoint, data=dew21_hd) #pearson cor test
pval <- function(pear) tidy(pear)$p.value #p value from cor test
df <- function(pear) tidy(pear)$parameter[1] #deg of freedom from cor test

dew21_r_hd <- dew21_hd %>%
  select(-c(ID)) %>%
  group_by(Datetime) %>%
  summarize(r2 = cor(Elevation, dewpoint, use="complete.obs")^2)

slope21_hd <- dew21_hd %>%
  group_nest(Datetime) %>%
  mutate(model = map(data, fit_model)) %>%
  mutate(slope = map_dbl(model, get_slope)) %>%
  mutate(slope_Ckm = slope*1000) %>%
  mutate(pear = map(data, pearson)) %>%
  mutate(pval = map_dbl(pear, pval)) %>%
  mutate(df = map_dbl(pear, df))

slope21_hd <- slope21_hd %>%
  add_column(r2 = dew21_r_hd$r2) %>%
  mutate(side="east")

#2021 MP
dew21_mp <- all21 %>%
  filter(side == "MP") %>%
  group_by(Datetime) %>%
  mutate(NAcount = sum(is.na(dewpoint))) %>% 
  filter(!any(NAcount > 2)) %>%
  filter(Datetime > ymd_hms("2021-05-01 00:00:00")) %>%
  filter(Datetime < ymd_hms("2021-07-01 00:00:00"))


#slope and r2 for 2020 daily - MP
fit_model <- function(dew21_mp) lm(dewpoint ~ Elevation, data = dew21_mp)
get_slope <- function(mod) tidy(mod)$estimate[2]
pearson <- function(dew21_mp) cor.test(dew21_mp$Elevation, dew21_mp$dewpoint, data=dew21_mp) #pearson cor test
pval <- function(pear) tidy(pear)$p.value #p value from cor test
df <- function(pear) tidy(pear)$parameter[1] #deg of freedom from cor test

dew21_r_mp <- dew21_mp %>%
  select(-c(ID)) %>%
  group_by(Datetime) %>%
  summarize(r2 = cor(Elevation, dewpoint, use="complete.obs")^2)

slope21_mp <- dew21_mp %>%
  group_nest(Datetime) %>%
  mutate(model = map(data, fit_model)) %>%
  mutate(slope = map_dbl(model, get_slope)) %>%
  mutate(slope_Ckm = slope*1000) %>%
  mutate(pear = map(data, pearson)) %>%
  mutate(pval = map_dbl(pear, pval)) %>%
  mutate(df = map_dbl(pear, df))

slope21_mp <- slope21_mp %>%
  add_column(r2 = dew21_r_mp$r2) %>%
  mutate(side="west")

slope21_mp <- slope21_mp %>%
  select(-c(data, model))

slope21_hd <- slope21_hd %>%
  select(-c(data, model))

slope21_side <- rbind(slope21_hd, slope21_mp)

slope21_side <- slope21_side %>%
  mutate(year = 2021)

slope20_side <- slope20_side %>%
  mutate(year = 2020)

slope_allside <- rbind(slope20_side, slope21_side)

slope_stat_side <- slope_allside %>%
  group_by(side) %>%
  summarize(avgslope = mean(slope_Ckm, na.rm=T),
            avgr2 = mean(r2, na.rm=T),
            Ssd = sd(slope_Ckm, na.rm=T),
            Rsd = sd(r2, na.rm=T))

slope_stat_side_yr <- slope_allside %>%
  group_by(side, year) %>%
  summarize(avgslope = mean(slope_Ckm, na.rm=T),
            avgr2 = mean(r2, na.rm=T),
            Ssd = sd(slope_Ckm, na.rm=T),
            Rsd = sd(r2, na.rm=T))


slope_allside <- slope_allside %>%
filter(slope_Ckm > -30,
       slope_Ckm < 30)


PLOT = "dTEG_byside"
ggplot(slope_allside, aes(x=Datetime, y=slope_Ckm)) +
  geom_point(aes(colour=r2)) + 
  geom_hline(aes(yintercept=-5.1, linetype="Kunkel 1989"), color="Red", size=1) +
  #geom_hline(yintercept=-6.5, size=1, color="Red") +
  labs(x= "Date", y=expression("DTEG " (degree*C/km)), color=expression(paste("R"^2))) +
  scale_x_datetime(date_labels = "%b", date_break = "1 month") +
  facet_grid(side ~ year, scales="free_x", labeller=labeller(side = c("east" = "east", "west" = "west"))) +
  scale_color_gradient(low='grey', high='black') + 
  PlotFormat +
  scale_linetype_manual(name ="", values = c('solid'), guide=guide_legend(reverse=FALSE)) +
  guides(linetype = guide_legend(order=1))

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

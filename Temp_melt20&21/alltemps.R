#all Ta and Td (looking at seasons)

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

##setup
setwd("C:/Users/sears/Documents/Research/Snow_Hydro_Research/Thesis/Data/Air Temp/For R")

PlotFormat = theme(axis.text=element_text(size=16, color="black"),
                   axis.title.x=element_text(size=18, hjust=0.5, margin=margin(t=20, r=20, b=20, l=20), color="black"),              
                   axis.title.y=element_text(size=18, vjust=0.5,  margin=margin(t=20, r=20, b=20, l=20), color="black"),              
                   plot.title=element_text(size=26,face="bold",hjust=0.5, margin=margin(t=20, r=20, b=20, l=20)),      
                   legend.title=element_text(size=14, color="black"),                                                                    
                   legend.text=element_text(size=14, color="black"),                                                                   
                   legend.position = "right", 
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   #axis.line = element_line(colour = "black"),
                   strip.text = element_text(size=25),
                   panel.border = element_rect(colour = "black", fill=NA, size=1))

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

##pre-processing for Ta and Td data

all <- read.csv("FINAL_BMdatabase_20210808.csv") %>%
  mutate(Datetime = mdy_hm(Datetime))

#get rid of the 01 minutes and make it 00
minute(all$Datetime) <- 0

all <- all[-c(1,2), ]

############################################################################
#data are set up - run models now for Ta (get slope, r2, and p-value)

#slope, r2, deg of free, and pval for 2020 hourly
fit_model <- function(all) lm(temperature ~ Elevation, data = all) #linear model
get_slope <- function(model) tidy(model)$estimate[2] #pull out the slope
pearson <- function(all) cor.test(all$Elevation, all$temperature, data=all) #pearson cor test
pval <- function(pear) tidy(pear)$p.value #p value from cor test
df <- function(pear) tidy(pear)$parameter[1] #deg of freedom from cor test

#calculate r2
allta_r <- all %>%
  select(-c(ID)) %>%
  group_by(Datetime) %>%
  summarize(r2 = cor(Elevation, temperature, use="complete.obs")^2)

#run lm, pull out slope, pearson cor test, pull out pval + deg of free
slope_ta <- all %>%
  group_nest(Datetime) %>%
  mutate(model = map(data, fit_model)) %>%
  mutate(slope = map_dbl(model, get_slope)) %>%
  mutate(slope_Ckm = slope*1000) %>%
  mutate(pear = map(data, pearson)) %>%
  mutate(pval = map_dbl(pear, pval)) %>%
  mutate(df = map_dbl(pear, df))

#add derived r2 to rest of data
slope_ta <- slope_ta %>%
  add_column(r2 = allta_r$r2)

###########################################################################

PLOT = "Ta gradient by r2_allseasons"
ggplot(slope_ta, aes(x=Datetime, y=slope_Ckm)) +
  geom_point(aes(colour=r2), size=2) + 
  labs(x= "Date", y=expression("Air Temperature Gradient " (degree*C/km)), color=expression(paste("R"^2))) +
  scale_x_datetime(date_labels = "%b-%y", date_break = "2 months") +
  PlotFormat +
  geom_hline(aes(yintercept=-6.5, linetype="ELR"), color="Red", size=1) +
  scale_color_gradient(low='grey', high='black')+
  scale_linetype_manual(name ="", values = c('solid')) +
  scale_y_continuous(breaks=seq(-100, 100, 10)) 
  
ggsave(paste(PLOT,".png",sep=""), width = 15, height = 9)


PLOT = "Ta gradient by pval_allseasons"
ggplot(slope_ta, aes(x=Datetime, y=slope_Ckm)) +
  geom_point(aes(colour=cut(pval, c(-Inf, 0.05, Inf))), size=2) + 
  scale_x_datetime(date_labels = "%b-%y", date_break = "2 months") +
  PlotFormat +
  scale_color_manual(name = "pval",
                     values = c("black","gray"),
                     labels = c("S", "NS")) +
  labs(x="", y=expression("Air Temperature Gradient " (degree*C/km))) +
  guides(colour=guide_legend(title="p-value")) +
  scale_y_continuous(breaks=seq(-100, 100, 10)) +
  geom_hline(aes(yintercept=-6.5, linetype="ELR"), color="Red", size=1) +
  scale_linetype_manual(name ="", values = c('solid'))

ggsave(paste(PLOT,".png",sep=""), width = 15, height = 9)

###############################################################################
#now do for Td

#datd are set up - run models now for td (get slope, r2, and p-value)

#slope, r2, deg of free, and pval for 2020 hourly
fit_model <- function(all) lm(dewpoint ~ Elevation, data = all) #linear model
get_slope <- function(model) tidy(model)$estimate[2] #pull out the slope
pearson <- function(all) cor.test(all$Elevation, all$dewpoint, data=all) #pearson cor test
pval <- function(pear) tidy(pear)$p.value #p value from cor test
df <- function(pear) tidy(pear)$parameter[1] #deg of freedom from cor test

#calculate r2
alltd_r <- all %>%
  select(-c(ID)) %>%
  group_by(Datetime) %>%
  summarize(r2 = cor(Elevation, dewpoint, use="complete.obs")^2)

#run lm, pull out slope, pearson cor test, pull out pval + deg of free
slope_td <- all %>%
  group_nest(Datetime) %>%
  mutate(model = map(data, fit_model)) %>%
  mutate(slope = map_dbl(model, get_slope)) %>%
  mutate(slope_Ckm = slope*1000) %>%
  mutate(pear = map(data, pearson)) %>%
  mutate(pval = map_dbl(pear, pval)) %>%
  mutate(df = map_dbl(pear, df))

#add derived r2 to rest of data
slope_td <- slope_td %>%
  add_column(r2 = alltd_r$r2)
#############################################################################
#Td plots

PLOT = "Td gradient by r2_allseasons"
ggplot(slope_td, aes(x=Datetime, y=slope_Ckm)) +
  geom_point(aes(colour=r2), size=2) + 
  labs(x= "Date", y=expression("Dewpoint Temperature Gradient " (degree*C/km)), color=expression(paste("R"^2))) +
  scale_x_datetime(date_labels = "%b-%y", date_break = "2 months") +
  PlotFormat +
  geom_hline(aes(yintercept=-5.1, linetype="L&E 2006"), color="Red", size=1) +
  scale_color_gradient(low='grey', high='black')+
  scale_linetype_manual(name ="", values = c('solid')) +
  scale_y_continuous(breaks=seq(-100, 100, 10)) 

ggsave(paste(PLOT,".png",sep=""), width = 15, height = 9)


PLOT = "Td gradient by pval_allseasons"
ggplot(slope_td, aes(x=Datetime, y=slope_Ckm)) +
  geom_point(aes(colour=cut(pval, c(-Inf, 0.05, Inf))), size=2) + 
  scale_x_datetime(date_labels = "%b-%y", date_break = "2 months") +
  PlotFormat +
  scale_color_manual(name = "pval",
                     values = c("black","gray"),
                     labels = c("S", "NS")) +
  labs(x="", y=expression("Dewpoint Temperature Gradient " (degree*C/km))) +
  guides(colour=guide_legend(title="p-value")) +
  scale_y_continuous(breaks=seq(-100, 100, 10)) +
  geom_hline(aes(yintercept=-5.1, linetype="L&E 2006"), color="Red", size=1) +
  scale_linetype_manual(name ="", values = c('solid'))

ggsave(paste(PLOT,".png",sep=""), width = 15, height = 9)

##############################################################################
#now split by snow season (peakl to SAG, SAG to start, start to peak, peak to SAG)

#assigning a 'season' to Ta gradients
ta_season <- slope_ta %>%
  select(-c(data, model, pear)) %>%
  mutate(doy = decimal_date(Datetime)) %>%
  mutate(season = case_when(
    between(doy, 2020.3306010929, 2020.4971539162) ~ "peak to SAG 2020",
    between(doy, 2020.4972677596, 2020.7485200364) ~ "SAG to start 2020",
    between(doy, 2020.7486338798, 2021.3286529680) ~ "start to peak 2020-2021",
    between(doy, 2021.3287671233, 2021.4958904110) ~ "peak to SAG 2021")) %>%
  drop_na()

#assigning a 'season' to Td gradients
td_season <- slope_td %>%
  select(-c(data, model, pear)) %>%
  mutate(doy = decimal_date(Datetime)) %>%
  mutate(season = case_when(
    between(doy, 2020.3306010929, 2020.4971539162) ~ "peak to SAG 2020",
    between(doy, 2020.4972677596, 2020.7485200364) ~ "SAG to start 2020",
    between(doy, 2020.7486338798, 2021.3286529680) ~ "start to peak 2020-2021",
    between(doy, 2021.3287671233, 2021.4958904110) ~ "peak to SAG 2021")) %>%
  drop_na()

#factor so they're in chrono order
ta_season$season <- factor(ta_season$season, levels =c("peak to SAG 2020", "SAG to start 2020", 
                                                       "start to peak 2020-2021","peak to SAG 2021"))
#plot Ta and Td gradients by season
PLOT = "Ta gradient_season by r2"
ggplot(ta_season, aes(x=Datetime, y=slope_Ckm)) +
  geom_point(aes(colour=r2), size=2) + 
  labs(x= "Date", y=expression("Air temperature gradient " (degree*C/km)), color=expression(paste("R"^2))) +
  scale_x_datetime(date_labels = "%b", date_break = "1 month") +
  facet_wrap(~factor(season) ,scales ="free_x") + 
  scale_color_gradient(low='grey', high='black')+
  scale_linetype_manual(name ="", values = c('solid')) +
  #scale_y_continuous(breaks=seq(-100, 50, 10)) +
  PlotFormat 

ggsave(paste(PLOT,".png",sep=""), width = 15, height = 9)

PLOT = "Ta gradient_season by p"
ggplot(ta_season, aes(x=Datetime, y=slope_Ckm)) +
  geom_point(aes(colour=cut(pval, c(-Inf, 0.05, Inf))), size=2) + 
  scale_x_datetime(date_labels = "%b", date_break = "1 month") +
  scale_color_manual(name = "pval",
                     values = c("black","gray"),
                     labels = c("S", "NS")) +
  labs(x="", y=expression("Air Temperature Gradient " (degree*C/km))) +
  guides(colour=guide_legend(title="p-value")) +
  #scale_y_continuous(breaks=seq(-100, 100, 10)) +
  #geom_hline(aes(yintercept=-5.1, linetype="L&E 2006"), color="Red", size=1) +
  #scale_linetype_manual(name ="", values = c('solid')) +
  facet_wrap(~factor(season), scales = "free_x") +
  PlotFormat

ggsave(paste(PLOT,".png",sep=""), width = 15, height = 9)

PLOT = "Td gradient_season by r2"
ggplot(td_season, aes(x=Datetime, y=slope_Ckm)) +
  geom_point(aes(colour=r2), size=2) + 
  labs(x= "Date", y=expression("Dewpoint temperature gradient " (degree*C/km)), color=expression(paste("R"^2))) +
  scale_x_datetime(date_labels = "%b", date_break = "1 month") +
  facet_wrap(~factor(season) ,scales ="free_x") + 
  scale_color_gradient(low='grey', high='black')+
  scale_linetype_manual(name ="", values = c('solid')) +
  #scale_y_continuous(breaks=seq(-100, 50, 10)) +
  PlotFormat 

ggsave(paste(PLOT,".png",sep=""), width = 15, height = 9)

PLOT = "Td gradient_season by p"
ggplot(td_season, aes(x=Datetime, y=slope_Ckm)) +
  geom_point(aes(colour=cut(pval, c(-Inf, 0.05, Inf))), size=2) + 
  scale_x_datetime(date_labels = "%b", date_break = "1 month") +
  scale_color_manual(name = "pval",
                     values = c("black","gray"),
                     labels = c("S", "NS")) +
  labs(x="", y=expression("Dewpoint Temperature Gradient " (degree*C/km))) +
  guides(colour=guide_legend(title="p-value")) +
  #scale_y_continuous(breaks=seq(-100, 100, 10)) +
  #geom_hline(aes(yintercept=-5.1, linetype="L&E 2006"), color="Red", size=1) +
  #scale_linetype_manual(name ="", values = c('solid')) +
  facet_wrap(~factor(season), scales = "free_x") +
  PlotFormat

ggsave(paste(PLOT,".png",sep=""), width = 15, height = 9)

############################################################################
#compare wind data to ALL Ta and Td data

#bring in wind data that is stored with rad data
rad21 <- read.csv(file="C:/Users/sears/Documents/Research/Snow_Hydro_Research/Thesis/Data/Radiation/For R/Rad_melt21.csv", 
                  header=TRUE) %>%
  mutate(Datetime = mdy_hm(Datetime))

uz <- rad21 %>%
  mutate(dt_agg = floor_date(Datetime, unit = "hour")) %>%
  group_by(dt_agg) %>%
  summarize(uz = mean(WS_ms)) %>%
  rename(Datetime = dt_agg)

#ta vs uz
uz_ta <- merge(uz, ta_season, by="Datetime")

ggplot(uz_ta21, aes(x=uz, y=slope_Ckm)) +
  geom_point(aes(colour=r2)) +
  labs(y="Ta gradient", x="windspeed (m/s)", title="all Ta vs. Uz")

#td vs uz
uz_td <- merge(uz, td_season, by="Datetime")

ggplot(uz_td21, aes(x=uz, y=slope_Ckm)) +
  geom_point(aes(colour=r2)) +
  labs(y="Td gradient", x="windspeed (m/s)", title="all Td vs. Uz")

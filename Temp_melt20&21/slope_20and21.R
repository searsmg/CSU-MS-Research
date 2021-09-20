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
library(plotly)
library(tidyr)
library(tibble)

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
#2020 slope - hourly
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
#2021 slope - hourly
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
#take T20 and T21 (original formats) and average by day and sensor
daily20 <- T20 %>%
  group_by(daily = format(Datetime, "%Y-%m-%d"), ID, Elevation) %>%
  summarize(Ta = mean(AirT_C,na.rm=T)) %>% 
  ungroup()

daily21 <- T21 %>%
  group_by(daily = format(Datetime, "%Y-%m-%d"), ID, Elevation) %>%
  summarize(Ta = mean(AirT_C, na.rm=T)) %>%
  ungroup()

#########################################################################
#slope and r2 for 2020 daily
fit_model <- function(daily20) lm(Ta ~ Elevation, data = daily20)
get_slope <- function(mod) tidy(mod)$estimate[2]
pearson <- function(daily20) cor.test(daily20$Elevation, daily20$Ta, data=daily20) #pearson cor test
pval <- function(pear) tidy(pear)$p.value #p value from cor test
df <- function(pear) tidy(pear)$parameter[1] #deg of freedom from cor test

T20_r_daily <- daily20 %>%
  select(-c(ID)) %>%
  group_by(daily) %>%
  summarize(r2 = cor(Elevation, Ta, use="complete.obs")^2)

slope20_d <- daily20 %>%
  group_nest(daily) %>%
  mutate(model = map(data, fit_model)) %>%
  mutate(slope = map_dbl(model, get_slope)) %>%
  mutate(slope_Ckm = slope*1000) %>%
  mutate(pear = map(data, pearson)) %>%
  mutate(pval = map_dbl(pear, pval)) %>%
  mutate(df = map_dbl(pear, df))

slope20_d <- slope20_d %>%
  add_column(r2 = T20_r_daily$r2)

#########################################################################
#slope and r2 for 2021 daily
fit_model <- function(daily21) lm(Ta ~ Elevation, data = daily21) #linear regression
get_slope <- function(mod) tidy(mod)$estimate[2] #slope of lm
pearson <- function(daily21) cor.test(daily21$Elevation, daily21$Ta, data=daily21) #pearson cor test
pval <- function(pear) tidy(pear)$p.value #p value from cor test
df <- function(pear) tidy(pear)$parameter[1] #deg of freedom from cor test

#get R2
T21_r_daily <- daily21 %>%
  select(-c(ID)) %>%
  group_by(daily) %>%
  summarize(r2 = cor(Elevation, Ta, use="complete.obs")^2)

#get lm, slope, pearson cor test, p value, and deg of freedom
slope21_d <- daily21 %>%
  group_nest(daily) %>%
  mutate(model = map(data, fit_model)) %>%
  mutate(slope = map_dbl(model, get_slope)) %>%
  mutate(slope_Ckm = slope*1000) %>%
  mutate(pear = map(data, pearson)) %>%
  mutate(pval = map_dbl(pear, pval)) %>%
  mutate(df = map_dbl(pear, df))

#add R2 vector to above dataframe
slope21_d <- slope21_d %>%
  add_column(r2 = T21_r_daily$r2)

########################################################################
#plot the p values with slope over time for all sensors

ggplot(slope20_d) +
  geom_point(aes(as.Date(daily), slope_Ckm, colour=cut(pval, c(-Inf, 0.05, Inf))))+
  scale_color_manual(name = "pval",
                     values = c("black","blue")) +
  scale_x_date(date_labels = "%m-%d-%Y") +
  labs(title="2020 All")
  
ggplot(slope21_d) +
  geom_point(aes(as.Date(daily), slope_Ckm, colour=cut(pval, c(-Inf, 0.05, Inf))))+
  scale_color_manual(name = "pval",
                     values = c("black","blue")) +
  scale_x_date(date_labels = "%m-%d-%Y") +
  labs(title="2021 All")

########################################################################
#2020 data by transect

daily20$side = substr(daily20$ID,1,2)

daily20_hd <- daily20 %>%
  filter(side == "HD") %>%
  group_by(daily) %>%
  mutate(NAcount = sum(is.na(Ta))) %>% 
  filter(!any(NAcount > 2))

#slope and r2 for 2020 daily - HD
fit_model <- function(daily20_hd) lm(Ta ~ Elevation, data = daily20_hd)
get_slope <- function(mod) tidy(mod)$estimate[2]
pearson <- function(daily20_hd) cor.test(daily20_hd$Elevation, daily20_hd$Ta, data=daily20_hd) #pearson cor test
pval <- function(pear) tidy(pear)$p.value #p value from cor test
df <- function(pear) tidy(pear)$parameter[1] #deg of freedom from cor test

T20_r_hd <- daily20_hd %>%
  select(-c(ID)) %>%
  group_by(daily) %>%
  summarize(r2 = cor(Elevation, Ta, use="complete.obs")^2)

slope20_hd <- daily20_hd %>%
  group_nest(daily) %>%
  mutate(model = map(data, fit_model)) %>%
  mutate(slope = map_dbl(model, get_slope)) %>%
  mutate(slope_Ckm = slope*1000) %>%
  mutate(pear = map(data, pearson)) %>%
  mutate(pval = map_dbl(pear, pval)) %>%
  mutate(df = map_dbl(pear, df))

slope20_hd <- slope20_hd %>%
  add_column(r2 = T20_r_hd$r2)

####now do for 2020 MP
daily20_mp <- daily20 %>%
  filter(side == "MP") %>%
  group_by(daily) %>%
  mutate(NAcount = sum(is.na(Ta))) %>% 
  filter(!any(NAcount > 2))

#slope and r2 for 2020 daily - mp
fit_model <- function(daily20_mp) lm(Ta ~ Elevation, data = daily20_mp)
get_slope <- function(mod) tidy(mod)$estimate[2]
pearson <- function(daily20_mp) cor.test(daily20_mp$Elevation, daily20_mp$Ta, data=daily20_mp) #pearson cor test
pval <- function(pear) tidy(pear)$p.value #p value from cor test
df <- function(pear) tidy(pear)$parameter[1] #deg of freedom from cor test

T20_r_mp <- daily20_mp %>%
  select(-c(ID)) %>%
  group_by(daily) %>%
  summarize(r2 = cor(Elevation, Ta, use="complete.obs")^2)

slope20_mp <- daily20_mp %>%
  group_nest(daily) %>%
  mutate(model = map(data, fit_model)) %>%
  mutate(slope = map_dbl(model, get_slope)) %>%
  mutate(slope_Ckm = slope*1000) %>%
  mutate(pear = map(data, pearson)) %>%
  mutate(pval = map_dbl(pear, pval)) %>%
  mutate(df = map_dbl(pear, df))

slope20_mp <- slope20_mp %>%
  add_column(r2 = T20_r_mp$r2)

###now graph everything for 2020 - all, mp, hd
ggplot() +
  #geom_point(data=slope20_d, aes(as.Date(daily), slope_Ckm, colour=cut(pval, c(-Inf, 0.05, Inf))), size=2, shape=19)+
  scale_color_manual(name = "pval",
                     values = c("black","blue")) +
  scale_x_date(date_labels = "%m-%d-%Y") +
  labs(title="2020 NE") +
  #geom_point(data=slope20_hd, aes(as.Date(daily), slope_Ckm, colour=cut(pval, c(-Inf, 0.05, Inf))), shape=15, size=2) +
  geom_point(data=slope20_mp, aes(as.Date(daily), slope_Ckm, colour=cut(pval, c(-Inf, 0.05, Inf))), shape=17, size=2)

############################################################################
#2021 data by transect

daily21$side = substr(daily21$ID,1,2)

daily21_hd <- daily21 %>%
  filter(side == "HD") %>%
  group_by(daily) %>%
  mutate(NAcount = sum(is.na(Ta))) %>% 
  filter(!any(NAcount > 2))

#slope and r2 for 2121 daily - HD
fit_model <- function(daily21_hd) lm(Ta ~ Elevation, data = daily21_hd)
get_slope <- function(mod) tidy(mod)$estimate[2]
pearson <- function(daily21_hd) cor.test(daily21_hd$Elevation, daily21_hd$Ta, data=daily21_hd) #pearson cor test
pval <- function(pear) tidy(pear)$p.value #p value from cor test
df <- function(pear) tidy(pear)$parameter[1] #deg of freedom from cor test

T21_r_hd <- daily21_hd %>%
  select(-c(ID)) %>%
  group_by(daily) %>%
  summarize(r2 = cor(Elevation, Ta, use="complete.obs")^2)

slope21_hd <- daily21_hd %>%
  group_nest(daily) %>%
  mutate(model = map(data, fit_model)) %>%
  mutate(slope = map_dbl(model, get_slope)) %>%
  mutate(slope_Ckm = slope*1000) %>%
  mutate(pear = map(data, pearson)) %>%
  mutate(pval = map_dbl(pear, pval)) %>%
  mutate(df = map_dbl(pear, df))

slope21_hd <- slope21_hd %>%
  add_column(r2 = T21_r_hd$r2)

####now do for 2121 MP
daily21_mp <- daily21 %>%
  filter(side == "MP") %>%
  group_by(daily) %>%
  mutate(NAcount = sum(is.na(Ta))) %>% 
  filter(!any(NAcount > 2))

#slope and r2 for 2121 daily - mp
fit_model <- function(daily21_mp) lm(Ta ~ Elevation, data = daily21_mp)
get_slope <- function(mod) tidy(mod)$estimate[2]
pearson <- function(daily21_mp) cor.test(daily21_mp$Elevation, daily21_mp$Ta, data=daily21_mp) #pearson cor test
pval <- function(pear) tidy(pear)$p.value #p value from cor test
df <- function(pear) tidy(pear)$parameter[1] #deg of freedom from cor test

T21_r_mp <- daily21_mp %>%
  select(-c(ID)) %>%
  group_by(daily) %>%
  summarize(r2 = cor(Elevation, Ta, use="complete.obs")^2)

slope21_mp <- daily21_mp %>%
  group_nest(daily) %>%
  mutate(model = map(data, fit_model)) %>%
  mutate(slope = map_dbl(model, get_slope)) %>%
  mutate(slope_Ckm = slope*1000) %>%
  mutate(pear = map(data, pearson)) %>%
  mutate(pval = map_dbl(pear, pval)) %>%
  mutate(df = map_dbl(pear, df))

slope21_mp <- slope21_mp %>%
  add_column(r2 = T21_r_mp$r2)

###now graph everything for 2121 - all, mp, hd
ggplot() +
  geom_point(data=slope21_d, aes(as.Date(daily), slope_Ckm, colour=cut(pval, c(-Inf, 0.05, Inf))), size=2, shape=19)+
  scale_color_manual(name = "pval",
                     values = c("black","blue")) +
  scale_x_date(date_labels = "%m-%d-%Y") +
  labs(title="2021 NE") +
  geom_point(data=slope21_hd, aes(as.Date(daily), slope_Ckm, colour=cut(pval, c(-Inf, 0.05, Inf))), shape=15, size=2) +
  geom_point(data=slope21_mp, aes(as.Date(daily), slope_Ckm, colour=cut(pval, c(-Inf, 0.05, Inf))), shape=17, size=2)


#########################################################################
#look at daily slope for 2020 and 2021

PLOT = "NSTGE_20_daily"
ggplot(slope20_d, aes(x=as.Date(daily), y=slope_Ckm)) +
  geom_point(aes(colour=r2)) + 
  geom_hline(aes(yintercept=-6.5, linetype="ELR"), color="Red", size=1) +
  #ylim(-20,30) +
  labs(x= "Date", y=expression("NSTGE " (degree*C/km)), color=expression(paste("R"^2))) +
  #scale_x_datetime(date_labels = "%b", date_break = "1 month") +
  #facet_wrap(~year, scales="free_x") + PlotFormat +
  scale_color_gradient(low='grey', high='black')+
  scale_linetype_manual(name ="", values = c('solid')) 

ggsave(paste(PLOT,".png",sep=""), width = 15, height = 9)

PLOT = "NSTGE_21_daily"
ggplot(slope21_d, aes(x=as.Date(daily), y=slope_Ckm)) +
  geom_point(aes(colour=r2)) + 
  geom_hline(aes(yintercept=-6.5, linetype="ELR"), color="Red", size=1) +
  #ylim(-20,30) +
  labs(x= "Date", y=expression("NSTGE " (degree*C/km)), color=expression(paste("R"^2))) +
  #scale_x_datetime(date_labels = "%b", date_break = "1 month") +
  #facet_wrap(~year, scales="free_x") + PlotFormat +
  scale_color_gradient(low='grey', high='black')+
  scale_linetype_manual(name ="", values = c('solid')) 

ggsave(paste(PLOT,".png",sep=""), width = 15, height = 9)

########################################################################
#looking at specific days
daily21_test <- daily21 %>%
  filter(daily == "2021-04-15")

daily21_test$side = substr(daily21_test$ID,1,2)


eq <- function(x,y) {
  m <- lm(y ~ x)
  as.character(
    as.expression(
      substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
                 list(a = format(coef(m)[1], digits = 4),
                      b = format(coef(m)[2], digits = 4),
                      r2 = format(summary(m)$r.squared, digits = 3)))
    )
  )
}

ggplot(daily21_test, aes(Elevation, Ta)) + geom_point(aes(color=side))+
  geom_smooth(method='lm', formula=y~x)+
  ylim(-10,20)+
  geom_text(x = 3200, y = 5, label = eq(daily21_test$Elevation/1000,daily21_test$Ta), parse = TRUE)

lm <- lm(daily21_test$Ta ~ daily21_test$Elevation)
summary(lm)

test1 <- cor.test(daily21_test$Elevation, daily21_test$Ta)
test1$parameter[1]

################################################
#looking at specific sensors
daily21_fil <- daily21 %>%
  filter(ID == "MP3")

daily21_fil2 <- daily21 %>%
  filter(ID == "MP4")

daily21_fil3 <- daily21 %>%
  filter(ID == "MP5")

daily21_fil4 <- daily21 %>%
  filter(ID =="MP2")

daily21_fil <- rbind(daily21_fil, daily21_fil2, daily21_fil3, daily21_fil4)

temp <- ggplot(daily21_fil, aes(daily, Ta, colour=ID)) + geom_point()

ggplotly(temp)

###############################################################
#20 average daily Ta by sides and all
avgT_20 <- daily20 %>%
  group_by(daily) %>%
  summarize(Tavg = mean(Ta, na.rm=T))

avgT_20_hd <- daily20 %>%
  filter(side == "HD") %>%
  group_by(daily) %>% 
  summarize(Tavg = mean(Ta, na.rm=T))

avgT_20_mp <- daily20 %>%
  filter(side == "MP") %>%
  group_by(daily) %>%
  summarize(Tavg = mean(Ta, na.rm=T))

ggplot() +
  geom_point(data=avgT_20, aes(as.Date(daily), Tavg)) +
  geom_point(data=avgT_20_hd, aes(as.Date(daily), Tavg), color="purple") +
  geom_point(data=avgT_20_mp, aes(as.Date(daily), Tavg), color="dark green") +
  labs(title = "2020. green=NE, purple=NW, black=all")

#21 average daily Ta by sides and all
avgT_21 <- daily21 %>%
  group_by(daily) %>%
  summarize(Tavg = mean(Ta, na.rm=T))

avgT_21_hd <- daily21 %>%
  filter(side == "HD") %>%
  group_by(daily) %>% 
  summarize(Tavg = mean(Ta, na.rm=T))

avgT_21_mp <- daily21 %>%
  filter(side == "MP") %>%
  group_by(daily) %>%
  summarize(Tavg = mean(Ta, na.rm=T))

ggplot() +
  geom_point(data=avgT_21, aes(as.Date(daily), Tavg)) +
  geom_point(data=avgT_21_hd, aes(as.Date(daily), Tavg), color="purple") +
  geom_point(data=avgT_21_mp, aes(as.Date(daily), Tavg), color="dark green") +
  labs(title = "2021. green=NE, purple=NW, black=all")
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

T20_r_daily <- daily20 %>%
  select(-c(ID)) %>%
  group_by(daily) %>%
  summarize(r2 = cor(Elevation, Ta, use="complete.obs")^2)

slope20_d <- daily20 %>%
  group_nest(daily) %>%
  mutate(model = map(data, fit_model)) %>%
  mutate(slope = map_dbl(model, get_slope)) %>%
  mutate(slope_Ckm = slope*1000)

slope20_d <- slope20_d %>%
  add_column(r2 = T20_r_daily$r2)

#########################################################################
#slope and r2 for 2021 daily
fit_model <- function(daily21) lm(Ta ~ Elevation, data = daily21)
get_slope <- function(mod) tidy(mod)$estimate[2]
get_p <- function(mod) tidy(mod)$coefficeints[,4]

T21_r_daily <- daily21 %>%
  select(-c(ID)) %>%
  group_by(daily) %>%
  summarize(r2 = cor(Elevation, Ta, use="complete.obs")^2)
  
T21_p <- daily21 %>%
  drop_na() %>%
  select(-c(ID)) %>%
  group_by(daily) %>% 
  do(tidy(lm(Ta~Elevation, data=daily21))) %>%
  select(c(daily, p.value))


slope21_d <- daily21 %>%
  group_nest(daily) %>%
  mutate(model = map(data, fit_model)) %>%
  mutate(slope = map_dbl(model, get_slope)) %>%
  mutate(slope_Ckm = slope*1000) %>%

  

slope21_d <- slope21_d %>%
  add_column(r2 = T21_r_daily$r2)

########################################################################
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

################################################

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


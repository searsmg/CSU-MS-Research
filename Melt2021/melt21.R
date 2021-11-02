### Script to calculate Melt 21 data ###

library(ggplot2)
library(lubridate)
library(dplyr)
library(plotly)
library(tidyverse)
library(RNRCS)
library(esquisse)
library(RColorBrewer)
library(viridis)
library(rcartocolor)
library(gridExtra)

rm(list = ls()) 

#set working directory and csv file
setwd("C:/Users/sears/Documents/Research/Snow_Hydro_Research/Thesis/Data/Melt")

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

###########################################################################################
#bringing in swe data for mp4 (swe17)
swe17 <- read.csv("swe17.csv") %>%
  mutate(Date=ymd(Date))

#see if hourly precip works to improve the model of added snow
fsnow_hr <- read.csv("C:/Users/sears/Documents/Research/Snow_Hydro_Research/Thesis/Data/SNOTEL/jwhr.csv") %>%
  mutate(Datetime=mdy_hm(Datetime))

#now try to apply fresh snow using modeled precip based on elevation from Liston and Elder 2006
fsnow_plap <- fsnow_hr %>%
  group_by(Date = format(Datetime, "%Y-%m-%d")) %>%
  summarize(fsnow = sum(precip_mm)) %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(fsnow_fix = fsnow*1.05056) #1.05056 is from liston and elder 2006

#add in daily JW for TI models
fsnow_day <- read.csv("C:/Users/sears/Documents/Research/Snow_Hydro_Research/Thesis/Data/SNOTEL/dailyjw.csv") %>%
  mutate(Date = mdy(Date))

#fsnow_day <- fsnow_day %>%
#mutate(Ta_C = (Ta_F-32)*(5/9),
#       PrecipAcum_mm = precip_accum_in*25.4,
#       Sd_mm = Sd_in*25.4,
#       SWE_mm = SWE_in*25.4) %>%
#  select(-c(Ta_F, precip_accum_in, Sd_in, SWE_in)) %>%
#  mutate(precip = PrecipAcum_mm - lag(PrecipAcum_mm))


########################################################
#now run for MP4 using temp rad index model 

mp4a <- read.csv("mp4a.csv") %>%
  mutate(Datetime=ymd_hms(Datetime))

mp4a <- mp4a %>%
  group_by(Date = format(Datetime, "%Y-%m-%d")) %>%
  summarize(Tcum = sum(tpos_a)*1/24,
            radcum = sum(nrfix_a)*1/24) %>%
  mutate(Date = as.Date(Date))

mp4b1 <- read.csv("mp4b1.csv") %>%
  mutate(Datetime=ymd_hms(Datetime))

mp4b1 <- mp4b1 %>%
  group_by(Date = format(Datetime, "%Y-%m-%d")) %>%
  summarize(Tcum = sum(tpos_b1)*1/24,
            radcum = sum(nrfix_b1)*1/24) %>%
  mutate(Date = as.Date(Date))

mp4b2 <- read.csv("mp4b2.csv") %>%
  mutate(Datetime=ymd_hms(Datetime))

mp4b2 <- mp4b2 %>%
  group_by(Date = format(Datetime, "%Y-%m-%d")) %>%
  summarize(Tcum = sum(tpos_b2)*1/24,
            radcum = sum(nrfix_b2)*1/24) %>%
  mutate(Date = as.Date(Date))

mp4c <- read.csv("mp4c.csv") %>%
  mutate(Datetime=ymd_hms(Datetime))

mp4c <- mp4c %>%
  group_by(Date = format(Datetime, "%Y-%m-%d")) %>%
  summarize(Tcum = sum(tpos_c)*1/24,
            radcum = sum(nrfix_c)*1/24) %>%
  mutate(Date = as.Date(Date))

mp4d <- read.csv("mp4d.csv") %>%
  mutate(Datetime=ymd_hms(Datetime))

mp4d <- mp4d %>%
  group_by(Date = format(Datetime, "%Y-%m-%d")) %>%
  summarize(Tcum = sum(tpos_d)*1/24,
            radcum = sum(nrfix_d)*1/24) %>%
  mutate(Date = as.Date(Date))

mp4e1 <- read.csv("mp4e1.csv") %>%
  mutate(Datetime=ymd_hms(Datetime))

mp4e1 <- mp4e1 %>%
  group_by(Date = format(Datetime, "%Y-%m-%d")) %>%
  summarize(Tcum = sum(tpos_e1)*1/24) %>%
  mutate(Date = as.Date(Date))

mp4e2 <- read.csv("mp4e2.csv") %>%
  mutate(Datetime=ymd_hms(Datetime))

mp4e2 <- mp4e2 %>%
  group_by(Date = format(Datetime, "%Y-%m-%d")) %>%
  summarize(Tcum = sum(tpos_e2)*1/24) %>%
  mutate(Date = as.Date(Date))


####################################################################
#now model using degree day for T and rad

#define params
mft <- 2.2 #1.77463
tref <- 4.57663
mfr <- 0.18 #0.15009

#####model a####
mp4a <- mp4a %>%
  mutate(melt = if_else(Tcum<=tref,mfr*radcum,
                        mft*(Tcum-tref)+mfr*radcum)) %>%
  mutate(melt = pmax(melt, 0))

mp4a <- merge(mp4a, fsnow_plap, by="Date") #update later depending on fsnow

mp4a$swe_cum <- as.numeric(NA)
mp4a[1,"swe_cum"] <- 644


for(i in 2:nrow(mp4a)){
  if(is.na(mp4a$swe_cum[i])){
    mp4a$swe_cum[i] = mp4a$swe_cum[i-1]+mp4a$fsnow_fix[i]-mp4a$melt[i]
  }
}

ggplot() + geom_line(data=mp4a, aes(Date, swe_cum)) +
  geom_point(data=swe17, aes(x=Date, y=SWE))

####model b1####
mp4b1 <- mp4b1 %>%
  mutate(melt = if_else(Tcum<=tref,mfr*radcum,
                        mft*(Tcum-tref)+mfr*radcum)) %>%
  mutate(melt = pmax(melt, 0))

mp4b1 <- merge(mp4b1, fsnow_plap, by="Date") #update later depending on fsnow

mp4b1$swe_cum <- as.numeric(NA)
mp4b1[1,"swe_cum"] <- 644


for(i in 2:nrow(mp4b1)){
  if(is.na(mp4b1$swe_cum[i])){
    mp4b1$swe_cum[i] = mp4b1$swe_cum[i-1]+mp4b1$fsnow_fix[i]-mp4b1$melt[i]
  }
}

ggplot() + geom_line(data=mp4b1, aes(Date, swe_cum)) +
  geom_point(data=swe17, aes(x=Date, y=SWE))

####model b2####
mp4b2 <- mp4b2 %>%
  mutate(melt = if_else(Tcum<=tref,mfr*radcum,
                        mft*(Tcum-tref)+mfr*radcum)) %>%
  mutate(melt = pmax(melt, 0))

mp4b2 <- merge(mp4b2, fsnow_plap, by="Date") #update later depending on fsnow

mp4b2$swe_cum <- as.numeric(NA)
mp4b2[1,"swe_cum"] <- 644


for(i in 2:nrow(mp4b2)){
  if(is.na(mp4b2$swe_cum[i])){
    mp4b2$swe_cum[i] = mp4b2$swe_cum[i-1]+mp4b2$fsnow_fix[i]-mp4b2$melt[i]
  }
}

ggplot() + geom_line(data=mp4b2, aes(Date, swe_cum)) +
  geom_point(data=swe17, aes(x=Date, y=SWE))

####model c####
mp4c <- mp4c %>%
  mutate(melt = if_else(Tcum<=tref,mfr*radcum,
                        mft*(Tcum-tref)+mfr*radcum)) %>%
  mutate(melt = pmax(melt, 0))

mp4c <- merge(mp4c, fsnow_plap, by="Date") #update later depending on fsnow

mp4c$swe_cum <- as.numeric(NA)
mp4c[1,"swe_cum"] <- 644


for(i in 2:nrow(mp4c)){
  if(is.na(mp4c$swe_cum[i])){
    mp4c$swe_cum[i] = mp4c$swe_cum[i-1]+mp4c$fsnow_fix[i]-mp4c$melt[i]
  }
}

ggplot() + geom_line(data=mp4c, aes(Date, swe_cum)) +
  geom_point(data=swe17, aes(x=Date, y=SWE))

####model d####
mp4d <- mp4d %>%
  mutate(melt = if_else(Tcum<=tref,mfr*radcum,
                        mft*(Tcum-tref)+mfr*radcum)) %>%
  mutate(melt = pmax(melt, 0))

mp4d <- merge(mp4d, fsnow_plap, by="Date") #update later depending on fsnow

mp4d$swe_cum <- as.numeric(NA)
mp4d[1,"swe_cum"] <- 644


for(i in 2:nrow(mp4d)){
  if(is.na(mp4d$swe_cum[i])){
    mp4d$swe_cum[i] = mp4d$swe_cum[i-1]+mp4d$fsnow_fix[i]-mp4d$melt[i]
  }
}

ggplot() + geom_line(data=mp4d, aes(Date, swe_cum)) +
  geom_point(data=swe17, aes(x=Date, y=SWE))

####model e1####
#first bring in variable MFt
mfvar <- read.csv("TI_mfs.csv") %>%
  mutate(Date = mdy(Date))

mp4e1 <- merge(mp4e1, mfvar, by="Date")

mp4e1 <- mp4e1 %>%
  mutate(melt = if_else(Tcum<=0,0,
                        MFt*(Tcum-0))) %>%
  mutate(melt = pmax(melt, 0))

mp4e1 <- merge(mp4e1, fsnow_plap, by="Date") #update later depending on fsnow

mp4e1$swe_cum <- as.numeric(NA)
mp4e1[1,"swe_cum"] <- 644


for(i in 2:nrow(mp4e1)){
  if(is.na(mp4e1$swe_cum[i])){
    mp4e1$swe_cum[i] = mp4e1$swe_cum[i-1]+mp4e1$fsnow_fix[i]-mp4e1$melt[i]
  }
}

ggplot() + geom_line(data=mp4e1, aes(Date, swe_cum)) +
  geom_point(data=swe17, aes(x=Date, y=SWE))


####model e2####
#first bring in variable MFt
mfvar <- read.csv("TI_mfs.csv") %>%
  mutate(Date = mdy(Date))

mp4e2 <- merge(mp4e2, mfvar, by="Date")

mp4e2 <- mp4e2 %>%
  mutate(melt = if_else(Tcum<=0,0,
                        MFt*(Tcum-0))) %>%
  mutate(melt = pmax(melt, 0))

mp4e2 <- merge(mp4e2, fsnow_plap, by="Date") #update later depending on fsnow

mp4e2$swe_cum <- as.numeric(NA)
mp4e2[1,"swe_cum"] <- 644


for(i in 2:nrow(mp4e2)){
  if(is.na(mp4e2$swe_cum[i])){
    mp4e2$swe_cum[i] = mp4e2$swe_cum[i-1]+mp4e2$fsnow_fix[i]-mp4e2$melt[i]
  }
}

ggplot() + geom_line(data=mp4e2, aes(Date, swe_cum)) +
  geom_point(data=swe17, aes(x=Date, y=SWE))

##############################################################################
#filter so swe doesn't go below 0

mp4a <- mp4a %>%
  mutate(swe_cum = ifelse(swe_cum <0,0,swe_cum)) %>%
  rename(swe_a = swe_cum) %>%
  select(c(Date, swe_a))

mp4b1 <- mp4b1 %>%
  mutate(swe_cum = ifelse(swe_cum <0,0,swe_cum)) %>%
  rename(swe_b1 = swe_cum)%>%
  select(c(Date, swe_b1))

mp4b2 <- mp4b2 %>%
  mutate(swe_cum = ifelse(swe_cum <0,0,swe_cum)) %>%
  rename(swe_b2 = swe_cum)%>%
  select(c(Date, swe_b2))

mp4c <- mp4c %>%
  mutate(swe_cum = ifelse(swe_cum <0,0,swe_cum)) %>%
  rename(swe_c = swe_cum)%>%
  select(c(Date, swe_c))

mp4d <- mp4d %>%
  mutate(swe_cum = ifelse(swe_cum <0,0,swe_cum)) %>%
  rename(swe_d = swe_cum)%>%
  select(c(Date, swe_d))

mp4e1 <- mp4e1 %>%
  mutate(swe_cum = ifelse(swe_cum <0,0,swe_cum)) %>%
  rename(swe_e1 = swe_cum)%>%
  select(c(Date, swe_e1))

mp4e2 <- mp4e2 %>%
  mutate(swe_cum = ifelse(swe_cum <0,0,swe_cum)) %>%
  rename(swe_e2 = swe_cum)%>%
  select(c(Date, swe_e2))

allmod <- cbind(mp4a, mp4b1, mp4b2, mp4c, mp4d, mp4e1, mp4e2)
allmod <- allmod[-c(3,5,7,9,11,13)]

###############################################################################
#PLOTTING

palette_OkabeIto <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", 
                      "#0072B2", "#D55E00", "#CC79A7", "#999999")

safe_pal <- carto_pal(12, "Safe")

#all models together
PLOT="allmelt"
custombreaks <- seq(0, 700, 50)
ggplot() + 
  geom_line(data=mp4a, aes(Date, swe_a, color="A"), size=1.25) +
  geom_line(data=mp4b1, aes(Date, swe_b1, color="B1"), size=1.25) +
  geom_line(data=mp4b2, aes(Date, swe_b2, color="B2"), size=1.25) +
  geom_line(data=mp4c, aes(Date, swe_c, color="C"), size=1.25) +
  geom_line(data=mp4d, aes(Date, swe_d, color="D"), size=1.25) +
  geom_line(data=mp4e1, aes(Date, swe_e1, color="E1"), size=1.25) +
  geom_line(data=mp4e2, aes(Date, swe_e2, color="E2"), size=1.25) +
  geom_point(data=swe17, aes(x=Date, y=SWE, shape="observed\nSWE"), size=4) +
  scale_shape_manual(values=17) +
  scale_color_manual(values=safe_pal) +
  #scale_color_viridis(discrete=T, option="D") +
  labs(y="SWE (mm)", shape="", color="models", x="") +
  PlotFormat + 
  #theme(legend.position = c(0.95, 0.8)) +
  scale_y_continuous(breaks = custombreaks, labels = every_nth(custombreaks, 2, inverse=TRUE))

ggsave(paste(PLOT,".png",sep=""), width = 15, height = 9)


allmelt <- ggplot() + 
  geom_line(data=mp4a, aes(Date, swe_a, color="A"), size=1.25) +
  geom_line(data=mp4b1, aes(Date, swe_b1, color="B1"), size=1.25) +
  geom_line(data=mp4b2, aes(Date, swe_b2, color="B2"), size=1.25) +
  geom_line(data=mp4c, aes(Date, swe_c, color="C"), size=1.25) +
  geom_line(data=mp4d, aes(Date, swe_d, color="D"), size=1.25) +
  geom_line(data=mp4e1, aes(Date, swe_e1, color="E1"), size=1.25) +
  geom_line(data=mp4e2, aes(Date, swe_e2, color="E2"), size=1.25) +
  geom_point(data=swe17, aes(x=Date, y=SWE, shape="observed\nSWE"), size=4) +
  scale_shape_manual(values=17) +
  scale_color_manual(values=safe_pal) +
  #scale_color_viridis(discrete=T, option="D") +
  labs(y="SWE (mm)", shape="", color="models", x="")

ggplotly(allmelt)

## \]81d0ifference when comparing models to a
ggplot(allmod, aes(x=Date)) +
  geom_line(aes(y=swe_a-swe_b1, color="b1 diff")) +
  geom_line(aes(y=swe_a-swe_b2, color="b2_diff")) +
  geom_line(aes(y=swe_a-swe_c, color="c_diff")) +
  geom_line(aes(y=swe_a-swe_d, color="d_diff")) +
  labs(x="", y="SWE difference (mm)", color="model") +
  scale_color_brewer(palette="Dark2")

#compare nrcum to each scenario (4)
b1swe <- ggplot(allmod, aes(x=swe_a, y=swe_b1)) + 
  geom_line() +
  scale_x_reverse() + scale_y_reverse() +
  geom_abline(intercept = 0, slope = 1, size=1, color="red") #1:1 

b2swe <- ggplot(allmod, aes(x=swe_a, y=swe_b2)) + 
  geom_line() +
  scale_x_reverse() + scale_y_reverse() +
  geom_abline(intercept = 0, slope = 1, size=1, color="red") #1:1

cswe <- ggplot(allmod, aes(x=swe_a, y=swe_c)) + 
  geom_line() +
  scale_x_reverse() + scale_y_reverse() +
  geom_abline(intercept = 0, slope = 1, size=1, color="red") #1:1

dswe <- ggplot(allmod, aes(x=swe_a, y=swe_d)) + 
  geom_line() +
  scale_x_reverse() + scale_y_reverse() +
  geom_abline(intercept = 0, slope = 1, size=1, color="red") #1:1

grid.arrange(b1swe, b2swe, cswe, dswe, nrow=2)

ggplot() + 
  geom_line(data=mp4a, aes(Date, swe_a, color="A"), size=1.25) +
  geom_line(data=mp4b1, aes(Date, swe_b1, color="B1"), size=1.25) +
  geom_line(data=mp4b2, aes(Date, swe_b2, color="B2"), size=1.25) +
  geom_line(data=mp4c, aes(Date, swe_c, color="C"), size=1.25) +
  geom_line(data=mp4d, aes(Date, swe_d, color="D"), size=1.25) +
  geom_line(data=mp4e1, aes(Date, swe_e1, color="E1"), size=1.25) +
  geom_line(data=mp4e2, aes(Date, swe_e2, color="E2"), size=1.25) +
  geom_point(data=swe17, aes(x=Date, y=SWE, shape="observed\nSWE"), size=4) +
  scale_shape_manual(values=17) +
  scale_color_manual(values=safe_pal) +
  #scale_color_viridis(discrete=T, option="D") +
  labs(y="SWE (mm)", shape="", color="models", x="") +
  PlotFormat + 
  #theme(legend.position = c(0.95, 0.8)) +
  scale_y_continuous(trans="log10")

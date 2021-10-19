### Script to calculate Melt 21 data ###

library(ggplot2)
library(lubridate)
library(dplyr)
library(plotly)
library(tidyverse)
library(RNRCS)
library(esquisse)

rm(list = ls()) 

#set working directory and csv file
setwd("C:/Users/sears/Documents/Research/Snow_Hydro_Research/Thesis/Data/Melt")

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

####################################################################
#now model using degree day for T and rad

#define params
mft <- 2.2 #1.77463
tref <- 4.57663
mfr <- 0.18 #0.15009

#model a
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

#model b1
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

#model b2
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

#model c
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

#model d
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


ggplot() + 
  geom_line(data=mp4a, aes(Date, swe_cum, color="a")) +
  geom_line(data=mp4b1, aes(Date, swe_cum, color="b1")) +
  geom_line(data=mp4b2, aes(Date, swe_cum, color="b2")) +
  geom_line(data=mp4c, aes(Date, swe_cum, color="c")) +
  geom_line(data=mp4d, aes(Date, swe_cum, color="d")) +
  geom_point(data=swe17, aes(x=Date, y=SWE))



#filter so melt does not go negative 
tobs_hrsum <- tobs_hrsum %>%
  filter(swe_cum > 0)

telr_hrsum <- telr_hrsum %>%
  filter(swe_cum > 0)

#plot the two to compare elr vs obs
PLOT = "Base vs ELR+obvp"
compare <- ggplot() + geom_line(data=tobs_hrsum, aes(Date, swe_cum), size=1) +
  geom_line(data=telr_hrsum, aes(Date, swe_cum), color="purple", size=1) +
  geom_point(data=swe17, aes(x=Date, y=SWE), shape="triangle", size=2) +
  theme_bw() +
  PlotFormat +
  labs(x="", y="SWE (mm)") +
  scale_y_continuous(breaks=seq(0, 700, 100)) 
  
ggplotly(compare)
compare


ggsave(paste(PLOT,".png",sep=""), width = 15, height = 9)
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
setwd("C:/Users/sears/Documents/Research/Snow_Hydro_Research/Thesis/Data/Air Temp/For R/2021")

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


##########################################################################
#### PROCESSING ####

#temp, elevation, and elevation band data
temp_elev_21 <- read.csv(file="Melt21_elev_noOUT.csv", header=TRUE) %>%
  mutate(Datetime = mdy_hm(Datetime))

RH_dewpt <- read.csv(file="RH_dewpt_all.csv", header=TRUE) %>%
  mutate(Datetime = mdy_hm(Datetime))

all21 <- merge(temp_elev_21, RH_dewpt, by=c("Datetime", "ID"))

#write.csv(all21, "all21.csv")

#get a temp for each elev band (grouping bands and getting avg temp)
#bands21 <- all21 %>%
#  group_by(Datetime, Band) %>%
#  summarize(bandT = mean(AirT_C, na.rm=TRUE),
#            bandRH = mean(humidity),
#            banddewpt = mean(dewpoint))

#bandT_long <- bands21 %>%
#  pivot_wider(names_from = Band, values_from = bandT)

#write.csv(bandT_long, "bandT.csv")

#bandRH_long <- bands21 %>%
#  select(Datetime, bandRH,Band) %>%
#  pivot_wider(names_from = Band, values_from=bandRH)

#write.csv(bandRH_long, "bandRH.csv")

###################################################
#bring band T back in - have all Ta for melt 21
bandT <- read.csv(file="bands21_9.csv", header=TRUE) %>%
  mutate(Datetime = mdy_hm(Datetime))

#bring RH back in
bandRH <- read.csv(file="bandRH.csv", header=TRUE) %>%
  mutate(Datetime = mdy_hm(Datetime))

#get rid of the minute on date time (e.g., 14:01 - want 14:00)
minute(bandT$Datetime) <- 0
minute(bandRH$Datetime) <- 0

#start working on rad - this is 10 min data and hourly avg is every 7 rows
rad21 <- read.csv(file="C:/Users/sears/Documents/Research/Snow_Hydro_Research/Thesis/Data/Radiation/For R/Rad_melt21.csv", 
                  header=TRUE) %>%
  mutate(Datetime = mdy_hm(Datetime))

#get hourly rad data
rad21 <- rad21 %>%
  filter(minute(Datetime) == 0) %>%
  filter(row_number() %% 2 != 1) ## Delete odd-rows

#add rad to bandT
bandT_rad <- merge(bandT, rad21, by = "Datetime")

#add RH to bandT+rad
band_all <- merge(bandT_rad, bandRH, by="Datetime")

#now break each band out so we can calculate rad and melt
b1_2 <- band_all %>%
  select(Datetime, b1_2, Swin, Swout, Lwin, Lwout, b1_2_RH)

b3 <- band_all %>%
  select(Datetime, b3, Swin, Swout, Lwin, Lwout, b3_RH)

b4 <- band_all %>%
  select(Datetime, b4, Swin, Swout, Lwin, Lwout, b4_RH)

b5 <- band_all %>%
  select(Datetime, b5, Swin, Swout, Lwin, Lwout, b5_RH)

b6 <- band_all %>%
  select(Datetime, b6, Swin, Swout, Lwin, Lwout, b6_RH)

b7 <- band_all %>%
  select(Datetime, b7, Swin, Swout, Lwin, Lwout, b7_RH)

b8 <- band_all %>%
  select(Datetime, b8, Swin, Swout, Lwin, Lwout, b8_RH)

b9 <- band_all %>%
  select(Datetime, b9, Swin, Swout, Lwin, Lwout, b9_RH)

b10_14 <- band_all %>%
  select(Datetime, b10_14, Swin, Swout, Lwin, Lwout, b10_RH)


#### PROCESSING DONE ####
##########################################################################

## DON'T NEED TO RUN ANY OF THE ABOVE LINES AS LONG AS RDATA FILE IS READ IN
#load("C:/Users/sears/Documents/Repos/CSU-MS-Research/Melt2021/DFs.Rdata")

## for each band compute LWin
# first find sat vapor press (ea), then back out Cc. fix Cc for >1 or <1, then calc LW in for all bands

#stefan boltz constant
stef <- 5.67 * 10^-8

b1_2 <- b1_2 %>%
  mutate(esat = (6.112*exp((17.62*b1_2)/(243.12+b1_2)))) %>%
  mutate(ea = (b1_2_RH * esat)/100) %>%
  mutate(Cc_pt1 = Lwin/((stef)*(b1_2+273.15)^4)) %>%
  mutate(Cc_pt2 = Cc_pt1 /(0.53+(0.065*ea))) %>%
  mutate(Cc = (Cc_pt2 -1)/0.4) %>%
  mutate(Cc_fix = if_else(Cc<0,0,if_else(Cc>1,1,Cc))) %>%
  mutate(Lwin_fix = (0.53+(0.065*ea))*(1+(0.4*Cc_fix))*(5.67*10^-8)*((b1_2+273.15)^4)) %>%
  select(-c(Cc_pt1, Cc_pt2, Cc))

b3 <- b3 %>%
  mutate(esat = (6.112*exp((17.62*b3)/(243.12+b3)))) %>%
  mutate(ea = (b3_RH * esat)/100) %>%
  mutate(Cc_pt1 = Lwin/((stef)*(b3+273.15)^4)) %>%
  mutate(Cc_pt2 = Cc_pt1 /(0.53+(0.065*ea))) %>%
  mutate(Cc = (Cc_pt2 -1)/0.4) %>%
  mutate(Cc_fix = if_else(Cc<0,0,if_else(Cc>1,1,Cc))) %>%
  mutate(Lwin_fix = (0.53+(0.065*ea))*(1+(0.4*Cc_fix))*(5.67*10^-8)*((b3+273.15)^4)) %>%
  select(-c(Cc_pt1, Cc_pt2, Cc))

b4 <- b4 %>%
  mutate(esat = (6.112*exp((17.62*b4)/(243.12+b4)))) %>%
  mutate(ea = (b4_RH * esat)/100) %>%
  mutate(Cc_pt1 = Lwin/((stef)*(b4+273.15)^4)) %>%
  mutate(Cc_pt2 = Cc_pt1 /(0.53+(0.065*ea))) %>%
  mutate(Cc = (Cc_pt2 -1)/0.4) %>%
  mutate(Cc_fix = if_else(Cc<0,0,if_else(Cc>1,1,Cc))) %>%
  mutate(Lwin_fix = (0.53+(0.065*ea))*(1+(0.4*Cc_fix))*(5.67*10^-8)*((b4+273.15)^4)) %>%
  select(-c(Cc_pt1, Cc_pt2, Cc))

b5 <- b5 %>%
  mutate(esat = (6.112*exp((17.62*b5)/(243.12+b5)))) %>%
  mutate(ea = (b5_RH * esat)/100) %>%
  mutate(Cc_pt1 = Lwin/((stef)*(b5+273.15)^4)) %>%
  mutate(Cc_pt2 = Cc_pt1 /(0.53+(0.065*ea))) %>%
  mutate(Cc = (Cc_pt2 -1)/0.4) %>%
  mutate(Cc_fix = if_else(Cc<0,0,if_else(Cc>1,1,Cc))) %>%
  mutate(Lwin_fix = (0.53+(0.065*ea))*(1+(0.4*Cc_fix))*(5.67*10^-8)*((b5+273.15)^4)) %>%
  select(-c(Cc_pt1, Cc_pt2, Cc))

b6 <- b6 %>%
  mutate(esat = (6.112*exp((17.62*b6)/(243.12+b6)))) %>%
  mutate(ea = (b6_RH * esat)/100) %>%
  mutate(Cc_pt1 = Lwin/((stef)*(b6+273.15)^4)) %>%
  mutate(Cc_pt2 = Cc_pt1 /(0.53+(0.065*ea))) %>%
  mutate(Cc = (Cc_pt2 -1)/0.4) %>%
  mutate(Cc_fix = if_else(Cc<0,0,if_else(Cc>1,1,Cc))) %>%
  mutate(Lwin_fix = (0.53+(0.065*ea))*(1+(0.4*Cc_fix))*(5.67*10^-8)*((b6+273.15)^4)) %>%
  select(-c(Cc_pt1, Cc_pt2, Cc))

b7 <- b7 %>%
  mutate(esat = (6.112*exp((17.62*b7)/(243.12+b7)))) %>%
  mutate(ea = (b7_RH * esat)/100) %>%
  mutate(Cc_pt1 = Lwin/((stef)*(b7+273.15)^4)) %>%
  mutate(Cc_pt2 = Cc_pt1 /(0.53+(0.065*ea))) %>%
  mutate(Cc = (Cc_pt2 -1)/0.4) %>%
  mutate(Cc_fix = if_else(Cc<0,0,if_else(Cc>1,1,Cc))) %>%
  mutate(Lwin_fix = (0.53+(0.065*ea))*(1+(0.4*Cc_fix))*(5.67*10^-8)*((b7+273.15)^4)) %>%
  select(-c(Cc_pt1, Cc_pt2, Cc))

b8 <- b8 %>%
  mutate(esat = (6.112*exp((17.62*b8)/(243.12+b8)))) %>%
  mutate(ea = (b8_RH * esat)/100) %>%
  mutate(Cc_pt1 = Lwin/((stef)*(b8+273.15)^4)) %>%
  mutate(Cc_pt2 = Cc_pt1 /(0.53+(0.065*ea))) %>%
  mutate(Cc = (Cc_pt2 -1)/0.4) %>%
  mutate(Cc_fix = if_else(Cc<0,0,if_else(Cc>1,1,Cc))) %>%
  mutate(Lwin_fix = (0.53+(0.065*ea))*(1+(0.4*Cc_fix))*(5.67*10^-8)*((b8+273.15)^4)) %>%
  select(-c(Cc_pt1, Cc_pt2, Cc))

b9 <- b9 %>%
  mutate(esat = (6.112*exp((17.62*b9)/(243.12+b9)))) %>%
  mutate(ea = (b9_RH * esat)/100) %>%
  mutate(Cc_pt1 = Lwin/((stef)*(b9+273.15)^4)) %>%
  mutate(Cc_pt2 = Cc_pt1 /(0.53+(0.065*ea))) %>%
  mutate(Cc = (Cc_pt2 -1)/0.4) %>%
  mutate(Cc_fix = if_else(Cc<0,0,if_else(Cc>1,1,Cc))) %>%
  mutate(Lwin_fix = (0.53+(0.065*ea))*(1+(0.4*Cc_fix))*(5.67*10^-8)*((b9+273.15)^4)) %>%
  select(-c(Cc_pt1, Cc_pt2, Cc))

b10_14 <- b10_14 %>%
  mutate(esat = (6.112*exp((17.62*b10_14)/(243.12+b10_14)))) %>%
  mutate(ea = (b10_RH * esat)/100) %>%
  mutate(Cc_pt1 = Lwin/((stef)*(b10_14+273.15)^4)) %>%
  mutate(Cc_pt2 = Cc_pt1 /(0.53+(0.065*ea))) %>%
  mutate(Cc = (Cc_pt2 -1)/0.4) %>%
  mutate(Cc_fix = if_else(Cc<0,0,if_else(Cc>1,1,Cc))) %>%
  mutate(Lwin_fix = (0.53+(0.065*ea))*(1+(0.4*Cc_fix))*(5.67*10^-8)*((b10_14+273.15)^4)) %>%
  select(-c(Cc_pt1, Cc_pt2, Cc))


###########################################################################################
#bringing in swe data for mp4 (swe17)
swe17 <- read.csv("swe17.csv") %>%
  mutate(Date=ymd(Date))

#see if hourly precip works to improve the model of added snow
fsnow_hr <- read.csv("C:/Users/sears/Documents/Research/Snow_Hydro_Research/Thesis/Data/SNOTEL/jwhr.csv") %>%
  mutate(Datetime=mdy_hm(Datetime))

fsnow <- read.csv("C:/Users/sears/Documents/Research/Snow_Hydro_Research/Thesis/Data/SNOTEL/JW21_melt.csv") %>%
  mutate(Date = mdy(Date)) %>%
  select(c(Date, freshsnow))

#now try to apply fresh snow using modeled precip based on elevation from Liston and Elder 2006
fsnow_plap <- fsnow_hr %>%
  group_by(Date = format(Datetime, "%Y-%m-%d")) %>%
  summarize(fsnow = sum(precip_mm)) %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(fsnow_fix = fsnow*1.05056) #1.05056 is from liston and elder 2006


########################################################
#now run for MP4 using temp rad index model 

mp4obs <- read.csv("C:/Users/sears/Documents/Research/Snow_Hydro_Research/Thesis/Data/Radiation/For R/mp4obs.csv") %>%
  mutate(Datetime=ymd_hms(Datetime))

mp4elr <- read.csv("C:/Users/sears/Documents/Research/Snow_Hydro_Research/Thesis/Data/Radiation/For R/mp4elr.csv") %>%
  mutate(Datetime=ymd_hms(Datetime))

###############################################################
# getting cum T and rad

tobs_hrsum <- mp4obs %>%
  mutate(Tpos = if_else(AirT_C <0,0,AirT_C)) %>%
  group_by(Date = format(Datetime, "%Y-%m-%d")) %>%
  summarize(Tcum = sum(Tpos)*1/24,
            radcum = sum(nrfix)*1/24) %>%
  mutate(Date = as.Date(Date))

telr_hrsum <- mp4elr %>%
  mutate(Tpos = if_else(Tlap <0,0,Tlap)) %>%
  group_by(Date = format(Datetime, "%Y-%m-%d")) %>%
  summarize(Tcum = sum(Tpos)*1/24,
            radcum = sum(nrfix)*1/24) %>%
  mutate(Date = as.Date(Date))

####################################################################
#now model using degree day for T and rad

#define params
mft <- 1.5 #1.588608537
tref <- 3
mfr <- 0.1 #0.146665342

tobs_hrsum <- tobs_hrsum %>%
  filter(Date > "2021-04-30") %>%
  mutate(melt = if_else(Tcum<=tref,mfr*radcum,
                        mft*(Tcum-tref)+mfr*radcum)) %>%
  mutate(melt = pmax(melt, 0))

tobs_hrsum <- merge(tobs_hrsum, fsnow_plap, by="Date") #update later depending on fsnow

tobs_hrsum$swe_cum <- as.numeric(NA)
tobs_hrsum[1,"swe_cum"] <- 644
#tobs_hrsum$fsnow_fix <- tobs_hrsum$freshsnow*1.5056

for(i in 2:nrow(tobs_hrsum)){
  if(is.na(tobs_hrsum$swe_cum[i])){
    tobs_hrsum$swe_cum[i] = tobs_hrsum$swe_cum[i-1]+tobs_hrsum$fsnow_fix[i]-tobs_hrsum$melt[i]
  }
}

ggplot() + geom_line(data=tobs_hrsum, aes(Date, swe_cum)) +
  geom_point(data=swe17, aes(x=Date, y=SWE))

##elr model
telr_hrsum <- telr_hrsum %>%
  filter(Date > "2021-04-30") %>%
  mutate(melt = if_else(Tcum<=tref,mfr*radcum,
                        mft*(Tcum-tref)+mfr*radcum)) %>%
  mutate(melt = pmax(melt, 0))

telr_hrsum <- merge(telr_hrsum, fsnow_plap, by="Date") #update fsno

telr_hrsum$swe_cum <- as.numeric(NA)
telr_hrsum[1,"swe_cum"] <- 644

for(i in 2:nrow(telr_hrsum)){
  if(is.na(telr_hrsum$swe_cum[i])){
    telr_hrsum$swe_cum[i] = telr_hrsum$swe_cum[i-1]+telr_hrsum$fsnow_fix[i]-telr_hrsum$melt[i]
  }
}

ggplot() + geom_line(data=telr_hrsum, aes(Date, swe_cum)) +
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
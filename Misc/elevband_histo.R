library(tidyverse)
library(ggplot2)


#set working directory and csv file
setwd("C:/Users/sears/Documents/Research/Snow_Hydro_Research/Thesis/Data/Bands")

#bring in 20 and 21 temp datasets
bands <- read.csv(file = "Elevbands.csv")


PlotFormat = theme(axis.text=element_text(size=16, color="black"),
                   axis.title.x=element_text(size=18, hjust=0.5, margin=margin(t=20, r=20, b=20, l=20), color="black"),              
                   axis.title.y=element_text(size=18, vjust=0.5,  margin=margin(t=20, r=20, b=20, l=20), color="black"),              
                   plot.title=element_text(size=26,face="bold",hjust=0.5, margin=margin(t=20, r=20, b=20, l=20)),      
                   legend.title=element_blank(),                                                                    
                   legend.text=element_text(size=16, color="black"),                                                                   
                   legend.position = "right", 
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   strip.text = element_text(size=25))



PLOT ="band histrogram"
ggplot(bands) + geom_col(aes(x=Band, y=percent)) + PlotFormat + labs(x="", y="Percent")


ggsave(paste(PLOT,".png",sep=""), width = 15, height = 9)

veg <- read.csv(file="veg_band.csv")

veg <- veg %>%
  pivot_longer(!Band, names_to="veg", values_to="count")

PLOT="veg by band"
ggplot(veg, aes(fill=veg, x=Band, y=count)) +
  geom_bar(position="stack", stat="identity")+
  xlim(1,14) +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14)) +
  PlotFormat +
  labs(y=expression(paste("Area ",(km^2)))) +
  scale_fill_manual(values=c("Barren.land"="#B2ADA3", "Developed"="#ED0000",
                             "Evergreen" = "#1C6330", "Herbaceous"="#E2E2C1",
                             "Perennial.ice"="#E1E1E1", "Woody.wetland"="#BAD8EA"),
                    labels=c("Barren", "Developed", "Evergreen", "Herbaceous", "Perennial Ice", "Woody Wetland")) 
  
  
ggsave(paste(PLOT,".png",sep=""), width = 15, height = 9)

#bring in some swe data
swe <- read.csv(file="swe.csv")
swe$date <- factor(swe$date, 
                   levels=c("1-May", "12-May", "27-May", "10-Jun"))

ggplot(swe, aes(ID, swe, fill=canopy)) +
  geom_bar(position="stack", stat="identity", na.rm=TRUE)+
  scale_x_continuous("ID", labels = as.character(swe$ID), breaks = swe$ID)+
  geom_bar(colour="black", stat="identity")+
  facet_wrap(~date) +
  PlotFormat +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())



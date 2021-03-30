#############################################
#####        HYDRO DAYS 2021            #####
#############################################

library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)

rm(list=ls())

## data prep

setwd("C:/Users/sears/Documents/Research/Hydrology_Days/2021/Figs_Data/")

#read in elev data
melt <- read.csv("C:/Users/sears/Documents/Research/Hydrology_Days/2021/Figs_Data/MeltSum_CumSum.csv")

melt$datetime = mdy_hm(paste(melt$Datetime, melt$time))

#Plot size
PlotWidth = 16.5
PlotHeight = 9

PlotFormat = theme(axis.text=element_text(size=20),
                   axis.title.x=element_text(size=24, hjust=0.5, margin=margin(t=20, r=20, b=20, l=20)),              
                   axis.title.y=element_text(size=24, vjust=0.5,  margin=margin(t=20, r=20, b=20, l=20)),              
                   plot.title=element_text(size=26,face="bold",hjust=0.5, margin=margin(t=20, r=20, b=20, l=20)),      
                   legend.title=element_blank(),                                                                    
                   legend.text=element_text(size=20),                                                                   
                   legend.position = "bottom", 
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   axis.text.x = element_text(colour = "black"),
                   axis.text.y = element_text(colour = "black"))


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


#plots 
PLOT = "Cumulative Melt"
breaks_y <- seq(0,750, 50)
ggplot(melt)+geom_line(aes(x=datetime, y=ObsCumSum_Melt_mm, color="Observed temp"), size=1.5)+
  geom_line(aes(x=datetime, y=ELRCumSum_Melt_mm, color="ELR"), size=1.5) + PlotFormat + 
  labs(y= "Cumulative melt (mm)") +
  scale_y_continuous(breaks = breaks_y, labels = every_nth(breaks_y, 2, inverse=TRUE))+
  #scale_x_datetime(date_breaks = "14 days", labels = date_format("%b %d")) +
  theme(axis.title.x=element_blank())+
  scale_color_manual(values=c("green", "black"))

ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)

PLOT = "1to1"
ggplot(melt)+geom_point(aes(x=ObsMelt_mm, y=ELRMelt_mm), size=3)+
  geom_abline(intercept = 0, slope = 1, size=1.5, color="red") +
  PlotFormat +
  labs(y="Melt with ELR (mm/hr)", x="Melt with observed temp (mm/hr)")

ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight)

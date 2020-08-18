#lapse rate analysis for melt season 2020

library(ggplot2)

############DATA PREP#############

#set working directory and csv file
setwd("C:/Users/sears/Documents/Research/Snow_Hydro_Research/Field_Data_071120/Processed/")
#read csv, skip top 4 rows, use row 5 as header, remove columns 6:12
HD1 <- read.csv(file = "HD1-62.csv", skip=4, header=TRUE, stringsAsFactors = FALSE) 
HD1 <-HD1[-c(6:12)]

#remove NA rows
HD1<-na.omit(HD1)

#malipulate date
HD1$date <- c("Sun Apr 19 2020 16:01:06 GMT-0600 (MDT)", 
             "Sun Apr 19 2020 17:01:06 GMT-0600 (MDT)", 
             "Sun Apr 19 2020 17:01:06 GMT-0600 (MDT)")
HD1$date <- as.POSIXct(HD1$date, , "%a %b %d %Y %H:%M:%S")


############PLOTS##########################

#Color scheme for plots
colors3 <- c("dark grey","light blue","red")

#Height and width for plots
PlotWidth = 15
PlotHeight = 9

#ggplot theme to control formatting parameters for plots with month on the x-axis
PlotTheme = theme(axis.text=element_text(size=20),    #Text size for axis tick mark labels
                  axis.title.x=element_text(size=24, hjust=0.5, margin=margin(t=20, r=20, b=20, l=20)),               #Text size and alignment for x-axis label
                  axis.title.y=element_text(size=24, vjust=0.5,  margin=margin(t=20, r=20, b=20, l=20)),              #Text size and alignment for y-axis label
                  plot.title=element_text(size=26,face="bold",hjust=0.5, margin=margin(t=20, r=20, b=20, l=20)),      #Text size and alignment for plot title
                  legend.title=element_text(size=24),                                                                    #Text size of legend category labels
                  legend.text=element_text(size=20),                                                                   #Text size of legend title
                  legend.position = "right")        


#HD1_Temp Plot
PLOT="HD1_Temp"
ggplot(HD1, aes(x=date, y=temperature, group=1)) + geom_point(colour="blue", size=2) + labs(title="HD1 Air Temperature", x="Date", y="Temperature (C)") + PlotTheme 

ggsave(paste(PLOT,".png",sep=""), width = PlotWidth, height = PlotHeight) 



#Data wrangling for Blue Maestro sesnors (14 total) for WY20 snowmelt season


library(ggplot2)
library(lubridate)
library(dplyr)
library(stringr)

rm(list = ls()) 

############DATA PREP#############

#set working directory and csv file
setwd("C:/Users/sears/Documents/Research/Snow_Hydro_Research/BM Sensors/Field_Data_20210716/Processed/") #update wd
              
#######read csv, skip top 4 rows, use row 5 as header, remove columns 6:12
HD1 <- read.csv(file = "HD1.csv", skip=2, header=TRUE, stringsAsFactors = FALSE) 
#HD1 <-HD1[-c(6:12)]

#remove NA rows
#HD1<-na.omit(HD1)

HD1$Elevation <- 3059.04
HD1$ID <- "HD1"

#######read csv, skip top 2 rows, use row 5 as header, remove columns 6:12
HD2 <- read.csv(file = "HD2.csv", skip=2, header=TRUE, stringsAsFactors = FALSE) 
#HD2 <-HD2[-c(6:12)]

#remove NA rows
#HD2<-na.omit(HD2)

HD2$Elevation <- 3104.37
HD2$ID <- "HD2"

########read csv, skip top 4 rows, use row 5 as header, remove columns 6:12
HD3 <- read.csv(file = "HD3.csv", skip=2, header=TRUE, stringsAsFactors = FALSE) 
#HD3 <-HD3[-c(6:12)]

#remove NA rows
#HD2<-na.omit(HD2)

HD3$Elevation <- 3158.3
HD3$ID <- "HD3"

########read csv, skip top 4 rows, use row 5 as header, remove columns 6:12
HD4 <- read.csv(file = "HD4.csv", skip=2, header=TRUE, stringsAsFactors = FALSE) 
#HD4 <-HD4[-c(6:12)]

#remove NA rows
#HD2<-na.omit(HD2)

HD4$Elevation <- 3216.93
HD4$ID <- "HD4"

########read csv, skip top 4 rows, use row 5 as header, remove columns 6:12
HD5 <- read.csv(file = "HD5.csv", skip=2, header=TRUE, stringsAsFactors = FALSE) 
#HD4 <-HD4[-c(6:12)]

#remove NA rows
#HD2<-na.omit(HD2)

HD5$Elevation <- 3268.33
HD5$ID <- "HD5"

########read csv, skip top 4 rows, use row 5 as header, remove columns 6:12
HD6 <- read.csv(file = "HD6.csv", skip=2, header=TRUE, stringsAsFactors = FALSE) 
#HD4 <-HD4[-c(6:12)]

#remove NA rows
#HD2<-na.omit(HD2)

HD6$Elevation <- 3312.44
HD6$ID <- "HD6"

########read csv, skip top 4 rows, use row 5 as header, remove columns 6:12
HD7 <- read.csv(file = "HD7.csv", skip=2, header=TRUE, stringsAsFactors = FALSE) 
#HD4 <-HD4[-c(6:12)]

#remove NA rows
#HD2<-na.omit(HD2)

HD7$Elevation <- 3370.84
HD7$ID <- "HD7"

########read csv, skip top 4 rows, use row 5 as header, remove columns 6:12
HD8 <- read.csv(file = "HD8.csv", skip=2, header=TRUE, stringsAsFactors = FALSE) 
#HD4 <-HD4[-c(6:12)]

#remove NA rows
#HD2<-na.omit(HD2)

HD8$Elevation <- 3410.95
HD8$ID <- "HD8"

########read csv, skip top 4 rows, use row 5 as header, remove columns 6:12
HD9 <- read.csv(file = "HD9.csv", skip=2, header=TRUE, stringsAsFactors = FALSE) 
#HD4 <-HD4[-c(6:12)]

#remove NA rows
#HD2<-na.omit(HD2)

HD9$Elevation <- 3450.43
HD9$ID <- "HD9"

########read csv, skip top 4 rows, use row 5 as header, remove columns 6:12
MP1 <- read.csv(file = "MP1.csv", skip=2, header=TRUE, stringsAsFactors = FALSE) 
#HD4 <-HD4[-c(6:12)]

#remove NA rows
#HD2<-na.omit(HD2)

MP1$Elevation <- 3051.77
MP1$ID <- "MP1"

########read csv, skip top 4 rows, use row 5 as header, remove columns 6:12
MP2 <- read.csv(file = "MP2.csv", skip=2, header=TRUE, stringsAsFactors = FALSE) 
#HD4 <-HD4[-c(6:12)]

#remove NA rows
#HD2<-na.omit(HD2)

MP2$Elevation <- 3092.27
MP2$ID <- "MP2"

########read csv, skip top 4 rows, use row 5 as header, remove columns 6:12
MP3 <- read.csv(file = "MP3.csv", skip=2, header=TRUE, stringsAsFactors = FALSE) 
#HD4 <-HD4[-c(6:12)]

#remove NA rows
#HD2<-na.omit(HD2)

MP3$Elevation <- 3144.61
MP3$ID <- "MP3"

########read csv, skip top 4 rows, use row 5 as header, remove columns 6:12
MP4 <- read.csv(file = "MP4.csv", skip=2, header=TRUE, stringsAsFactors = FALSE) 
#HD4 <-HD4[-c(6:12)]

#remove NA rows
#HD2<-na.omit(HD2)

MP4$Elevation <- 3197.48
MP4$ID <- "MP4"

########read csv, skip top 4 rows, use row 5 as header, remove columns 6:12
MP5 <- read.csv(file = "MP5.csv", skip=2, header=TRUE, stringsAsFactors = FALSE) 
#HD4 <-HD4[-c(6:12)]

#remove NA rows
#HD2<-na.omit(HD2)

MP5$Elevation <- 3248.88
MP5$ID <- "MP5"

########read csv, skip top 4 rows, use row 5 as header, remove columns 6:12
MP6 <- read.csv(file = "MP6.csv", skip=2, header=TRUE, stringsAsFactors = FALSE) 
#HD4 <-HD4[-c(6:12)]

#remove NA rows
#HD2<-na.omit(HD2)

MP6$Elevation <- 3303.05
MP6$ID <- "MP6"

########read csv, skip top 4 rows, use row 5 as header, remove columns 6:12
MP7 <- read.csv(file = "MP7.csv", skip=2, header=TRUE, stringsAsFactors = FALSE) 
#HD4 <-HD4[-c(6:12)]

#remove NA rows
#HD2<-na.omit(HD2)

MP7$Elevation <- 3358.03
MP7$ID <- "MP7"

########read csv, skip top 4 rows, use row 5 as header, remove columns 6:12
MP8 <- read.csv(file = "MP8.csv", skip=2, header=TRUE, stringsAsFactors = FALSE) 
#HD4 <-HD4[-c(6:12)]

#remove NA rows
#HD2<-na.omit(HD2)

MP8$Elevation <- 3407.92
MP8$ID <- "MP8"

########read csv, skip top 4 rows, use row 5 as header, remove columns 6:12
MP9 <- read.csv(file = "MP9.csv", skip=2, header=TRUE, stringsAsFactors = FALSE) 
#MP9 <-MP9[-c(6:12)]
#remove empty rows
MP9<-na.omit(MP9)


MP9$Elevation <- 3453.61
MP9$ID <- "MP9"


MP_HD_All_Jul21 <- rbind(HD1, HD2, HD3, HD4, HD5, HD6, HD7, HD8, HD9, MP1, MP2, MP3, MP4, MP5, MP6, MP7, MP8, MP9)


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

#########################################################################
PLOT = "Lapse Rates - All"
ggplot(MP_HD_All_Jul21) + geom_point(aes(x=Elevation, y=temperature))

write.csv(MP_HD_All_Jul21, "MP_HD_All_Jul21.csv")


#lapse rate analysis for melt season 2020

#set working directory and csv file
setwd("C:/Users/sears/Documents/Research/Snow_Hydro_Research/Field_Data_071120/Processed/")
#read csv, skip top 4 rows, use row 5 as header, remove columns 6:12
HD1 <- read.csv(file = "HD1-62.csv", skip=4, header=TRUE, stringsAsFactors = FALSE) 
HD1 <-HD1[-c(6:12)]

#remove NA rows
HD1<-na.omit(HD1)
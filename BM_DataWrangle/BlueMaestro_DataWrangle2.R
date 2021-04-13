

library(dplyr)
library(lubridate)
library(tidyverse)
library(stringr)

rm(list = ls()) 

setwd("C:/Users/sears/Documents/Research/Snow_Hydro_Research/BM Sensors/Field_Data_041021/Processed/")

#read in all files. remove top two rows and several columns
temp = list.files(pattern="")
for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i], 
                                                   skip=2, header=TRUE, stringsAsFactors = FALSE))

addID <- function(dfList){
  for(i in 1:length(dfList)){
  df$ID = deparse(substitute(df))
  df$ID = str_remove(df$ID, ".csv")
  }
  return(df)
}

HD1 <- addID(HD1.csv)




library(RNRCS) # pull SNOTEL data
library(lubridate)
library(dplyr)
library(ggplot2)

#download Joe Wright SNOTEL data from WY 2010 to present and add as a df. Also, get date into correct format.
JW21_sno <- grabNRCS.data(network = "SNTL", site_id = 551, timescale = "daily", DayBgn = '2020-10-01', DayEnd = '2021-06-03') %>%
  mutate(Date = ymd(Date))

JW21_sno <- JW21_sno %>%
  mutate(dens = (Snow.Water.Equivalent..in..Start.of.Day.Values/Snow.Depth..in..Start.of.Day.Values)*100)

PLOT = "JW SNOTEL dens over time"
ggplot(data = subset(JW21_sno, !is.na(dens)), aes(x=Date, y=dens))+
  geom_line() + scale_x_date(date_breaks = "1 month")

#test


library(RNRCS) # pull SNOTEL data
library(lubridate)
library(dplyr)
library(ggplot2)
library(dataRetrieval)

#download Joe Wright SNOTEL data from WY 2010 to present and add as a df. Also, get date into correct format.
JW21 <- grabNRCS.data(network = "SNTL", site_id = 551, timescale = "hourly", DayBgn = '2021-04-01', DayEnd = '2021-07-01') %>%
  mutate(Date = ymd_hm(Date))

write.csv(JW21, "JW21.csv")

JW21_sno <- JW21_sno %>%
  mutate(dens = (Snow.Water.Equivalent..in..Start.of.Day.Values/Snow.Depth..in..Start.of.Day.Values)*100)

PLOT = "JW SNOTEL dens over time"
ggplot(data = subset(JW21_sno, !is.na(dens)), aes(x=Date, y=dens))+
  geom_line() + scale_x_date(date_breaks = "1 month")

#pull all JW SNOTEL data 
JWall_sno <- grabNRCS.data(network = "SNTL", site_id = 551, timescale = "daily", DayBgn = '1978-10-01', DayEnd = '2020-09-30') %>%
  mutate(Date = ymd(Date))

JWall_sno <- addWaterYear(JWall_sno)

#some random stats for 2.1 - peak SWE and total precip accum
JW_yravg <- JWall_sno %>%
  group_by(waterYear) %>%
  summarize(max_accumprecip = max(Precipitation.Accumulation..in..Start.of.Day.Values),
            maxSWE = max(Snow.Water.Equivalent..in..Start.of.Day.Values)) %>% 
  mutate(max_accumeprecip_mm = max_accumprecip*25.4,
         maxSWE_mm=maxSWE*25.4)

mean(JW_yravg$maxSWE_mm, na.rm=TRUE)
mean(JW_yravg$max_accumeprecip_mm, na.rm=TRUE)

JW_peakSWEdate <- JWall_sno %>%
  group_by(waterYear) %>%
  slice(which.max(Snow.Water.Equivalent..in..Start.of.Day.Values)) %>% 
  filter(waterYear >= 1980)

JW_peakSWEdate$moday <- format(JW_peakSWEdate$Date,"%j")
JW_peakSWEdate$moday <- as.numeric(JW_peakSWEdate$moday)

mean(JW_peakSWEdate$moday)

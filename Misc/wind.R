#looking at wind data

library(broom)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(dplyr)
library(plotly)
library(gridExtra)
library(scales)
library(RColorBrewer)
library(plotly)
library(tidyr)
library(tibble)
library(ggpubr)
library(patchwork)
library(devtools)
library(openair)

rm(list = ls())

#bringing in 2020 wind data
uz20 <- read.csv(file="C:/Users/sears/Documents/Research/Snow_Hydro_Research/Thesis/Data/Radiation/For R/uz_all.csv", 
                    header=TRUE) %>%
  mutate(Datetime = mdy_hm(Datetime)) %>%
  mutate(dt_agg = floor_date(Datetime, unit = "hour")) %>%
  group_by(dt_agg) %>%
  summarize(uz = mean(WS_ms),
            dir = mean(Dir)) %>%
  rename(Datetime = dt_agg) %>%
  filter(Datetime > ymd_hms("2020-04-30 23:00:00")) %>%
  filter(Datetime < ymd_hms("2020-07-01 00:00:00"))

#wind rose for 2020 snowmelt period
windRose(uz20, ws = "uz", wd = "dir", ws2 = NA, wd2 = NA,
         ws.int = 2, angle = 30, type = "default", bias.corr = TRUE, cols
         = "jet", grid.line = NULL, width = 1.5, seg = NULL, auto.text
         = TRUE, breaks = 6, offset = 10, normalise = FALSE, max.freq =
           NULL, paddle = FALSE, key.header = NULL, key.footer = "(m/s)",
         key.position = "bottom", key = list(height=1), dig.lab = 5, statistic =
           "prop.count", pollutant = NULL, annotate = TRUE, angle.scale =
           315, border = NA)

#bringing in 2021
uz21 <- read.csv(file="C:/Users/sears/Documents/Research/Snow_Hydro_Research/Thesis/Data/Radiation/For R/wind21.csv", 
                 header=TRUE) %>%
  mutate(Datetime = mdy_hm(Datetime)) %>%
  mutate(ws_ms = wspeed / 2.237)
  mutate(dt_agg = floor_date(Datetime, unit = "hour")) %>%
  group_by(dt_agg) %>%
  summarize(ws_ms = mean(WS_ms),
            dir = mean(Dir)) %>%
  rename(Datetime = dt_agg)

windRose(uz21, ws = "ws_ms", wd = "dir", ws2 = NA, wd2 = NA,
           ws.int = 2, angle = 30, type = "default", bias.corr = TRUE, cols
           = "jet", grid.line = NULL, width = 1.5, seg = NULL, auto.text
           = TRUE, breaks = 6, offset = 10, normalise = FALSE, max.freq =
             NULL, paddle = FALSE, key.header = NULL, key.footer = "(m/s)",
           key.position = "bottom", key = list(height=1), dig.lab = 5, statistic =
             "prop.count", pollutant = NULL, annotate = TRUE, angle.scale =
             315, border = NA)

#for sake of speed - run the dewpt_20&21 script and Temps_20and21 to get dataframes of values to compare to wind
uz21 <- merge(uz21, dew21, by="Datetime")

uz21 <- uz21 %>%
  select(c(Datetime, ws_ms, dir, slope_Ckm, r2)) %>%
  rename(dslope = slope_Ckm,
         dr2 = r2)

uz20 <- merge(uz20, dew20, by="Datetime")

uz20 <- uz20 %>%
  select(c(Datetime, uz, dir, slope_Ckm, r2)) %>%
  rename(dslope = slope_Ckm,
         dr2 = r2)

#above is dew pt - now bring in air temp
uz21 <- merge(uz21, T21_slope, by="Datetime")
uz20 <- merge(uz20, T20_slope, by="Datetime")

#now have df with tslope, dslope, corresponding R2, wind speed, and dir for 2021
uz21 <- uz21 %>%
  rename(tslope = Slope_degCkm,
         tr2 = R2) %>%
  select(-Slope_degCm)

#now have df with tslope, dslope, corresponding R2, wind speed, and dir for 2020
uz20 <- uz20 %>%
  rename(tslope = Slope_degCkm,
         tr2 = R2) %>%
  select(-Slope_degCm)



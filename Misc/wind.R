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

# #bringing in 2020 wind data
# uz20 <- read.csv(file="C:/Users/sears/Documents/Research/Snow_Hydro_Research/Thesis/Data/Radiation/For R/uz_all.csv", 
#                     header=TRUE) %>%
#   mutate(Datetime = mdy_hm(Datetime)) %>%
#   mutate(dt_agg = floor_date(Datetime, unit = "hour")) %>%
#   group_by(dt_agg) %>%
#   summarize(uz = mean(WS_ms),
#             dir = mean(Dir)) %>%
#   rename(Datetime = dt_agg) %>%
#   filter(Datetime > ymd_hms("2020-04-30 23:00:00")) %>%
#   filter(Datetime < ymd_hms("2020-07-01 00:00:00"))
# 
# #wind rose for 2020 snowmelt period
# windRose(uz20, ws = "uz", wd = "dir", ws2 = NA, wd2 = NA,
#          ws.int = 2, angle = 30, type = "default", bias.corr = TRUE, cols
#          = "jet", grid.line = NULL, width = 1.5, seg = NULL, auto.text
#          = TRUE, breaks = 6, offset = 10, normalise = FALSE, max.freq =
#            NULL, paddle = FALSE, key.header = NULL, key.footer = "(m/s)",
#          key.position = "bottom", key = list(height=1), dig.lab = 5, statistic =
#            "prop.count", pollutant = NULL, annotate = TRUE, angle.scale =
#            315, border = NA)
# 
# #bringing in 2021
# uz21 <- read.csv(file="C:/Users/sears/Documents/Research/Snow_Hydro_Research/Thesis/Data/Radiation/For R/wind21.csv", 
#                  header=TRUE) %>%
#   mutate(Datetime = mdy_hm(Datetime)) %>%
#   mutate(ws_ms = wspeed / 2.237)
#   mutate(dt_agg = floor_date(Datetime, unit = "hour")) %>%
#   group_by(dt_agg) %>%
#   summarize(ws_ms = mean(WS_ms),
#             dir = mean(Dir)) %>%
#   rename(Datetime = dt_agg)
# 
# windRose(uz21, ws = "ws_ms", wd = "dir", ws2 = NA, wd2 = NA,
#            ws.int = 2, angle = 30, type = "default", bias.corr = TRUE, cols
#            = "jet", grid.line = NULL, width = 1.5, seg = NULL, auto.text
#            = TRUE, breaks = 6, offset = 10, normalise = FALSE, max.freq =
#              NULL, paddle = FALSE, key.header = NULL, key.footer = "(m/s)",
#            key.position = "bottom", key = list(height=1), dig.lab = 5, statistic =
#              "prop.count", pollutant = NULL, annotate = TRUE, angle.scale =
#              315, border = NA)
# 
# #for sake of speed - run the dewpt_20&21 script and Temps_20and21 to get dataframes of values to compare to wind
# uz21 <- merge(uz21, dew21, by="Datetime")
# 
# uz21 <- uz21 %>%
#   select(c(Datetime, ws_ms, dir, slope_Ckm, r2)) %>%
#   rename(dslope = slope_Ckm,
#          dr2 = r2)
# 
# uz20 <- merge(uz20, dew20, by="Datetime")
# 
# uz20 <- uz20 %>%
#   select(c(Datetime, uz, dir, slope_Ckm, r2)) %>%
#   rename(dslope = slope_Ckm,
#          dr2 = r2)
# 
# #above is dew pt - now bring in air temp
# uz21 <- merge(uz21, T21_slope, by="Datetime")
# uz20 <- merge(uz20, T20_slope, by="Datetime")
# 
# #now have df with tslope, dslope, corresponding R2, wind speed, and dir for 2021
# uz21 <- uz21 %>%
#   rename(tslope = Slope_degCkm,
#          tr2 = R2) %>%
#   select(-Slope_degCm)
# 
# #now have df with tslope, dslope, corresponding R2, wind speed, and dir for 2020
# uz20 <- uz20 %>%
#   rename(tslope = Slope_degCkm,
#          tr2 = R2) %>%
#   select(-Slope_degCm)

#add a plot format for later
PlotFormat = theme(axis.text=element_text(size=18, color="black"),
                   axis.title.x=element_text(size=22, hjust=0.5, margin=margin(t=20, r=20, b=20, l=20), color="black"),              
                   axis.title.y=element_text(size=22, vjust=0.5,  margin=margin(t=20, r=20, b=20, l=20), color="black"),              
                   plot.title=element_text(size=26,face="bold",hjust=0.5, margin=margin(t=20, r=20, b=20, l=20)),      
                   legend.title=element_text(size=18, color="black"),                                                                    
                   legend.text=element_text(size=18, color="black"),                                                                   
                   legend.position = "right", 
                   panel.grid.major = element_line(colour = "grey80"),
                   #panel.grid.minor = element_line(colour = "grey80"),
                   #panel.grid.major = element_blank(), 
                   #panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   #axis.line = element_line(colour = "black"),
                   strip.text = element_text(size=28),
                   panel.border = element_rect(colour = "black", fill=NA, size=1),
                   legend.key=element_blank())


load("Misc/wind_data.RData")

windRose(uz20, ws = "uz", wd = "dir", ws2 = NA, wd2 = NA,
         ws.int = 2, angle = 30, type = "default", bias.corr = TRUE, cols
         = "jet", grid.line = NULL, width = 1.5, seg = NULL, auto.text
         = TRUE, breaks = 6, offset = 10, normalise = FALSE, max.freq =
           NULL, paddle = FALSE, key.header = NULL, key.footer = "(m/s)",
         key.position = "bottom", key = list(height=1), dig.lab = 5, statistic =
           "prop.count", pollutant = NULL, annotate = TRUE, angle.scale =
           315, border = NA)

windRose(uz21, ws = "ws_ms", wd = "dir", ws2 = NA, wd2 = NA,
         ws.int = 2, angle = 30, type = "default", bias.corr = TRUE, cols
         = "jet", grid.line = NULL, width = 1.5, seg = NULL, auto.text
         = TRUE, breaks = 6, offset = 10, normalise = FALSE, max.freq =
           NULL, paddle = FALSE, key.header = NULL, key.footer = "(m/s)",
         key.position = "bottom", key = list(height=1), dig.lab = 5, statistic =
           "prop.count", pollutant = NULL, annotate = TRUE, angle.scale =
           315, border = NA)

#define the wind directions
uz20 <- uz20 %>%
  mutate(aspect_dir = case_when(
    between(dir, 0, 22.5) ~"North",
    between(dir, 22.5, 67.5) ~ "Northeast",
    between(dir, 67.5, 112.5) ~ "East",
    between(dir, 112.5, 157.5) ~ "Southeast",
    between(dir, 157.5, 202.5) ~ "South",
    between(dir, 202.5, 247.5) ~ "Southwest",
    between(dir, 247.5, 292.5) ~ "West",
    between(dir, 292.5, 337.5) ~ "Northwest",
    between(dir, 337.5, 360) ~ "North"))

uz21 <- uz21 %>%
  mutate(aspect_dir = case_when(
    between(dir, 0, 22.5) ~"North",
    between(dir, 22.5, 67.5) ~ "Northeast",
    between(dir, 67.5, 112.5) ~ "East",
    between(dir, 112.5, 157.5) ~ "Southeast",
    between(dir, 157.5, 202.5) ~ "South",
    between(dir, 202.5, 247.5) ~ "Southwest",
    between(dir, 247.5, 292.5) ~ "West",
    between(dir, 292.5, 337.5) ~ "Northwest",
    between(dir, 337.5, 360) ~ "North"))

#filter for each year and variable for r2 > 0.2
uz20dew <- uz20 %>%
  filter(dr2 > 0.2) %>%
  mutate(sign = ifelse(dslope > 0, "positive DTEG",
                       "negative DTEG"))

uz20t <- uz20 %>%
  filter(tr2 > 0.2) %>%
  mutate(sign = ifelse(dslope > 0, "positive TEG",
                       "negative TEG"))

uz21dew <- uz21 %>%
  filter(dr2 > 0.2) %>%
  mutate(sign = ifelse(dslope > 0, "positive DTEG",
                                          "negative DTEG"))

uz21t <- uz21 %>%
  filter(tr2 > 0.2) %>%
  mutate(sign = ifelse(dslope > 0, "positive TEG",
                       "negative TEG"))

uz_t <- bind_rows(uz21t, uz20t)

uz_d <- bind_rows(uz21dew, uz20dew)


PLOT="uzdew_aspectboxplot"
ggplot(uz_d, aes(x=aspect_dir, y=dslope, fill=sign)) +
  geom_boxplot()+
  ylim(-30,30) +
  facet_grid(~year) +
  scale_fill_manual(values=c("red", "light blue")) +
  PlotFormat +
  labs(x="Wind Direction", y=expression("DTEG " (degree*C/km)),
       fill="") +
  theme(legend.position = "bottom")

ggsave(paste(PLOT,".png",sep=""), width = 15, height = 9)

PLOT="uzt_aspectboxplot"
ggplot(uz_t, aes(x=aspect_dir, y=tslope, fill=sign)) +
  geom_boxplot()+
  facet_grid(~year) +
  scale_fill_manual(values=c("red", "light blue")) +
  PlotFormat +
  labs(x="Wind Direction", y=expression("TEG " (degree*C/km)),
       fill="") +
  theme(legend.position = "bottom")

ggsave(paste(PLOT,".png",sep=""), width = 15, height = 9)


knitr::opts_chunk$set(message = FALSE, warning = FALSE, fig.show = "animate")
library(gganimate)
library(ggmap)
library(ggplot2)
library(lubridate)
library(tidyverse)

source("scripts/alltruth_clustering.r")

### load clustered tweet data
accidents <- truth.geo.df

### plot accidents by month
ggplot(accidents) +
  geom_bar(aes(x = month), color = "black", fill = "skyblue") +
  theme_classic() + 
  ggtitle("Geocoded Accidents in Nairobi by Month") +
  theme(plot.title = element_text(lineheight = 0.5, hjust = 0.5))

### plot accidents by day of week
ggplot(accidents) +
  geom_bar(aes(x = day), color = "black", fill = "skyblue") +
  theme_classic() + 
  ggtitle("Geocoded Accidents in Nairobi by Day of the Week") +
  theme(plot.title = element_text(lineheight = 0.5, hjust = 0.5))

### plot accidents by hour of day
ggplot(accidents) +
  geom_bar(aes(x = hour), color = "black", fill = "skyblue") +
  theme_classic() + 
  ggtitle("Geocoded Accidents in Nairobi by Hour of Day") +
  theme(plot.title = element_text(lineheight = 0.5, hjust = 0.5))

## plot accidents by hour of day and fill by day
# The palette with grey:
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ggplot(accidents) +
  geom_bar(aes(x = hour, fill = day), color = "black") +
  scale_fill_manual(values=cbPalette, name = "Day of Week") +
  theme_classic() + 
  ggtitle("Geocoded Accidents in Nairobi by Hour of Day and Day of Week") +
  theme(plot.title = element_text(lineheight = 0.5, hjust = 0.5))

### map twitter accidents
map <- get_googlemap(center = c(lon = 36.8219, lat = -1.2921), zoom = 12, scale = 2, maptype = "hybrid")

ggmap(map, extent = "device") +
  geom_point(data = accidents, aes(x = lon.wgs84, y = lat.wgs84, colour = day), 
             alpha = 1, size = 2) +
  scale_colour_manual(values=cbPalette, name = "Day of Week")

### map twitter accidents with day of week animation
accidents$day_int <- as.numeric(accidents$day)

p <- ggmap(map, extent = "device") +
  geom_point(data = accidents, aes(x = lon.wgs84, y = lat.wgs84, colour = day, frame = day_int), 
             alpha = 1, size = 3) +
  scale_colour_manual(values=cbPalette, name = "Day of Week")

gganimate(p, filename = "visualizations/crash_map.gif", title_frame = FALSE, 
          ani.width = 700, interval = 2)


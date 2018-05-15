# clusters all collected data since November 2017
library(raster)
library(sp)
library(tidyverse)

truth.df <- read_csv("tweets/truthdata_allrounds_withtime.csv") %>%
  mutate(date = mdy_hm(tweet_nairobi_time, tz = "Africa/Nairobi")) %>%
  mutate(year = year(date)) %>%
  mutate(month = month(date, label = TRUE)) %>%
  mutate(day = wday(date, label = TRUE)) %>%
  mutate(hour = hour(date))

##### Cluster
# Spatially Project
truth.geo.df <- truth.df[!is.na(truth.df$latitude_truth),]
truth.geo.df <- truth.df[!is.na(truth.df$longitude_truth),]

truth.geo.df$lat.wgs84 <- truth.geo.df$latitude_truth
truth.geo.df$lon.wgs84 <- truth.geo.df$longitude_truth
coordinates(truth.geo.df) <- ~longitude_truth + latitude_truth
crs(truth.geo.df) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
equal.distant.projection <- paste("+proj=aeqd +lat_0=",-1.283333," +lon_0=",36.816667, sep="")

# filter out points outside of Kenya
counties <- c("Nairobi", "Kiambu", "Murang'a", "Kajiado", "Machakos")
ke_counties <- getData("GADM", level = 1, country = "KEN")
ke_county <- ke_counties[ke_counties@data$NAME_1 %in% counties,]

ke_country <- getData("GADM", level = 0, country = "KEN")
over_truth <- over(truth.geo.df, ke_country)
truth.geo.df <- truth.geo.df[!is.na(over_truth$OBJECTID),]

truth.geo.df <- spTransform(truth.geo.df, CRS(equal.distant.projection)) 
truth.geo.df$lon <- coordinates(truth.geo.df)[,1]
truth.geo.df$lat <- coordinates(truth.geo.df)[,2]
truth.geo.df <- truth.geo.df@data

# Implement Clustering
truth.geo.df$id <- 1:nrow(truth.geo.df)
truth.geo.df$cluster.id <- NA
cluster_km <- 2

for(i in 1:nrow(truth.geo.df)){
  truth.geo.df.i <- truth.geo.df[i,]
  within.time <- abs((truth.geo.df.i$date - truth.geo.df$date)) <= 30*60
  within.distance <- sqrt((truth.geo.df.i$lon - truth.geo.df$lon)^2 + (truth.geo.df.i$lat - truth.geo.df$lat)^2) <= (cluster_km*1000)
  truth.geo.df$cluster.id[within.time & within.distance] <- truth.geo.df.i$id
}

rm(truth.df, truth.geo.df.i, cluster_km, equal.distant.projection, i, 
   within.distance, within.time, ke_country, over_truth, counties, ke_counties, ke_county)




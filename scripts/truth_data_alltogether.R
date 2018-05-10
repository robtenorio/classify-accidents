# Merge Truth Datasets into Timeseries

file.path <- "~/Dropbox/World Bank/CrashMap-Nairobi/"
width=12
height=6

library(readxl)
library(doBy)
library(dplyr)
library(plyr)
library(sp)
library(rgdal)
library(rgeos)
library(raster)

# Load and Merge Truth Datasets ------------------------------------------------
truth.r1 <- read.csv(paste(file.path, "Truth Datasets/Truth Data Round 1/truthdata_2_1_2018.csv", sep="")) %>%
  subset(select=c(id, tweet, accident_truth, latitude_truth, longitude_truth, street_name_am, street_name_rk, landmark_am, landmark_rk)) %>%
  dplyr::rename(street_name_c1 = street_name_am) %>%
  dplyr::rename(street_name_c2 = street_name_rk) %>%
  dplyr::rename(landmark_c1 = landmark_am) %>%
  dplyr::rename(landmark_c2 = landmark_rk)
truth.r2 <- read.csv(paste(file.path, "Truth Datasets/Truth Data Round 2/Final Truth Dataset/truth_r2.csv", sep="")) %>%
  subset(select=c(id, tweet, accident_truth, latitude_truth, longitude_truth, street_name_c1, street_name_c2, landmark_c1, landmark_c2))
truth.r3 <- read.csv(paste(file.path, "Truth Datasets/Truth Data Round 3/Final Truth Dataset/truth_r3.csv", sep="")) %>%
  subset(select=c(id, tweet, accident_truth, latitude_truth, longitude_truth, street_name_c1, street_name_c2, landmark_c1, landmark_c2))
truth.r4 <- read.csv(paste(file.path, "Truth Datasets/Truth Data Round 4/Final Truth Dataset/truth_r4.csv", sep="")) %>%
  subset(select=c(id, tweet, accident_truth, latitude_truth, longitude_truth, street_name_c1, street_name_c2, landmark_c1, landmark_c2))
truth.r5 <- read.csv(paste(file.path, "Truth Datasets/Truth Data Round 5/Final Truth Dataset/truth_r5.csv", sep="")) %>%
  subset(select=c(id, tweet, accident_truth, latitude_truth, longitude_truth, street_name_c1, street_name_c2, landmark_c1, landmark_c2))

truth.r6.s <- read_excel(paste(file.path,"Truth Datasets/Truth Data Round 6/Coders Work/tweets_to_classify_r6_salome.xlsx",sep=""))
truth.r6.a <- read_excel(paste(file.path,"Truth Datasets/Truth Data Round 6/Coders Work/tweets_to_classify_r6_andrew.xlsx",sep=""))
truth.r6.s <- truth.r6.s[-(1:2),] %>% as.data.frame
truth.r6.a <- truth.r6.a[-(1:2),] %>% as.data.frame
names(truth.r6.s) <- paste(c("id","coder_c1","tweet","accident_truth","street_name_c1","landmark_c1","exact_location_c1","latitude_truth","longitude_truth"),sep="")
names(truth.r6.a) <- paste(c("id","coder_c1","tweet","accident_truth","street_name_c1","landmark_c1","exact_location_c1","latitude_truth","longitude_truth"),sep="")
truth.r6 <- rbind(truth.r6.s,truth.r6.a)
truth.r6$latitude_truth <- as.numeric(truth.r6$latitude_truth)
truth.r6$longitude_truth <- as.numeric(truth.r6$longitude_truth)

truth.df <- rbind.fill(truth.r3, truth.r4, truth.r5, truth.r6)

# Merge in Tweet Times ---------------------------------------------------------
tweets.raw.r3.all <- read.csv(paste(file.path, "Truth Datasets/Truth Data Round 3/Tweets to Code/feb2018_all.csv", sep=""))
tweets.raw.r3.all$date <- as.character(tweets.raw.r3.all$date)
tweets.raw.r3.all <- subset(tweets.raw.r3.all, select=c(notification_id, date))
tweets.raw.r3 <- read.csv(paste(file.path, "Truth Datasets/Truth Data Round 3/Tweets to Code/feb2018.csv", sep=""))
tweets.raw.r3 <- subset(tweets.raw.r3, select=c(notification_id,tweet_id))
tweets.raw.r3 <- merge(tweets.raw.r3, tweets.raw.r3.all, by="notification_id")
tweets.raw.r3 <- subset(tweets.raw.r3, select=c(tweet_id,date))
names(tweets.raw.r3) <- c("id","tweet_nairobi_time")

tweets.raw.r4 <- read.csv(paste(file.path, "Truth Datasets/Truth Data Round 4/Tweets to Code/ma3map_posts_3_12_TO_3_23_TOCODE.csv", sep=""))
tweets.raw.r4 <- subset(tweets.raw.r4, select=c(id, tweet_nairobi_time))

tweets.raw.r5 <- read_excel(paste(file.path, "Truth Datasets/Truth Data Round 5/Tweets to Code/TOCODE.xlsx", sep="")) %>% as.data.frame
tweets.raw.r5 <- subset(tweets.raw.r5, select=c(id, tweet_nairobi_time))

tweets.raw.r6 <- read_excel(paste(file.path, "Truth Datasets/Truth Data Round 6/Tweets to Code/TOCODE.xlsx", sep="")) %>% as.data.frame
tweets.raw.r6 <- subset(tweets.raw.r6, select=c(id, tweet_nairobi_time))

# Make all time variables consistent between datasets
#r3
tweets.raw.r3$tweet_nairobi_time <- substring(tweets.raw.r3$tweet_nairobi_time, 1,19)
tweets.raw.r3$tweet_nairobi_time <- as.character(tweets.raw.r3$tweet_nairobi_time)

#r4
tweets.raw.r4$tweet_nairobi_time <- as.character(tweets.raw.r4$tweet_nairobi_time)
r4.time <- substring(tweets.raw.r4$tweet_nairobi_time,9,13)
r4.time[nchar(r4.time) %in% 4] <- paste("0",r4.time[nchar(r4.time) %in% 4],sep="")
tweets.raw.r4$tweet_nairobi_time <- paste("2018-03-",
                                      substring(tweets.raw.r4$tweet_nairobi_time,3,4),
                                      " ",
                                      r4.time,
                                      ":00",
                                      sep="")
tweets.raw.r4$tweet_nairobi_time <- as.character(tweets.raw.r4$tweet_nairobi_time)

#r5
tweets.raw.r5$tweet_nairobi_time <- as.character(tweets.raw.r5$tweet_nairobi_time)

#r6
tweets.raw.r6$tweet_nairobi_time <- as.character(tweets.raw.r6$tweet_nairobi_time)

# Append All Tweet Times
tweets.time <- rbind(tweets.raw.r3,tweets.raw.r4,tweets.raw.r5,tweets.raw.r6)

# Merge Tweet Times with Tweets
truth.df <- merge(truth.df, tweets.time, by="id")

# Prep Data for Graphs ---------------------------------------------------------
truth.df <- truth.df[truth.df$accident_truth %in% "Yes",]
truth.df$year_month_day <- substring(truth.df$tweet_nairobi_time,1,10)

# Dates Dataframe to Merge Into
date.to.string <- function(val) if(val < 10) paste("0",val,sep="") else paste(val)
dates.df <- c(
  paste(rep(2017, 31),"-",lapply(rep(12, 31), date.to.string) %>% unlist,"-",lapply(1:31, date.to.string) %>% unlist, sep=""),
  paste(rep(2018, 31),"-",lapply(rep(1, 31), date.to.string) %>% unlist,"-",lapply(1:31, date.to.string) %>% unlist, sep=""),
  paste(rep(2018, 28),"-",lapply(rep(2, 28), date.to.string) %>% unlist,"-",lapply(1:28, date.to.string) %>% unlist, sep=""),
  paste(rep(2018, 31),"-",lapply(rep(3, 31), date.to.string) %>% unlist,"-",lapply(1:31, date.to.string) %>% unlist, sep=""),
  paste(rep(2018, 30),"-",lapply(rep(4, 30), date.to.string) %>% unlist,"-",lapply(1:30, date.to.string) %>% unlist, sep="")
)

dates.df <- as.data.frame(dates.df)
names(dates.df) <- "year_month_day"
dates.df$year_month_day <- as.character(dates.df$year_month_day)

# Prep Data for Analysis -------------------------------------------------------

##### Format Date Variables
truth.df$tweet_nairobi_time <- as.character(truth.df$tweet_nairobi_time)
truth.df$hour <- as.numeric(substr(truth.df$tweet_nairobi_time, 12,13))
truth.df$minute <- as.numeric(substr(truth.df$tweet_nairobi_time, 15,16))
truth.df$second <- as.numeric(substr(truth.df$tweet_nairobi_time, 18,19))
truth.df$year_month_day <- substr(truth.df$tweet_nairobi_time, 1,10)
truth.df$seconds.since.base <- as.numeric(as.POSIXct(truth.df$tweet_nairobi_time, tryFormats = c("%Y-%m-%d %H:%M:%OS")))

##### Cluster
# Spatially Project
truth.geo.df <- truth.df[!is.na(truth.df$latitude_truth),]
truth.geo.df <- truth.df[!is.na(truth.df$longitude_truth),]

truth.geo.df$lat.wgs84 <- truth.geo.df$latitude_truth
truth.geo.df$lon.wgs84 <- truth.geo.df$longitude_truth
coordinates(truth.geo.df) <- ~longitude_truth + latitude_truth
crs(truth.geo.df) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
equal.distant.projection <- paste("+proj=aeqd +lat_0=",-1.283333," +lon_0=",36.816667, sep="")
truth.geo.df <- spTransform(truth.geo.df, CRS(equal.distant.projection)) 
truth.geo.df$lon <- coordinates(truth.geo.df)[,1]
truth.geo.df$lat <- coordinates(truth.geo.df)[,2]
truth.geo.df <- truth.geo.df@data

# Implement Clustering
truth.geo.df$id <- 1:nrow(truth.geo.df)
truth.geo.df$cluster.id <- NA
cluster_minutes <- 60*5
cluster_km <- 2

for(i in 1:nrow(truth.geo.df)){
  truth.geo.df.i <- truth.geo.df[i,]
  within.time <- abs((truth.geo.df.i$seconds.since.base - truth.geo.df$seconds.since.base)) <= (cluster_minutes * 60)
  within.distance <- sqrt((truth.geo.df.i$lon - truth.geo.df$lon)^2 + (truth.geo.df.i$lat - truth.geo.df$lat)^2) <= (cluster_km*1000)
  truth.geo.df$cluster.id[within.time & within.distance] <- truth.geo.df.i$id
}

##### Export Cluster Dataset
truth.geo.cluster.df <- summaryBy(lon.wgs84+lat.wgs84 ~ year_month_day+cluster.id, data=truth.geo.df, FUN=mean, keep.names=T)
write.csv(truth.geo.cluster.df, "~/Desktop/accidents_clustered.csv")

##### Calculating Relevant Variables
dates.df$accidents.tweets <- lapply(dates.df$year_month_day, function(d) truth.df$id[truth.df$year_month_day %in% d] %>% length) %>% unlist
dates.df$geocoded.accidents.tweets <- lapply(dates.df$year_month_day, function(d) truth.geo.df$cluster.id[truth.geo.df$year_month_day %in% d] %>% length) %>% unlist
dates.df$unique.geocoded.accidents <- lapply(dates.df$year_month_day, function(d) truth.geo.df$cluster.id[truth.geo.df$year_month_day %in% d] %>% unique %>% length) %>% unlist

##### Week Variable
dates.df <- dates.df[order(dates.df$year_month_day),] 

campaign.time <- (dates.df$year_month_day >= "2018-03-12") & (dates.df$year_month_day <= "2018-04-26")
week.num <- rep(1:ceiling(nrow(dates.df[campaign.time,]) / 7), each = 7, times = 1)
week.num <- week.num[1:nrow(dates.df[campaign.time,])]
dates.df$week.num[campaign.time] <- paste0("post_",week.num)

pre.campaign.time <- (dates.df$year_month_day >= "2018-02-01") & (dates.df$year_month_day <= "2018-02-14")
week.num <- rep(1:ceiling(nrow(dates.df[pre.campaign.time,]) / 7), each = 7, times = 1)
week.num <- week.num[1:nrow(dates.df[pre.campaign.time,])]
dates.df$week.num[pre.campaign.time] <- paste0("pre_",week.num)

week <- 7

cat(
round(mean(dates.df$accidents.tweets[dates.df$week.num %in% paste0("post_",week)]),2),
" [",
min(dates.df$accidents.tweets[dates.df$week.num %in% paste0("post_",week)]),
", ",
max(dates.df$accidents.tweets[dates.df$week.num %in% paste0("post_",week)]),
"]",
sep=""
)

cat(
  round(mean(dates.df$geocoded.accidents.tweets[dates.df$week.num %in% paste0("post_",week)]),2),
  " [",
  min(dates.df$geocoded.accidents.tweets[dates.df$week.num %in% paste0("post_",week)]),
  ", ",
  max(dates.df$geocoded.accidents.tweets[dates.df$week.num %in% paste0("post_",week)]),
  "]",
  sep=""
)

cat(
  round(mean(dates.df$unique.geocoded.accidents[dates.df$week.num %in% paste0("post_",week)]),2),
  " [",
  min(dates.df$unique.geocoded.accidents[dates.df$week.num %in% paste0("post_",week)]),
  ", ",
  max(dates.df$unique.geocoded.accidents[dates.df$week.num %in% paste0("post_",week)]),
  "]",
  sep=""
)


dates.df$year_month_day <- as.Date(dates.df$year_month_day, "%Y-%m-%d")

dates.df$accidents.tweets[is.na(dates.df$week.num)] <- NA
dates.df$geocoded.accidents.tweets[is.na(dates.df$week.num)] <- NA
dates.df$unique.geocoded.accidents[is.na(dates.df$week.num)] <- NA

dates.df <- dates.df[dates.df$year_month_day >= "2018-02-01",]

ggplot() +
  geom_line(data=dates.df, aes(year_month_day, unique.geocoded.accidents)) +
  geom_point(data=dates.df, aes(year_month_day, unique.geocoded.accidents)) +
  theme_minimal() + 
  labs(x="",y="",title="Number of Unique Geocoded Accidents")

summary(dates.df$unique.geocoded.accidents[dates.df$year_month_day >= "2018-03-12"])


dates.week.df <- summaryBy(accidents.tweets+geocoded.accidents.tweets+unique.geocoded.accidents ~ week.num,
                           data=dates.df,FUN=mean,keep.names=T)
dates.week.df$discount <- dates.week.df$unique.geocoded.accidents / dates.week.df$geocoded.accidents.tweets
dates.week.df$potential.total.unique.accidents <- dates.week.df$accidents.tweets * dates.week.df$discount
dates.week.df$proportion.unique.geocoded <- dates.week.df$unique.geocoded.accidents / dates.week.df$potential.total.unique.accidents

dates.week.df.week.i <- dates.week.df[dates.week.df$week.num %in% "post_7",]
round(dates.week.df.week.i$discount,2)
round(dates.week.df.week.i$potential.total.unique.accidents,2)
round(dates.week.df.week.i$proportion.unique.geocoded,2)

##### Accident Tweets Not Geocoded
truth.accident.nogeo.df <- truth.df[is.na(truth.df$latitude_truth),]
write.csv(truth.accident.nogeo.df, "~/Desktop/accident_nogeo.csv")
# "-----------------------------------------------------------------------------
# PURGATORY --------------------------------------------------------------------
# "-----------------------------------------------------------------------------













# Prep Data for Analysis -------------------------------------------------------
truth.df$geocoded <- FALSE
truth.df$geocoded[!is.na(truth.df$longitude_truth)] <- TRUE
truth.df$geocoded.true[truth.df$geocoded==TRUE] <- 1
truth.df$geocoded.false[truth.df$geocoded==FALSE] <- 1

tweets.geocoded.df <- summaryBy(geocoded.true+geocoded.false ~ year_month_day, data=truth.df, keep.names=T, FUN=sum, na.rm=T)

tweets.geocoded.df <- rbind(cbind(tweets.geocoded.df$year_month_day, tweets.geocoded.df$geocoded.true) %>% as.data.frame %>% mutate(type="geocoded"),
                            cbind(tweets.geocoded.df$year_month_day, tweets.geocoded.df$geocoded.false) %>% as.data.frame %>% mutate(type=" notgeocoded"))
       
names(tweets.geocoded.df) <- c("year_month_day","N","type") 
tweets.geocoded.df$N <- as.numeric(as.character(tweets.geocoded.df$N))
tweets.geocoded.df$year_month_day <- as.character(tweets.geocoded.df$year_month_day)

tweets.geocoded.df <- merge(dates.df,tweets.geocoded.df,by="year_month_day",all.x=T)
tweets.geocoded.df$year_month_day <- as.character(tweets.geocoded.df$year_month_day)
tweets.geocoded.df <- tweets.geocoded.df[tweets.geocoded.df$year_month_day >= "2018-02-01",]
tweets.geocoded.df <- tweets.geocoded.df[tweets.geocoded.df$year_month_day <= "2018-04-18",]

tweets.geocoded.df$year_month_day <- as.Date(tweets.geocoded.df$year_month_day, "%Y-%m-%d")

acc_tweet_bar <- ggplot() + 
  geom_col(data=tweets.geocoded.df, aes(year_month_day, N, fill=type)) +
  geom_vline(xintercept = 3.5, color="red") +
  labs(x="",
       y="",
       title="Accident Tweets: All vs Geocoded",
       fill="Accident Tweets") +
  scale_fill_manual(values=c("orange3","skyblue4"), 
                    labels=c("All Tweets","Geocoded")) +
  theme_minimal()
ggsave(plot=acc_tweet_bar, filename=paste(file.path, "Analyze Scraped Tweets/Results/Figures/tweets_acc_geo_truth.png",sep=""), width=width, height=height)

# Cluster ----------------------------------------------------------------------

# Format Date Variables
truth.df$tweet_nairobi_time <- as.character(truth.df$tweet_nairobi_time)
truth.df$hour <- as.numeric(substr(truth.df$tweet_nairobi_time, 12,13))
truth.df$minute <- as.numeric(substr(truth.df$tweet_nairobi_time, 15,16))
truth.df$second <- as.numeric(substr(truth.df$tweet_nairobi_time, 18,19))
truth.df$year_month_day <- substr(truth.df$tweet_nairobi_time, 1,10)
truth.df$seconds.since.base <- as.numeric(as.POSIXct(truth.df$tweet_nairobi_time, tryFormats = c("%Y-%m-%d %H:%M:%OS")))

# Spatially Project
truth.df <- truth.df[!is.na(truth.df$latitude_truth),]
truth.df <- truth.df[!is.na(truth.df$longitude_truth),]

truth.df$lat.wgs84 <- truth.df$latitude_truth
truth.df$lon.wgs84 <- truth.df$longitude_truth
coordinates(truth.df) <- ~longitude_truth + latitude_truth
crs(truth.df) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
equal.distant.projection <- paste("+proj=aeqd +lat_0=",-1.283333," +lon_0=",36.816667, sep="")
truth.df <- spTransform(truth.df, CRS(equal.distant.projection)) 
truth.df$lon <- coordinates(truth.df)[,1]
truth.df$lat <- coordinates(truth.df)[,2]
truth.df <- truth.df@data

# ID Variable
truth.df$id <- 1:nrow(truth.df)

truth.df$cluster.id <- NA
cluster_minutes <- 60*5
cluster_km <- 2

for(i in 1:nrow(truth.df)){
  truth.df.i <- truth.df[i,]
  within.time <- abs((truth.df.i$seconds.since.base - truth.df$seconds.since.base)) <= (cluster_minutes * 60)
  within.distance <- sqrt((truth.df.i$lon - truth.df$lon)^2 + (truth.df.i$lat - truth.df$lat)^2) <= (cluster_km*1000)
  truth.df$cluster.id[within.time & within.distance] <- truth.df.i$id
}

truth.df$N <- 1
truth.num.geo.df <- summaryBy(N ~ year_month_day, keep.names=T, FUN=sum, data=truth.df) %>% mutate(type=" Geo Tweets")

truth.num.geo.acc.df <- subset(truth.df, select=c(year_month_day, cluster.id)) %>% unique
truth.num.geo.acc.df$N <- 1
truth.num.geo.acc.sum.df <- summaryBy(N~year_month_day, data=truth.num.geo.acc.df, keep.names=T, FUN=sum) %>% mutate(type="Geo Accidents")

truth.num.geo.df$N <- truth.num.geo.df$N - truth.num.geo.acc.sum.df$N

cluster.df <- rbind(truth.num.geo.df, truth.num.geo.acc.sum.df)
cluster.df <- merge(cluster.df, dates.df, by="year_month_day", all.x=T)
cluster.df$year_month_day <- as.character(cluster.df$year_month_day)
cluster.df <- cluster.df[cluster.df$year_month_day >= "2018-02-01",]
cluster.df <- cluster.df[cluster.df$year_month_day <= "2018-04-18",]

cluster.df$year_month_day <- as.Date(cluster.df$year_month_day, "%Y-%m-%d")

acc_tweet_cluster_bar <- ggplot() +
  geom_col(data=cluster.df, aes(year_month_day, N, fill=type)) +
  labs(x="", y="",
       title="Geocoded Accidents Tweets vs. Unique Geocoded Accidents",
       fill="Type") + 
  scale_fill_manual(values=c("orange3","skyblue4"), 
                    labels=c("Geocoded Accident Tweets","Unique Geocoded Accidents")) +
  theme_minimal()
ggsave(plot=acc_tweet_cluster_bar, filename=paste(file.path, "Analyze Scraped Tweets/Results/Figures/tweets_acc_geo_cluster_truth.png",sep=""), width=width, height=height)



geo.accs <- cluster.df[cluster.df$type == "Geo Accidents",]
write.csv(geo.accs,"~/Desktop/geotweets.csv")
summary(geo.accs$N)
summary(geo.accs$N[as.character(geo.accs$year_month_day) < "2018-03-12"])
summary(geo.accs$N[as.character(geo.accs$year_month_day) >= "2018-04-01"])

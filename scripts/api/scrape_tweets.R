# Title: Scrape Tweets from Ma3Route
# Description: Load CSV of past tweets and scrape tweets since the lastest tweet 
# in the CSV. Update the CSV with new tweets.

# Documentation:
# https://www.rdocumentation.org/packages/smappR/versions/0.5/topics/getTimeline
# http://rtweet.info/
# https://developer.twitter.com/en/docs/tweets/timelines/guides/working-with-timelines

# To get ALL tweets
# https://stackoverflow.com/questions/8471489/find-all-tweets-from-a-user-not-just-the-first-3-200
# https://stackoverflow.com/questions/8471489/find-all-tweets-from-a-user-not-just-the-first-3-200

# NOTES
# 1. since_id wasn't working.
#    Current workaround is to: (1) import historic tweets, (2) find max id 
#    (3) load most recent 3200 tweets (4) keep those with ids greater than max_id

# Packages and Filepaths -------------------------------------------------------
file.path <- "~/Dropbox/World Bank/CrashMap-Nairobi/"
library(rtweet)

# Set Twitter API Tokens -------------------------------------------------------
appname <- "Ma3Map"
key <- "WnAFXFzY9G6R1Euq7oKcpFbfJ"
secret <- "uFtOmM7AKJK4bdIfHikqdwZ9I9fu6pXNGCkE9o2E0nOqKR2xSU"
twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret)

# Load Past Tweets -------------------------------------------------------------
ma3route.df <- read.csv(paste(file.path, "Scraper/Tweets/ma3route_tweets.csv",sep=""))
ma3route.df <- subset(ma3route.df, select=-c(X))

historic_tweets_max_id <- max(ma3route.df$status_id)

# Ma3Route API -----------------------------------------------------------------
ma3route.recent <- rtweet::get_timeline("Ma3Route", n=3200, tweet_mode="extended")
ma3route.recent.df <- as.data.frame(ma3route.recent)
#table(grepl("more at", ma3route.recent.df$text))
#tweet_mode='extended'

# Subset to new tweets
ma3route.recent.df$status_id <- as.numeric(ma3route.recent.df$status_id)
ma3route.recent.df <- ma3route.recent.df[ma3route.recent.df$status_id > historic_tweets_max_id,]

# Format Data ------------------------------------------------------------------
for(var in names(ma3route.recent.df)){
  ma3route.recent.df[[var]] <- as.character(ma3route.recent.df[[var]])
}

# Append Historic and Recent Tweets --------------------------------------------
ma3route.new.df <- rbind(ma3route.recent.df, ma3route.df)
write.csv(ma3route.new.df, paste(file.path, "Scraper/Tweets/ma3route_tweets.csv",sep=""))







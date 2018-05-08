# Uploads New Accident Tweets to Google Sheets.
# Either creates a new sheet for the day or updates an existing sheet. [1 sheet per day]
# https://docs.google.com/spreadsheets/d/1KO_IkxYfnpb4EcsVmtAtIgoe1t-kPegDDUrB8iqfjqU/edit#gid=0
# Account: robmarty3@gmail.com

# Packages and Parameters ------------------------------------------------------
file.path <- "~/Dropbox/World Bank/CrashMap-Nairobi/"

library(googlesheets)
library(dplyr)
source(paste(file.path, "Algorithm/code/Main Algorithm/crashmap_algorithm_clean.R",sep=""))

# Get Date in Nairobi ----------------------------------------------------------
current.time <- Sys.time()
attributes(current.time)$tzone <- "Africa/Nairobi" 
tweet.date <- as.character(current.time) %>%
  substring(1,10)

# Manually specify date
# tweet.date <- "2018-05-04" # yyyy-mm-dd [should be NAIROBI day; account for time change when looping through]

# Scrape Tweets ----------------------------------------------------------------
source(paste(file.path, "Scraper/R Code/scrape_tweets.R",sep=""))

# Load and Prep Scraped Data ---------------------------------------------------
ma3route.df <- read.csv(paste(file.path, "Scraper/Tweets/ma3route_tweets.csv",sep=""))
ma3route.df <- ma3route.df[ma3route.df$source %in% "Ma3Route",]
ma3route.df <- subset(ma3route.df, select=c(status_id, created_at, text))
ma3route.df$status_id <- as.character(ma3route.df$status_id)
ma3route.df$created_at <- as.character(ma3route.df$created_at)
ma3route.df$text <- as.character(ma3route.df$text)

ma3route.df$tweet_nairobi_time <- as.POSIXct(ma3route.df$created_at, tz="America/New_York") 
attributes(ma3route.df$tweet_nairobi_time)$tzone <- "Africa/Nairobi" 

ma3route.df$year_month_day <- as.character(ma3route.df$tweet_nairobi_time) %>%
  substring(1,10)

ma3route.df <- subset(ma3route.df, select=c(status_id, tweet_nairobi_time, year_month_day, text))
ma3route.df <- ma3route.df[ma3route.df$year_month_day %in% tweet.date,]

# Subset to Accident Tweets
algorithm.output <- crashmap_algorithm(tweets=ma3route.df$text,
                                       landmark.dictionary="dummy",
                                       only.use.first.landmark=FALSE,
                                       only.use.first.landmark.and.use.longest=FALSE,
                                       landmark.match.type.rank.use=FALSE,
                                       landmark.match.type.rank=c("blah"),
                                       landmark.match.more.types=FALSE,
                                       remove.street.names=FALSE,
                                       fuzzy_match_accident=FALSE,
                                       fuzzy_match_accident.dist=2,
                                       fuzzy_match_accident.onlymisspelled=FALSE,
                                       fuzzy_match_accident.min.word.length=6,
                                       multiple_landmark_distance_threshold_km=0.5,
                                       fuzzy_match_landmark=FALSE,
                                       fuzzy_match_landmark.dist=1,
                                       fuzzy_match_landmark.min.word.length=6,
                                       remove.text.after.toward=FALSE,
                                       google_map_api=FALSE,
                                       supporting.files.path="~/Dropbox/World Bank/CrashMap-Nairobi/Algorithm/code/Main Algorithm/Supporting Files/",
                                       levenstein.word=FALSE,
                                       levenstein.word.min.words.fuzzy=3,
                                       levenstein.word.max.word.distance=1,
                                       levenstein.word.fuzzy.match=FALSE,
                                       levenstein.word.fuzzy.match.dist=1,
                                       levenstein.word.fuzzy.match.min.word.length=6,
                                       levenstein.word.contiguous=TRUE,
                                       mc.cores=1)

algorithm.output.df <- algorithm.output$data
ma3route.df <- cbind(ma3route.df, algorithm.output.df)

ma3route.df$text <- as.character(ma3route.df$text)
ma3route.df <- ma3route.df[ma3route.df$accident,]
ma3route.df <- subset(ma3route.df, select=c(status_id, tweet_nairobi_time, year_month_day, text))

# Only run code if there are some tweets for the day ---------------------------
if(nrow(ma3route.df) >= 1){

  # Load Google Sheet ----------------------------------------------------------
  sheets.all <- gs_title("Accident Tweets")
  ws.titles <- sheets.all$ws$ws_title
  
  # Update sheet ---------------------------------------------------------------
  if(tweet.date %in% ws.titles){
    
    sheet.i <- sheets.all %>%
      gs_read(ws = tweet.date)
    sheet.i$status_id <- as.character(sheet.i$status_id)
  
    ma3route.df <- ma3route.df[!ma3route.df$status_id %in% sheet.i$status_id,]
  
    # Only run code if new tweets to add.
    if(nrow(ma3route.df) >= 1){
      
      ##### Geocode
      algorithm.output.geo <- crashmap_algorithm(tweets=ma3route.df$text,
                                                 landmark.dictionary="adj_google_maps_bitri_cluster_0.5dist_0.9close",
                                                 only.use.first.landmark=TRUE,
                                                 only.use.first.landmark.and.use.longest=TRUE,
                                                 landmark.match.type.rank.use=TRUE,
                                                 landmark.match.type.rank=c("bus_station","shopping_mall"),
                                                 landmark.match.more.types=TRUE,
                                                 prepositions.list = c("at","near","opposite","opp","after","before","hapo","outside"),
                                                 prioritize.exact.match=FALSE,
                                                 remove.street.names=TRUE,
                                                 fuzzy_match_accident=FALSE,
                                                 fuzzy_match_accident.dist=2,
                                                 fuzzy_match_accident.onlymisspelled=FALSE,
                                                 fuzzy_match_accident.min.word.length=6,
                                                 multiple_landmark_distance_threshold_km=1,
                                                 fuzzy_match_landmark=TRUE,
                                                 fuzzy_match_landmark.dist=1,
                                                 fuzzy_match_landmark.min.word.length=6,
                                                 remove.text.after.toward=FALSE,
                                                 google_map_api=FALSE,
                                                 supporting.files.path="~/Dropbox/World Bank/CrashMap-Nairobi/Algorithm/code/Main Algorithm/Supporting Files/",
                                                 levenstein.word=FALSE,
                                                 levenstein.word.min.words.fuzzy=2,
                                                 levenstein.word.max.word.distance=1,
                                                 levenstein.word.fuzzy.match=FALSE,
                                                 levenstein.word.fuzzy.match.dist=1,
                                                 levenstein.word.fuzzy.match.min.word.length=6,
                                                 levenstein.word.contiguous=TRUE,
                                                 mc.cores=1)
      algorithm.output.geo.df <- algorithm.output.geo$data %>%
        subset(select=c(landmarks_used_to_geocode,tweet_landmark_lat_final,tweet_landmark_lon_final))
      names(algorithm.output.geo.df) <- c("landmark_algorithm_used_to_geocode","latitude_algorithm","longitude_algorithm")
      
      ##### Prep Dataset to Upload
      ma3route.df <- cbind(ma3route.df, algorithm.output.geo.df)
      
      ##### Append New Data to Sheet
      sheets.all <- sheets.all %>%
        gs_add_row(ws = tweet.date, input = ma3route.df)
    }
    
  # Create new sheet -----------------------------------------------------------
  } else{
    
    ##### Geocode
    algorithm.output.geo <- crashmap_algorithm(tweets=ma3route.df$text,
                                       landmark.dictionary="adj_google_maps_bitri_cluster_0.5dist_0.9close",
                                       only.use.first.landmark=TRUE,
                                       only.use.first.landmark.and.use.longest=TRUE,
                                       landmark.match.type.rank.use=TRUE,
                                       landmark.match.type.rank=c("bus_station","shopping_mall"),
                                       landmark.match.more.types=TRUE,
                                       prepositions.list = c("at","near","opposite","opp","after","before","hapo","outside"),
                                       prioritize.exact.match=FALSE,
                                       remove.street.names=TRUE,
                                       fuzzy_match_accident=FALSE,
                                       fuzzy_match_accident.dist=2,
                                       fuzzy_match_accident.onlymisspelled=FALSE,
                                       fuzzy_match_accident.min.word.length=6,
                                       multiple_landmark_distance_threshold_km=1,
                                       fuzzy_match_landmark=TRUE,
                                       fuzzy_match_landmark.dist=1,
                                       fuzzy_match_landmark.min.word.length=6,
                                       remove.text.after.toward=FALSE,
                                       google_map_api=FALSE,
                                       supporting.files.path="~/Dropbox/World Bank/CrashMap-Nairobi/Algorithm/code/Main Algorithm/Supporting Files/",
                                       levenstein.word=FALSE,
                                       levenstein.word.min.words.fuzzy=2,
                                       levenstein.word.max.word.distance=1,
                                       levenstein.word.fuzzy.match=FALSE,
                                       levenstein.word.fuzzy.match.dist=1,
                                       levenstein.word.fuzzy.match.min.word.length=6,
                                       levenstein.word.contiguous=TRUE,
                                       mc.cores=1)
    algorithm.output.geo.df <- algorithm.output.geo$data %>%
      subset(select=c(landmarks_used_to_geocode,tweet_landmark_lat_final,tweet_landmark_lon_final))
    names(algorithm.output.geo.df) <- c("landmark_algorithm_used_to_geocode","latitude_algorithm","longitude_algorithm")
    
    ##### Prep Dataset to Upload
    ma3route.df <- cbind(ma3route.df, algorithm.output.geo.df)
    
    sheets.all <- sheets.all %>%
      gs_ws_new(ws_title = tweet.date, input = ma3route.df)
  }
}



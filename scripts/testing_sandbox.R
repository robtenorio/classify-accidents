# Test Algorithm
# Sandbox [Simple area to play with testing on a few tweets]

start.time.script <- Sys.time()
# Load Stuff -------------------------------------------------------------------
file.path <- "~/Dropbox/World Bank/CrashMap-Nairobi/"
source(paste(file.path, "Algorithm/code/Main Algorithm/crashmap_algorithm_clean.R",sep=""))

library(stringr)
library(tm)

# Tweets -----------------------------------------------------------------------
tweet <- c("accident on Limuru Road near Muthaiga Min-Market. Approach carefully.")
df <- as.data.frame(tweet)
df$tweet <- as.character(df$tweet)

# Run Algorithm ----------------------------------------------------------------
tweets_previous.output <- crashmap_algorithm(tweets=df$tweet,
                                             landmark.dictionary="adj_google_maps_bitri_cluster_0.5dist_0.9close",
                                             only.use.first.landmark=TRUE,
                                             only.use.first.landmark.and.use.longest=TRUE,
                                             landmark.match.type.rank.use=TRUE,
                                             landmark.match.type.rank=c("bus_station","shopping_mall"),
                                             landmark.match.more.types=TRUE,
                                             prepositions.list = c("at","near","opposite","opp","after","before","hapo","outside"),
                                             prioritize.exact.match =FALSE,
                                             remove.street.names=TRUE,
                                             fuzzy_match_accident=FALSE,
                                             fuzzy_match_accident.dist=2,
                                             fuzzy_match_accident.onlymisspelled=FALSE,
                                             fuzzy_match_accident.min.word.length=6,
                                             multiple_landmark_distance_threshold_km=0.5,
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

tweets_previous.output.df <- tweets_previous.output$data

# Script Time - - - - - - - - - - - - - - -
end.time.script <- Sys.time()
print(end.time.script - start.time.script)

# Parameters to run code internally --------------------------------------------
tweets=df$tweet
landmark.dictionary="adj_google_maps_bitri_cluster_0.5dist_0.9close"
only.use.first.landmark=TRUE
only.use.first.landmark.and.use.longest=TRUE
landmark.match.type.rank.use=TRUE
landmark.match.type.rank=c("bus_station")
fuzzy_match_accident=FALSE
remove.street.names=TRUE
prepositions.list <- c("at","near","opposite")
prioritize.exact.match <- TRUE
landmark.match.more.types <- TRUE
fuzzy_match_accident.dist=2
fuzzy_match_accident.onlymisspelled=FALSE
fuzzy_match_accident.min.word.length=6
multiple_landmark_distance_threshold_km=1
fuzzy_match_landmark=TRUE
fuzzy_match_landmark.dist=1
fuzzy_match_landmark.min.word.length=6
remove.text.after.toward=FALSE
google_map_api=FALSE
supporting.files.path="~/Dropbox/World Bank/CrashMap-Nairobi/Algorithm/code/Main Algorithm/Supporting Files/"
levenstein.word=FALSE
levenstein.word.min.words.fuzzy=2
levenstein.word.max.word.distance=1
levenstein.word.fuzzy.match=FALSE
levenstein.word.fuzzy.match.dist=1
levenstein.word.fuzzy.match.min.word.length=6
levenstein.word.contiguous=TRUE
mc.cores=1



















tweets=df$tweet
landmark.dictionary="original.clean"
only.use.first.landmark=TRUE
fuzzy_match_accident=FALSE
fuzzy_match_accident.dist=2
fuzzy_match_accident.onlymisspelled=FALSE
fuzzy_match_accident.min.word.length=6
multiple_landmark_distance_threshold_km=0.5
fuzzy_match_landmark=FALSE
fuzzy_match_landmark.dist=1
fuzzy_match_landmark.min.word.length=6
remove.text.after.toward=FALSE
google_map_api=FALSE
supporting.files.path="~/Dropbox/World Bank/CrashMap-Nairobi/Algorithm/code/Main Algorithm/Supporting Files/"
levenstein.word=FALSE
levenstein.word.min.words.fuzzy=2
levenstein.word.max.word.distance=1
levenstein.word.fuzzy.match=FALSE
levenstein.word.fuzzy.match.dist=1
levenstein.word.fuzzy.match.min.word.length=6
levenstein.word.contiguous=TRUE
mc.cores=1

  
  
  

tweets=tweets.truth$tweet
landmark.dictionary=parameters[i,]$landmark.dictionaries
fuzzy_match_accident=FALSE
fuzzy_match_accident.dist=1
fuzzy_match_accident.onlymisspelled=FALSE
fuzzy_match_accident.min.word.length=7
multiple_landmark_distance_threshold_km=0.5
fuzzy_match_landmark=FALSE
fuzzy_match_landmark.dist=1
fuzzy_match_landmark.min.word.length=7
supporting.files.path="~/Dropbox/World Bank/CrashMap-Nairobi/Algorithm/code/Main Algorithm/Supporting Files/"
remove.text.after.toward=FALSE
google_map_api=FALSE
levenstein.word=parameters[i,]$levenstein
levenstein.word.fuzzy.match=TRUE
levenstein.word.fuzzy.match.dist=1
levenstein.word.fuzzy.match.min.word.length=5
levenstein.word.contiguous=parameters[i,]$contiguous
mc.cores=4
  




tweets=tweets.truth$tweet
landmark.dictionary=parameters[i,]$landmark.dictionary
only.use.first.landmark=parameters[i,]$only.use.first.landmark
fuzzy_match_accident=FALSE
fuzzy_match_accident.dist=1
fuzzy_match_accident.onlymisspelled=FALSE
fuzzy_match_accident.min.word.length=7
multiple_landmark_distance_threshold_km=0.5
fuzzy_match_landmark=parameters[i,]$fuzzy_match_landmark
fuzzy_match_landmark.dist=parameters[i,]$fuzzy_match_landmark.dist
fuzzy_match_landmark.min.word.length=parameters[i,]$fuzzy_match_landmark.min.word.length
supporting.files.path="~/Dropbox/World Bank/CrashMap-Nairobi/Algorithm/code/Main Algorithm/Supporting Files/"
remove.text.after.toward=FALSE
google_map_api=FALSE
levenstein.word=parameters[i,]$levenstein.word
levenstein.word.min.words.fuzzy=parameters[i,]$levenstein.word.min.words.fuzzy
levenstein.word.max.word.distance=parameters[i,]$levenstein.word.max.word.distance
levenstein.word.fuzzy.match=parameters[i,]$levenstein.word.fuzzy.match
levenstein.word.fuzzy.match.dist=parameters[i,]$levenstein.word.fuzzy.match.dist
levenstein.word.fuzzy.match.min.word.length=parameters[i,]$levenstein.word.fuzzy.match.min.word.length
levenstein.word.contiguous=parameters[i,]$levenstein.word.contiguous
mc.cores=1
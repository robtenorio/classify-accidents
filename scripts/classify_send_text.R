library(googlesheets)
library(keras)
library(rtweet)
library(tfdeploy)
library(tidytext)
library(tidyverse)
library(twilio)

word_index <- readRDS("supporting_files/word_index.rds")

# First you need to set up your accound SID and token as environmental variables
Sys.setenv(TWILIO_SID = "AC6cc00fe6d81b56208ae565760e7fd510")
Sys.setenv(TWILIO_TOKEN = "d25f7ce67a9546146abc8ad609d376ad")

# Then we're just going to store the numbers in some variables
my_phone_number <- "2023200815"
twilios_phone_number <- "2407022326"

# make integer sequences of tweet text
make_index_list <- function(x) {
  data_index <- c()
  
  for (i in 1:length(x)) {
    text <- x[[i]]
    x_seq <- text_to_word_sequence(text, split = " ", lower = TRUE)
    
    x_int <- c()
    for(n in x_seq) {
      int <- word_index$index[word_index$word %in% n]
      x_int <- c(x_int, int)
      x_int
    }
    data_index[[i]] <- x_int
  }
  data_index
}

# hot encode integer sequences
vectorize_sequences <- function(sequences, dimension = nrow(word_index)) {
  results <- matrix(0, nrow = length(sequences), ncol = dimension)
  
  for (i in 1:length(sequences)) {
    results[i, sequences[[i]]] <- 1
  }
  results
}

### Function for predicting new Tweets

predict_accident <- function(tweet) {
  tweet_index <- make_index_list(tweet)
  tweet_vectorized <- vectorize_sequences(tweet_index)
  
  prediction <- predict_savedmodel(tweet_vectorized, 'classify_lower_25_300_92')
  probability <- prediction$predictions[[1]]$dense_64
  
  if(probability > .80) {

    tw_send_message(from = twilios_phone_number, to = my_phone_number, 
                    body = str_c("This tweet describes an accident with ", round(probability*100, 0), 
                                 "% probability: ", tweet, sep = ""))
  } else {
    list("This tweet does not describe an accident", tweet, probability)
  }
}

### Load ma3route data

# load streamed tweets
tweets_ma3route <- parse_stream("tweets/all-tweets.json")

# load previously saved tweets from Google Sheets
gs_sheet <- gs_title("ma3route_tweets")
tweets_gs <- gs_read(gs_sheet, ws = "all")

# get the number of rows for each data set
stream_rows <- nrow(tweets_ma3route)
gs_rows <- nrow(tweets_gs)

if(stream_rows > gs_rows) {
rows2add <- (gs_rows+1):stream_rows
}

if(stream_rows < gs_rows) {
rows2add <- stream_rows
}

if(gs_rows != stream_rows) {

for( i in rows2add) {
predict_accident(tweets_ma3route[i, "text"])
}

gs_add_row(gs_sheet, input = tweets_ma3route[rows2add,])

}

q()

predict_accident("13:36 Minor accident opposite ABC PLACE between a matatu and a voxy. via wainnel")

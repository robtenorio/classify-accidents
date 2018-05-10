options("scipen" = 20)

library(here)
library(rtweet)
library(tidyverse)
library(googlesheets)

# load streamed tweets
tweets_ma3route <- parse_stream(here("tweets", "all-tweets.json")) %>%
  mutate(id = 1:nrow(.))

# load previously saved tweets from google sheets
gs_sheet <- gs_title("ma3route_tweets")
stream_sheet <- gs_read(gs_sheet, ws = "stream")

# which status_ids are missing from stream_sheets
rows2add <- which(!tweets_ma3route$id %in% stream_sheet$id)

if(length(rows2add) > 0) {
  gs_add_row(gs_sheet, input = tweets_ma3route[rows2add,])
}

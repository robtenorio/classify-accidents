library(here)
library(rtweet)

# load streamed tweets
tweets_ma3route <- parse_stream(here("tweets", "ma3route-tweets.json"))
# load previously saved tweets from google sheets
gs_sheet <- gs_title("ma3route_tweets")
stream_sheet <- gs_read(gs_sheet, ws = "stream")

# get the number of rows for each data set
stream_rows <- nrow(tweets_ma3route)
gs_rows <- nrow(stream_sheet)

rows2add <- (gs_rows+1):stream_rows

if(gs_rows != stream_rows) {
  gs_add_row(gs_sheet, input = tweets_ma3route[rows2add,])
}


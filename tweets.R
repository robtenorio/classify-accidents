# collect tweets
library(rtweet)

world.old <- stream_tweets(
  "Ma3Route",
  timeout = FALSE,
  parse = FALSE,
  file_name = "ma3route-tweets.json",
  append = TRUE
)


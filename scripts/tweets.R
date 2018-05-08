# collect tweets
library(here)
library(rtweet)

world.old <- stream_tweets(
  "585372692",
  timeout = FALSE,
  parse = FALSE,
  file_name = here("tweets", "ma3route-tweets.json"),
  append = TRUE
)

get_timeline("585372692", n = 10, tweet_mode = "extended")

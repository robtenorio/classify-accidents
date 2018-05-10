# collect tweets
library(here)
library(rtweet)

stream_tweets(
  "@CNN",
  timeout = 30,
  parse = FALSE,
  file_name = here("tweets", "all-tweets.json"),
  append = TRUE
)

get_timeline("585372692", n = 10, tweet_mode = "extended")

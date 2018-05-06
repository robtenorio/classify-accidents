library(parallel)
library(stringdist)
library(tidyverse)
library(tm)

### tweets
tweets <- read_csv("tweets/truth_r5.csv") %>% select(id, tweet, accident_truth)

### Supporting files
supporting.files.path <- "supporting_files"
categories <- read_csv(paste(supporting.files.path,"accident_words_v1.csv",sep="/"))

### Stop words
stop_words_list <- tm::stopwords()
#stop_words_list <- c("ssfsdfsdfsffgjh")

# Remove certain words from stop_words list; "and": Need for places like "Nice and lovely." Could also remove stop words from landmarks?
stop_words_list <- stop_words_list[!(stop_words_list %in% "and")]

stop_words <- paste0("\\b", stop_words_list, "\\b")
stop_words <- paste(stop_words, collapse = "|")

### parameters
fuzzy_match_accident.min.word.length <- 6

# Functions: Cleaning Tweets ---------------------------------------------------
clean_tweets <- function(input, stop_words){
  
  input_clean <- input %>%
    mutate(tweet_clean = str_to_lower(tweet)) 
  
  input_clean <- input_clean %>%
    mutate_at(.vars = vars(tweet_clean), .funs = function(x) str_replace_all(x, "via @[a-z_,A-Z_,0-9_]*","")) %>% 
    mutate_at(.vars = vars(tweet_clean), .funs = function(x) str_replace_all(x, "@[a-z_,A-Z_,0-9_]* ", "")) %>% 
    mutate_at(.vars = vars(tweet_clean), .funs = function(x) str_replace_all(x, "\n", "")) %>% 
    mutate_at(.vars = vars(tweet_clean), .funs = function(x) str_replace_all(x, "~", "")) %>% 
    mutate_at(.vars = vars(tweet_clean), .funs = function(x) str_replace_all(x, "\\b(http|https)://t.co/[0-9,A-Z, a-z]*\\b", "")) %>% 
    mutate_at(.vars = vars(tweet_clean), .funs = function(x) str_replace_all(x, "\\b(http|https)://t.co/[0-9,A-Z, a-z] ", "")) %>% 
    mutate_at(.vars = vars(tweet_clean), .funs = function(x) str_replace_all(x, "\\b(http|https)://t.co\\b", "")) %>% 
    mutate_at(.vars = vars(tweet_clean), .funs = function(x) str_replace_all(x, "\\b(http|https):", "")) %>% 
    mutate_at(.vars = vars(tweet_clean), .funs = function(x) str_replace_all(x, "~more ⇢", "")) %>% 
    mutate_at(.vars = vars(tweet_clean), .funs = function(x) str_replace_all(x, "(RT|rt) @[a-z,A-Z,0-9, _]*:", "")) %>% 
    mutate_at(.vars = vars(tweet_clean), .funs = function(x) str_replace_all(x, "^[0-9][0-9]\\:[0-9][0-9]", "")) %>% 
    mutate_at(.vars = vars(tweet_clean), .funs = function(x) str_replace_all(x, "[[:punct:]]", " ")) %>% 
    mutate_at(.vars = vars(tweet_clean), .funs = function(x) trimws(x)) 
  
  # Amperstand with and
  input_clean <- input_clean %>%
    mutate_at(.vars = vars(tweet_clean), .funs=function(x) str_replace_all(x, " amp "," and "))
  
  input_clean <- input_clean %>%
    mutate_at(.vars = vars(tweet_clean), .funs = function(x) str_replace_all(x, stop_words,""))
  
  # Remove non-ASCII characters
  input_clean$tweet_clean <- iconv(input_clean$tweet_clean, "latin1", "ASCII", sub="")
  
  # Remove unnecessary whitespace
  input_clean$tweet_clean <- str_squish(input_clean$tweet_clean)
  
  input_clean <- input_clean %>% 
    mutate(rowid = 1:nrow(input))
  
  return(input_clean)
}

# Functions: Categorize Tweets -------------------------------------------------

### Exact Word Matches
category_match <- function(tweet, accident_words_space){
  cat.match <- str_extract(tweet, accident_words_space)
  cat.match <- cat.match[!is.na(cat.match)]
  cat.match <- paste(cat.match, collapse=";")
  if(identical(cat.match,character(0))){
    cat.match <- ""
  }
  return(cat.match)
}

fuzzy_match_accident <- FALSE
fuzzy_match_landmark <- FALSE

### If not doing a fuzzy match, set other parameters to NA
if(fuzzy_match_accident == FALSE){
  fuzzy_match_accident.dist <-  NA
  fuzzy_match_accident.onlymisspelled <- NA
  fuzzy_match_accident.min.word.length <- NA
}

if(fuzzy_match_landmark == FALSE){
  fuzzy_match_landmark.dist <- NA
  fuzzy_match_landmark.min.word.length <- NA
}

### Levenstein Word Matches
category_match_fuzzy <- function(tweet, accident_words, fuzzy_match_accident.onlymisspelled){
  
  # Prep list of words
  tweet_words <- strsplit(tweet, split=" ")[[1]]
  
  # Limit list to only misspelled words
  if(fuzzy_match_accident.onlymisspelled){
    tweet_words <- hunspell(tweet_words) %>% 
      unlist
  }
  
  # Limit list to words of a certain length
  tweet_words <- tweet_words[nchar(tweet_words) >= fuzzy_match_accident.min.word.length]
  
  # Only check fuzzy matches if words in word list
  if(identical(tweet_words, character(0))){
    matched_words_incorrect_spelling <- ""
    matched_words_correct_spelling <- ""
  } else{
    
    # Fuzzy Match Algorithm; Returns location in dictionary of closest matched word
    category_match_position <- amatch(tweet_words, accident_words, maxDist = fuzzy_match_accident.dist, method="lv") 
    
    # List of words in tweet with matches
    matched_words_incorrect_spelling <- tweet_words[!is.na(category_match_position)] %>%
      paste(collapse=";")
    
    # List of words in dictionary with matches
    category_match_position_unique <- category_match_position[!is.na(category_match_position)] %>% unique
    matched_words_correct_spelling <- accident_words[category_match_position_unique] %>%
      paste(collapse=";")
  }
  
  df.output <- as.data.frame(t(c(matched_words_incorrect_spelling, matched_words_correct_spelling)))
  names(df.output) <- c("matched_words_incorrect_spelling","matched_words_correct_spelling")
  
  # Variables original factors; change to character
  df.output$matched_words_incorrect_spelling <- as.character(df.output$matched_words_incorrect_spelling)
  df.output$matched_words_correct_spelling <- as.character(df.output$matched_words_correct_spelling)
  
  return(df.output)
}

# 3. Clean Tweets ------------------------------------------------------------
df.tweets <- clean_tweets(tweets, stop_words)

# 4. Categorize Tweets -------------------------------------------------------
accident_words <- categories$Word
accident_words_space <- paste0("\\b", categories$Word, "\\b")

mc.cores <- 3

df.tweets$category_word_match <- mclapply(df.tweets$tweet_clean, category_match, accident_words_space, mc.cores=mc.cores) %>% unlist()
df.tweets$accident_exact_match <- (df.tweets$category_word_match != "")

if(fuzzy_match_accident){
  category_match_fuzzy.df <- mclapply(df.tweets$tweet_clean, category_match_fuzzy, accident_words, fuzzy_match_accident.onlymisspelled, mc.cores=mc.cores) %>% bind_rows
  df.tweets <- cbind(df.tweets, category_match_fuzzy.df)
  df.tweets$accident_fuzzy_match <- (df.tweets$matched_words_incorrect_spelling != "")
  
  df.tweets$accident <- (df.tweets$accident_exact_match) | (df.tweets$accident_fuzzy_match)
} else{
  df.tweets$accident <- df.tweets$accident_exact_match
}



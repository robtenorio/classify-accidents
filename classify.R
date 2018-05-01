library(here)
library(keras)
library(tidytext)
library(tidyverse)

tweets <- read_csv(here("tweets", "truth_r5.csv")) %>% 
  select("tweet", "accident_truth") %>%
  set_names("tweet", "accident") %>%
  mutate(accident = factor(accident, levels = c("No", "Yes")))
  
table(tweets$accident)

# separate tweet data into training and testing sets
train_index <- sample(1:nrow(tweets), nrow(tweets)/2)
# data
train_data <- tweets$tweet[train_index]
test_data <- tweets$tweet[-train_index]
# labels
train_labels <- tweets$accident[train_index]
test_labels <- tweets$accident[-train_index]

# pre-process text
text_process <- function(text, stopwords = TRUE) {
  # remove time stamp and other numbers
  text_data <- gsub("[[:digit:]]*|\\r", "", text)
  
  cat_text <- paste(text_data, collapse=" ")
  text_sequence <- text_to_word_sequence(cat_text, split = " ")
  integers_cat <- text_one_hot(cat_text, n = 10000)
  
  if(stopwords) {
    text_index <- tibble(word = text_sequence, index = integers_cat) %>%
      anti_join(get_stopwords(), by = "word") %>% unique
  } else {
    text_index <- tibble(word = text_sequence, index = integers_cat) %>% unique
  }
  

  text_index
  }

# create a word index to match text to integers
word_index <- text_process(tweets$tweet)

# make integer sequences of tweet text
make_index_list <- function(x) {
  data_index <- c()
  for (i in 1:length(x)) {
    sequence <- x[[i]]
    sequence_index <- text_process(sequence)
    data_index[[i]] <- sequence_index$index
  }
  data_index
}

test_index <- make_index_list(test_data)
train_index <- make_index_list(train_data)

# hot encode integer sequences
vectorize_sequences <- function(sequences, dimension = 10000) {
  results <- matrix(0, nrow = length(sequences), ncol = dimension)
  
  for (i in 1:length(sequences)) {
    results[i, sequences[[i]]] <- 1
  }
  results
}

x_test <- vectorize_sequences(test_index)
x_train <- vectorize_sequences(train_index)

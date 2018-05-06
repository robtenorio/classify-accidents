library(here)
library(keras)
library(tidytext)
library(tidyverse)

tweets <- read_csv(here("tweets", "truthdata_allrounds.csv")) %>%
  mutate(accident = NA) %>%
  mutate(accident = replace(accident, accident_truth == "No", 0)) %>%
  mutate(accident = replace(accident, accident_truth == "Yes", 1)) %>%
  select("tweet", "accident") %>% na.omit

table(tweets$accident)

# pre-process text
text_process <- function(text, stopwords = FALSE, lower = FALSE) {
  # remove time stamp and other numbers
  #text_data <- gsub("[[:digit:]]*|\\r", "", text)
  
  cat_text <- paste(text, collapse=" ")
  text_sequence <- unique(text_to_word_sequence(cat_text, split = " ", lower = lower))
  
  if(stopwords) {
    text_index <- tibble(word = text_sequence, index = 1:length(text_sequence)) %>%
      anti_join(get_stopwords(), by = "word")
  } else {
    text_index <- tibble(word = text_sequence, index = 1:length(text_sequence))
  }
  
  text_index
}

# create a word index to match text to integers
word_index <- text_process(tweets$tweet)

# make integer sequences of tweet text
make_index_list <- function(x) {
  data_index <- c()
  
  for (i in 1:length(x)) {
    text <- x[[i]]
    x_seq <- text_to_word_sequence(text, split = " ", lower = FALSE)
    
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

# separate tweet data into training and testing sets
index2train <- sample(1:nrow(tweets), nrow(tweets)/2)
# data
train_data <- tweets$tweet[index2train]
test_data <- tweets$tweet[-index2train]

# labels
y_train <- tweets$accident[index2train]
y_test <- tweets$accident[-index2train]

# convert words to integers based on their place in the dictionary
test_index <- make_index_list(test_data)
train_index <- make_index_list(train_data)

x_train <- vectorize_sequences(train_index)
x_test <- vectorize_sequences(test_index)

#val_indices <- sample(1:nrow(x_train), 500)
val_indices <- 1:500

x_val <- x_train[val_indices,]
partial_x_train <- x_train[-val_indices,]
y_val <- y_train[val_indices]
partial_y_train <- y_train[-val_indices]

model <- keras_model_sequential() %>%
  layer_dense(units = 16, activation = "relu", input_shape = nrow(word_index)) %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")

model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)

history <- model %>% fit(
  partial_x_train,
  partial_y_train,
  epochs = 50,
  batch_size = 300,
  validation_data = list(x_val, y_val)
)

plot(history)

# retrain model with peak epochs from validation
model <- keras_model_sequential() %>%
  layer_dense(units = 16, activation = "relu", input_shape = nrow(word_index)) %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")

model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)

model %>% fit(
  x_train,
  y_train,
  epochs = 30,
  batch_size = 300
)

results <- model %>% evaluate(x_test, y_test)

results

predictions <- model %>% predict(x_test)

predictions_check <- tibble(tweet = test_data, accident = y_test, prediction = predictions[,1]) %>%
  mutate(predict_accident = 0) %>%
  mutate(predict_accident = replace(predict_accident, prediction > .70, 1)) %>%
  mutate(correct = case_when(accident == predict_accident ~ 1)) %>% 
  mutate(correct = replace(correct, is.na(correct), 0))

table(predictions_check$predict_accident)
table(predictions_check$accident)

mean(predictions_check$correct)











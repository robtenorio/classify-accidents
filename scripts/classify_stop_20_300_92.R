library(here)
library(keras)
library(tidytext)
library(tidyverse)

set.seed(5578)

tweets <- read_csv(here("tweets", "truthdata_allrounds.csv")) %>%
  mutate(accident = NA) %>%
  mutate(accident = replace(accident, accident_truth == "No", 0)) %>%
  mutate(accident = replace(accident, accident_truth == "Yes", 1)) %>%
  select("tweet", "accident") %>% na.omit

table(tweets$accident)

# pre-process text
text_process <- function(text, stopwords = TRUE, lower = FALSE) {
  # remove time stamp and other numbers
  #text_data <- gsub("[[:digit:]]*|\\r", "", text)
  
  cat_text <- paste(text, collapse=" ")
  text_sequence <- unique(text_to_word_sequence(cat_text, split = " ", lower = lower))
  
  if(stopwords) {
    text_sequence <- text_sequence[!text_sequence %in% get_stopwords()[["word"]]]
    text_index <- tibble(word = text_sequence, index = 1:length(text_sequence))
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
  layer_dense(units = 32, activation = "relu", input_shape = nrow(word_index)) %>%
  layer_dense(units = 32, activation = "relu") %>%
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
  layer_dense(units = 32, activation = "relu", input_shape = nrow(word_index)) %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")

model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)

model %>% fit(
  x_train,
  y_train,
  epochs = 20,
  batch_size = 300
)

results <- model %>% evaluate(x_test, y_test)

results

predictions <- model %>% predict(x_test)

predictions_check <- tibble(tweet = test_data, accident = y_test, prediction = predictions[,1]) %>%
  mutate(predict_accident = 0) %>%
  mutate(predict_accident = replace(predict_accident, prediction > .67, 1)) %>%
  mutate(correct = case_when(accident == predict_accident ~ 1)) %>% 
  mutate(correct = replace(correct, is.na(correct), 0))

table(predictions_check$predict_accident)
table(predictions_check$accident)

mean(predictions_check$correct)

export_savedmodel(model, "classify_stop_35_300_91")

### Function for predicting new Tweets

predict_accident <- function(tweet) {
  tweet_index <- make_index_list(tweet)
  tweet_vectorized <- vectorize_sequences(tweet_index)
  
  prediction <- model %>% predict(tweet_vectorized)
  
  if(prediction > .80) {
    list("This Tweet describes an accident.", tweet, prediction)
  } else {
    list("This Tweet does not describe an accident.", tweet, prediction)
  }
}

# accidents
predict_accident("18:25 hit and run on Waiyaki Way causing snarl up.  https://twitter.com/aqueerian/status/992787431157129217/photo/1pic.twitter.com/aaKMgmf8Zi  via @aqueerian")
predict_accident("12:52 Alert: Accident involving motorbike and private car at the Bomas of Kenya near \"KWS\" College on Langata Road. Exercise caution. + No police on sight.  https://twitter.com/GrayMarwa/status/992697224583897088 … via @GrayMarwa")
predict_accident("12:28 Snarl up on Langata Road caused by accident involving a motorcycle & motor vehicle after Bomas before the National Park. via @Jaxon09")
predict_accident("Fatal accident at Kiwanja on Nothern bypass cleared.huge crowd on the road side. Boda boda rider taken to hospital.the deceased was a pillion passenger @Kiss100kenya @PRSA_Roadsafety @NPSOfficial_KE @KURAroads @RadioJamboKenya @inooroke @CapitalFMKenya @2Fmke @K24Tv @ntsa_kenya
")

# non-accidents
predict_accident("22:36 @KeNHAKenya the pothole is still there! (as you approach  sigona from Limuru)Those are  4 cars with punctures! At this hour- and in the rain . For how long will Kenyans suffer?? @JamesMacharia_   via @bnyenjeri")
predict_accident("heavy traffic between Roysambu zimmerman. alternative routes via")
predict_accident("21:33 Ruaka route is really bad!. No cops to Assist control traffic.  Sad!! via @FaithMunyasi")
predict_accident("18:24 Has anyone ever received these monies police claim? Witnesses come forward,,me i know ukiwaambia unamjua ndo utajua hujui via @Mkulima18")
predict_accident("JAPAN: Bus drivers in Okayama have gone on strike, by continuing to drive their routes while refusing to take fares from passengers.")
predict_accident("16:57 Avoid Outering Rd from Taj Mall towards Donholm.Traffic bumper to bumper,   via @CaroleOO16")
predict_accident("16:55 I passed their a few hours ago and was astonished by how drivers can be...parking vehicles on the road to go and peep at a crashed vehicle causing huge traffic snarl up ..... Every parked vehicle needed to be booked for obstruction via @MateMurithi")
predict_accident("16:27 Stuck truck at Outering exit juu to Doni caltex frm pipeline creating jam kubwa   via @simwa_")
predict_accident("12:18 From South c to Uhuru High way towards CBD moving smoothly🚙🚙🚙🚙🚙🚙🚙 Mombasa Road via @Eng_Onsoti")
predict_accident("15:38 Very heavy traffic in Juja on your way to Thika. Still trying to recover the lorry that plunged into Ndarugo river.   via @ThikaTowntoday")
predict_accident("12:09 @NairobiAlerts @digitalmkenya Our laws are funny though,, the prosecutor had to prove the pastor intended to kill the said victim. Otherwise, it wil be treated as an accident and the best the family can do is to follow  https://www.ma3route.com/update/703679  via @MatthewsMbuthia")
predict_accident("12:02 @alfredootieno   He promised a change in 100 days. What we have seen is painful.
Water shortage is even beyond words can describe. Traffic has become the order of the day.
                 What kind of patience are you talking about? via @AderaJacob")
predict_accident("11:21 avoid route between muthaiga mini mkt and parklands.  Bumper to bumper traffic as usual. via @Its_Sheshii")
predict_accident("10:47 @Dj_Amar_B Riverside drive has become a construction site. I assume someone is blasting some site. via @Pagmainc")
predict_accident("10:32 Speed trap as you approach Kinoo.. via @njauwamahugu")
predict_accident("10:29 @KenyaPower_Care  A pole has fallen/leaning on a vehicle along Gaberone rd off Luthuli avenue   via @kimaniq")
predict_accident("10:11 @NPSOfficial_KE corruption thriving along Msa road just after machakos towards konza. cops in a highway xtrail patrol car via @cymohnganga")
predict_accident("15:29 Without pedestrian paths and street lights ... don't relaunched  https://twitter.com/KURAroads/status/993792093419171840 … via @silascheboi")
predict_accident("Peak of accidents in Nairobi occurs during morning rush hour. Report more accidents with a landmark/street intersection to help us map and enter to win 100KES daily. #saferroadsKE @KenyaRedCross @AccidentsKE To learn more visit https://bit.ly/2GIiMKp ")
predict_accident("15:53 Conductors trust this small metal chuma with their lives .....true or false?     @NairobiAlerts   via @digitalmkenya")
predict_accident("15:48 @KeNHAKenya @KURAroads     Too many near misses due to these potholes on a very busy turn off along Mombasa road...please send help before a major accident occurs.   via @GeoffKimani")
predict_accident("15:27 Heap of soil dangerously dumped at the junction of Karen road and Langata rd. Accident in waiting if doesn't get removed asap @MikeSonko via @sirngure")
predict_accident("14:20 So your driver gives you a casual call Ati Kuna Shida kidogo mkubwa, only to sent you a photo of this mess of your priced toy! I would casually chop off his limbs.   via @itsmuthama")

# problems - Im not even sure if these are accidents or not
predict_accident("15:38 Very heavy traffic in Juja on your way to Thika. Still trying to recover the lorry that plunged into Ndarugo river.   via @ThikaTowntoday")
predict_accident("13:22 Opposite Stella along outering road Singhong Corporation Lorry\"The Bufallo\"own accident veered off the road and plunged on drainage system, lungalunga road off Donholm KCD 192Z capsized as it climb bend-hill, U-Turn cautiou https://www.ma3route.com/update/704755  via @kkmunenebig")
predict_accident("09:28 these guys have been knocked down along Mombasa road, opposite sameer park.. any ambulances around?   via @eric_onchonga")


## remove hashtags and some stopwords (in a)
predict_accident("corruption thriving along Msa road just after machakos towards konza. cops highway xtrail patrol car via")
predict_accident("09:39 Via @MwendeCharles Nothing beats a Subaru. The way these machines negotiate sharp bends at 150Km/hr! Damn!     @NairobiAlerts   via @digitalmkenya")



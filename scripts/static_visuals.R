library(ggplot2)
library(lubridate)
library(tidyverse)

### load clustered tweet data
accidents <- read_csv("tweets/accidents_clustered.csv") %>% 
  mutate(month = month(year_month_day, label = TRUE)) %>%
  mutate(day = wday(year_month_day, label = TRUE))

### plot accidents by day of week
ggplot()

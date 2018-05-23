library(dplyr)
library(readr)

source("scripts/alltruth_clustering.r")
twitter_accidents <- truth.geo.df %>%
  mutate(source = "twitter")

twitter_ts <- twitter_accidents %>%
  mutate(date = mday(date)) %>%
  group_by(month, date) %>%
  summarise(twitter = n())

counties <- c("Nairobi", "Kiambu", "Murang'a", "Kajiado", "Machakos")

matatu_accidents <- read_csv("matatu_data/processed_incidents.csv") %>%
  mutate(injuries = factor(maincategory, levels = c("BOTH", "INJURY", "NON INJURY"), 
                           labels = c("Both", "Injuries", "No Injuries"))) %>%
  mutate(date = ymd_hms(incidentdate)) %>%
  mutate(year = year(date)) %>%
  mutate(month = month(date, label = TRUE)) %>%
  mutate(day = wday(date, label = TRUE)) %>%
  filter(!is.na(county)) %>%
  filter(county %in% counties)

matatu_accidents_feb2may <- matatu_accidents %>%
  filter(date >= ymd("2014-02-01") & date <= ymd("2014-04-30"))

matatu_ts <- matatu_accidents_feb2may %>%
  mutate(date = mday(date)) %>%
  group_by(month, date) %>%
  summarise(matatu = n())

govt_fatalities_2018 <- read_csv("govt_data/OFFICIAL FATAL REPORT 2018.csv") %>%
  fill(DATE) %>%
  mutate(date = mdy(DATE)) %>%
  mutate(year = year(date)) %>%
  mutate(month = month(date, label = TRUE)) %>%
  mutate(day = wday(date, label = TRUE)) %>%
  mutate(fatalities = NO.) %>%
  mutate(county = tolower(COUNTY)) %>%
  mutate(`TIME 24 HOURS` = replace(`TIME 24 HOURS`, `TIME 24 HOURS` == "10000HRS", "1000HRS")) %>%
  mutate(hour24 = gsub("(HRS)", "", `TIME 24 HOURS`)) %>%
  mutate(hour24 = gsub("^([[:digit:]]{1,2})([[:digit:]]{2})", "\\1:\\2", hour24)) %>%  
  filter(grepl(":", hour24)) %>%
  mutate(hour24 = gsub("^([[:digit:]]{1}:)", "0\\1", hour24)) %>%
  mutate(date_h = str_c(date, hour24, sep = " ")) %>%
  mutate(date = ymd_hm(date_h)) %>%
  filter(!is.na(date)) %>%
  filter(month <= "May") %>% # there are some dates in August 2018, which is before this data were collected
  filter(month >= "Feb" & month <= "Apr") %>% # we'll keep Feb. to May for now to match our Twitter data
  filter(county %in% tolower(counties)) %>%
  mutate(hour = hour(date))

govt_ts <- govt_fatalities_2018 %>%
  mutate(date = mday(date)) %>%
  group_by(month, date) %>%
  summarise(govt = n())

all_ts <- matatu_ts %>%
  left_join(govt_ts) %>%
  left_join(twitter_ts) %>%
  mutate(date_s = str_c(month, date, sep = "-")) %>%
  ungroup() %>%
  mutate(id = 1:nrow(.)) %>%
  mutate(date_s = as.factor(date_s)) %>%
  mutate(date = reorder(date_s, id)) %>%
  dplyr::select(date, twitter, matatu, govt)

saveRDS(all_ts, "accidents-dashboard/data/all_ts.rds")

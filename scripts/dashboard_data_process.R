library(lubridate)
library(dplyr)
library(tidyverse)

#### TWITTER DATA
#source("scripts/alltruth_clustering.r")
twitter <- read_csv("tweets/accidents_unique_algorithm.csv") %>%
  mutate(date = ymd_hms(time, tz = "Africa/Nairobi")) %>%
  mutate(year = year(date)) %>%
  mutate(month = month(date)) %>%
  mutate(day = wday(date, label = TRUE)) %>%
  mutate(hour = hour(date)) %>%
  mutate(source = "twitter") %>%
  mutate(time_day = case_when(hour %in% c(1,2,3,4) ~ "01:00-04:59",
                              hour %in% c(5,6,7,8,9) ~ "05:00-09:59",
                              hour %in% c(10,11,12,13) ~ "10:00-13:59",
                              hour %in% c(14,15,16,17) ~ "14:00-17:59",
                              hour %in% c(18,19,20,21) ~ "18:00-21:59",
                              hour %in% c(22,23,0) ~ "22:00-00:59")) %>%
  mutate(time_day = factor(time_day, ordered = TRUE)) %>%
  dplyr::select(year, month, day, time_day, longitude, latitude, date, source) %>%
  dplyr::rename(long = longitude, lat = latitude)

### geo data
counties <- c("Nairobi", "Kiambu", "Murang'a", "Kajiado", "Machakos")

#### Matatu Data
matatu_accidents <- read_csv("matatu_data/processed_incidents.csv") %>%
  mutate(injuries = factor(maincategory, levels = c("BOTH", "INJURY", "NON INJURY"), 
                           labels = c("Both", "Injuries", "No Injuries"))) %>%
  mutate(date = ymd_hms(incidentdate, tz = "Africa/Nairobi")) %>%
  mutate(county = factor(county)) %>% 
  mutate(county = reorder(county, county, function(x) -length(x))) %>%
  mutate(year = year(date)) %>%
  mutate(month = month(date, label = TRUE)) %>%
  mutate(day = wday(date, label = TRUE)) %>%
  mutate(day_floor = floor_date(date, "d")) %>%
  mutate(source = "matatu") %>%
  filter(!is.na(county)) %>% filter(injuries != "Both") %>%
  filter(county %in% counties)

matatu_accidents_aug2jul <- matatu_accidents %>% 
  filter(date >= ymd("2012-08-01") & date <= ymd("2014-07-30")) %>%
  mutate(year_compare = case_when(date >= ymd("2012-08-01") & date < ymd("2013-08-01") ~ "2012-2013",
                                  date >= ymd("2013-08-01") & date < ymd("2014-08-01") ~ "2013-2014")) %>%
  mutate(year_compare = ordered(year_compare))

matatu_crashes_total <- matatu_accidents_aug2jul %>%
  group_by(year_compare, day_floor) %>%
  summarise(total = n()) %>%
  ungroup() %>%
  left_join(unique(matatu_accidents_aug2jul[,c("day_floor", "day")])) %>%
  group_by(year_compare, day) %>%
  summarise(total = sum(total))

matatu_crashes_avg <- matatu_accidents_aug2jul %>%
  group_by(year_compare, day_floor) %>%
  summarise(total = n()) %>%
  ungroup() %>%
  left_join(unique(matatu_accidents_aug2jul[,c("day_floor", "day")])) %>%
  group_by(year_compare, day) %>%
  summarise(mean = mean(total))

matatu_passengers_avg <- matatu_accidents_aug2jul %>%
  group_by(year_compare, day_floor) %>%
  summarise(total = sum(passengers)) %>%
  ungroup() %>%
  left_join(unique(matatu_accidents_aug2jul[,c("day_floor", "day")])) %>%
  group_by(year_compare, day) %>%
  summarise(mean = mean(total))

matatu_injuries_avg <- matatu_accidents_aug2jul %>%
  group_by(year_compare, day_floor) %>%
  summarise(total = sum(injuredpersons)) %>%
  ungroup() %>%
  left_join(unique(matatu_accidents_aug2jul[,c("day_floor", "day")])) %>%
  group_by(year_compare, day) %>%
  summarise(mean = mean(total))

#### group by year and month
matatu_crashes_injuries_month <- matatu_accidents_aug2jul %>%
  group_by(year, month, injuries) %>%
  summarise(total = n()) %>%
  spread(injuries, total, fill = 0)

matatu_crashes_month <- matatu_accidents_aug2jul %>%
  group_by(year, month) %>%
  summarise(total = n())

matatu_crashes_injuredpersons_month <- matatu_accidents_aug2jul %>%
  group_by(year, month) %>%
  summarise(total = sum(injuredpersons))

matatu_crashes_passengers_month <- matatu_accidents_aug2jul %>%
  group_by(year, month) %>%
  summarise(total = sum(passengers))
  
matatu_police_station_crashes_year <- matatu_accidents_aug2jul %>%
  group_by(year_compare, p_station) %>%
  summarise(total = n())

#### GOVT DATA - CODED
govt_fatalities <- read_csv("govt_data/govt_data_truth.csv") %>%
  rename(long = longitude_truth, lat = latitude_truth) %>%
  mutate(source = "govt") %>%
  mutate(date = ymd(DATE, tz = "Africa/Nairobi")) %>%
  mutate(year = year(date)) %>%
  mutate(month = month(date)) %>%
  mutate(day = wday(date)) %>%
  select(year, month, day, long, lat, source) %>%
  filter(!is.na(long)) %>%
  filter(!is.na(lat))

ke_counties <- raster::getData("GADM", level = 1, country = "KEN")
nairobi_counties <- ke_counties[ke_counties@data$NAME_1 %in% counties,]

govt_fatalities_sp <- govt_fatalities
sp::coordinates(govt_fatalities_sp) <- ~long + lat
raster::projection(govt_fatalities_sp) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
over_govt <- sp::over(govt_fatalities_sp, nairobi_counties)
govt_fatalities_nairobi <- govt_fatalities[!is.na(over_govt$OBJECTID),]

binded_data <- plyr::rbind.fill(twitter, matatu_accidents_aug2jul, govt_fatalities_nairobi)

##### OLD CODE -- DELETE?
#### GOVT DATA

'govt_fatalities_2017 <- read_csv("govt_data/Copy of FATAL REPORT  2017 May to 2018 May-1.xlsx - 2017.csv") %>%
  fill(DATE) %>%
  mutate(date = mdy(DATE)) %>%
  mutate(year = year(date)) %>%
  mutate(month = month(date, label = TRUE)) %>%
  mutate(day = wday(date, label = TRUE)) %>%
  mutate(fatalities = NO.) %>%
  mutate(county = tolower(COUNTY)) %>%
  mutate(hour24 = gsub("^([[:digit:]]{1,2})([[:digit:]]{2})$", "\\1:\\2", `TIME 24 HOURS`)) %>%  
  filter(grepl(":", hour24)) %>%
  mutate(hour24 = gsub("^([[:digit:]]{1}:)", "0\\1", hour24)) %>%
  mutate(date_h = str_c(date, hour24, sep = " ")) %>%
  mutate(date = ymd_hm(date_h, tz = "Africa/Nairobi")) %>%
  mutate(hour = hour(date)) %>%
  mutate(source = "govt")

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
  mutate(date = ymd_hm(date_h, tz = "Africa/Nairobi")) %>%
  filter(!is.na(date)) %>%
  filter(month <= "May") %>% # there are some dates in August 2018, which is before this data were collected
  filter(month >= "Feb" & month <= "Apr") %>% # we will keep Feb. to May for now to match our Twitter data
  filter(county %in% tolower(counties)) %>% # keep only nairobi area
  mutate(hour = hour(date)) %>%
  mutate(source = "govt")

govt_fatalities <- rbind(govt_fatalities_2017, govt_fatalities_2018)

### Time Series

twitter_ts <- twitter %>%
  mutate(date = gsub(" ([[:digit:]]{2}:).*", "", date)) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d", tz = "EAT")) %>%
  mutate(date = ymd(date)) %>%
  mutate(date = floor_date(date, "d")) %>%
  group_by(date) %>%
  summarise(twitter = n()) %>%
  ungroup()

govt_ts <- govt_fatalities %>%
  mutate(date = gsub(" ([[:digit:]]{2}:).*", "", date)) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d", tz = "EAT")) %>%
  mutate(date = ymd(date)) %>%
  mutate(date = floor_date(date, "d")) %>%
  group_by(date) %>%
  summarise(govt = n()) %>%
  ungroup() %>%
  left_join(twitter_ts)

matatu_ts <- matatu_accidents_aug2jul %>%
  filter(injuries != "Both") %>%
  mutate(date = gsub(" ([[:digit:]]{2}:).*", "", date)) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d", tz = "EAT")) %>%
  mutate(date = ymd(date)) %>%
  mutate(date = floor_date(date, "d")) %>%
  group_by(date) %>%
  summarise(matatu = n()) %>%
  ungroup() %>%
  rbind.fill(govt_ts) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d", tz = "EAT"))

summary(twitter_ts)
summary(govt_ts)

summary(matatu_ts, na.rm = TRUE)'

saveRDS(binded_data, "crash-dashboard/data/binded_data.rds")

saveRDS(matatu_ts, "accidents-dashboard/data/data_ts.rds")



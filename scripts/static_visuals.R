knitr::opts_chunk$set(message = FALSE, warning = FALSE, fig.show = "animate")
options(max.print = 1000)
library(broom)
library(geosphere)
library(gganimate)
library(ggmap)
library(ggplot2)
library(gridExtra)
library(lubridate)
library(raster)
library(sf)
library(sp)
library(tidyverse)

#### TWITTER DATA
source("scripts/alltruth_clustering.r")
accidents <- truth.geo.df %>%
  mutate(source = "twitter") %>%
  mutate(hour_int = factor(hour))

accidents_avg <- accidents %>%
  group_by(year) %>%
  summarise(mean = n()/84)

accidents_hour_avg <- accidents %>%
  group_by(hour) %>%
  summarise(mean = n()/84)

accidents_ts <- accidents %>%
  mutate(date = floor_date(date, "d")) %>%
  group_by(date) %>%
  summarise(n = n())

### geo data
counties <- c("Nairobi", "Kiambu", "Murang'a", "Kajiado", "Machakos")
ke_counties <- getData("GADM", level = 1, country = "KEN")
ke_constituencies <- getData("GADM", level =2, country = "KEN")
nairobi_counties <- ke_counties[ke_counties@data$NAME_1 %in% counties,]
nairobi_counties@data$name_1 <- tolower(nairobi_counties@data$NAME_1)
nairobi_constituencies <- ke_constituencies[ke_constituencies@data$NAME_1 %in% counties, ]

#### Matatu Data
matatu_accidents <- read_csv("matatu_data/processed_incidents.csv") %>%
  mutate(injuries = factor(maincategory, levels = c("BOTH", "INJURY", "NON INJURY"), 
                           labels = c("Both", "Injuries", "No Injuries"))) %>%
  mutate(passengers_cat = cut(passengers, c(10,20,30,40,50,60,70))) %>%
  mutate(date = ymd_hms(incidentdate)) %>%
  mutate(county = factor(county)) %>% 
  mutate(county = reorder(county, county, function(x) -length(x))) %>%
  mutate(year = year(date)) %>%
  mutate(month = month(date, label = TRUE)) %>%
  mutate(day = wday(date, label = TRUE)) %>%
  mutate(source = "matatu") %>%
  filter(!is.na(county)) %>% filter(injuries != "Both") %>%
  filter(county %in% counties) %>%
  filter(year >= 2012)

matatu_accidents_may2may <- matatu_accidents %>% 
  filter(date >= ymd("2012-05-01") & date <= ymd("2014-04-30"))

matatu_passengers_total<- matatu_accidents %>%
  group_by(year, passengers_cat) %>%
  summarise(n = n())

matatu_passengers_avg <- matatu_accidents %>%
  group_by(year, passengers_cat) %>%
  mutate(days = case_when(year < 2014 ~ 365,
                          year == 2014 ~ 223)) %>%
  summarise(mean = n()/max(days))

matatu_injuries_avg <- matatu_accidents %>%
  group_by(year, injuries) %>%
  mutate(days = case_when(year < 2014 ~ 365,
                          year == 2014 ~ 223)) %>%
  summarise(mean = n()/max(days))

matatu_passengers_total_may2may <- matatu_accidents_may2may %>%
  group_by(year, passengers_cat) %>%
  summarise(n = n())

matatu_passengers_avg_may2may <- matatu_accidents_may2may %>%
  group_by(year, passengers_cat) %>%
  summarise(mean = n()/365)

saveRDS(matatu_accidents_may2may, "accidents-dashboard/data/matatu_may2012_2014.rds")

matatu_accidents_feb2may <- matatu_accidents %>%
  filter(date >= ymd("2014-02-01") & date <= ymd("2014-04-30"))

matatu_ts <- matatu_accidents_may2may %>%
  mutate(date = floor_date(date, "m")) %>%
  group_by(date, injuries) %>%
  summarise(n = n()) %>%
  filter(injuries != "Both") %>%
  mutate(date_int = as.numeric(date))

#### GOVT DATA
govt_fatalities <- read_csv("govt_data/Copy of FATAL REPORT  2017 May to 2018 May-1.xlsx - 2017.csv") %>%
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
  mutate(date = ymd_hm(date_h)) %>%
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
  mutate(date = ymd_hm(date_h)) %>%
  filter(!is.na(date)) %>%
  filter(month <= "May") %>% # there are some dates in August 2018, which is before this data were collected
  filter(month >= "Feb" & month <= "Apr") %>% # we'll keep Feb. to May for now to match our Twitter data
  filter(county %in% tolower(counties)) %>% # keep only nairobi area
  mutate(hour = hour(date)) %>%
  mutate(source = "govt")

## create a data frame of counts at the county level
govt_fatalities_2018_counties <- govt_fatalities_2018 %>%
  group_by(county) %>%
  summarise(n = n()) %>%
  left_join(nairobi_counties@data[,c("OBJECTID", "name_1")], by = c("county" = "name_1"))

nairobi_counties_df <- tidy(nairobi_counties) %>%
  mutate(OBJECTID = as.numeric(id)) %>%
  left_join(govt_fatalities_2018_counties, by = "OBJECTID")

nairobi_counties_df_noNA <- nairobi_counties_df[!is.na(nairobi_counties_df$n),]

## make data frames of shared columns between Twitter, Govt, and Matatu data so we can make graphs with both
shared_names_twitter_govt <- intersect(names(govt_fatalities_2018), names(accidents))
shared_names_twitter_matatu <- c("date", "year", "month", "day", "source")

# also subset government data to just cover the months for which we have Twitter data (but in 2017)
#govt_fatalities_20182 <- govt_fatalities_2018[,shared_names] %>% filter(date >= "2017-02-01" & date <= "2017-04-26")
govt_fatalities_20182 <- govt_fatalities_2018[,shared_names_twitter_govt]
accidents2 <- accidents[,shared_names_twitter_govt]

binded_govt_twitter <- rbind(govt_fatalities_20182, accidents2)

matatu_accidents_feb2may2 <- matatu_accidents_feb2may[shared_names_twitter_matatu]
accidents3 <- accidents[,shared_names_twitter_matatu]
binded_matatu_twitter <- rbind(matatu_accidents_feb2may2, accidents3)

# months don't overlap
binded_govt_twitter_month <- binded_govt_twitter %>%
  group_by(month, source) %>%
  summarise(n = n())

binded_govt_twitter_day <- binded_govt_twitter %>%
  group_by(day, source) %>%
  summarise(n = n())

binded_govt_twitter_hour <- binded_govt_twitter %>%
  group_by(hour, source) %>%
  summarise(n = n())

### Matatu data
## create a data frame of counts at the constituency level
matatu_accidents_constituency <- matatu_accidents_may2may %>%
  group_by(constituency) %>%
  summarise(n = n()) %>%
  left_join(nairobi_constituencies@data[,c("OBJECTID", "NAME_2")], by = c("constituency" = "NAME_2"))

nairobi_df <- tidy(nairobi_constituencies) %>%
  mutate(OBJECTID = as.numeric(id)) %>%
  left_join(matatu_accidents_constituency, by = "OBJECTID")

nairobi_df_noNA <- nairobi_df[!is.na(nairobi_df$n),]

# create a color palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#### Twitter Plots
### plot accidents by month
twitter_month <- ggplot(accidents) +
  geom_bar(aes(x = month), color = "black", fill = cbPalette[2]) +
  xlab("Month") +
  ylab("Number of Accidents") +
  theme_classic() + 
  ggtitle("Geocoded Accidents in Nairobi by Month \n Feb. 2018 to April 2018") +
  theme(plot.title = element_text(lineheight = 1, hjust = 0.5))

### plot accidents by day of week
twitter_day <- ggplot(accidents) +
  geom_bar(aes(x = day), color = "black", fill = cbPalette[2]) +
  xlab("Day of Week") +
  ylab("Number of Accidents") +
  theme_classic() + 
  ggtitle("Geocoded Accidents in Nairobi by Day of the Week \n Feb. 2018 to April 2018") +
  theme(plot.title = element_text(lineheight = 1, hjust = 0.5))

### plot accidents by hour of day
twitter_hour <- ggplot(accidents) +
  geom_bar(aes(x = hour), color = "black", fill = cbPalette[2]) +
  xlab("Hour of Day") +
  ylab("Number of Accidents") +
  theme_classic() + 
  ggtitle("Geocoded Accidents in Nairobi by Hour of Day \n Feb. 2018 to April 2018") +
  theme(plot.title = element_text(lineheight = 1, hjust = 0.5))

## plot accidents by hour of day and fill by day

twitter_hour_day <- ggplot(accidents) +
  geom_bar(aes(x = hour, fill = day), color = "black") +
  scale_fill_manual(values=cbPalette, name = "Day of Week") +
  xlab("Hour of Day") +
  ylab("Number of Accidents") +
  theme_classic() + 
  ggtitle("Geocoded Accidents in Nairobi by Hour of Day and Day of Week \n Feb. 2018 to April 2018") +
  theme(plot.title = element_text(lineheight = 1, hjust = 0.50))

twitter_line <- ggplot(accidents_ts) +
  geom_line(aes(x = date, y = n), color = cbPalette[2]) + 
  xlab("Day") +
  ylab("Number of Accidents") +
  ggtitle("Daily Accidents Identified from Twitter (Feb 2018 to April 2018)") +
  theme_classic() +
  theme(plot.title = element_text(lineheight = 0.5, hjust = 0.5))

#### Twitter and Govt Data Plots
#### Twitter Plots
source_names <- c(
  "twitter" = "Twitter \n (Feb - Apr 2018)",
  "govt" = "Government \n (Feb - Apr 2018)"
)

### plot accidents by month
# for the month data, there's no overlap
govt_twitter_month <- ggplot(binded_govt_twitter) +
  geom_bar(aes(x = month), color = "black", fill = cbPalette[2]) +
  facet_grid(source ~ ., labeller = as_labeller(source_names)) +
  theme_classic() +
  xlab("Month") +
  ylab("Number of Accidents") +
  ggtitle("Accidents in Nairobi by Month") +
  theme(plot.title = element_text(lineheight = 1, hjust = 0.5))

### plot accidents by day of week
govt_twitter_day <- ggplot(binded_govt_twitter) +
  geom_bar(aes(x = day), color = "black", fill = cbPalette[2]) +
  facet_grid(source ~ ., labeller = as_labeller(source_names)) +
  xlab("Day of Week") +
  ylab("Number of Accidents") +
  theme_classic() + 
  ggtitle("Accidents in Nairobi by Day of the Week") +
  theme(plot.title = element_text(lineheight = 1, hjust = 0.5))

### plot accidents by hour of day
govt_twitter_hour <- ggplot(binded_govt_twitter) +
  geom_bar(aes(x = hour), color = "black", fill = cbPalette[2]) +
  facet_grid(source ~ ., labeller = as_labeller(source_names)) +
  xlab("Hour of Day") +
  ylab("Number of Accidents") +
  theme_classic() + 
  ggtitle("Accidents in Nairobi by Hour of Day") +
  theme(plot.title = element_text(lineheight = 0.5, hjust = 0.5))

## plot accidents by hour of day and fill by day
govt_twitter_hour_day <- ggplot(binded_govt_twitter) +
  geom_bar(aes(x = hour, fill = day), color = "black") +
  scale_fill_manual(values=cbPalette, name = "Day of Week") +
  facet_grid(source ~ ., labeller = as_labeller(source_names)) +
  xlab("Hour of Day") +
  ylab("Number of Accidents") +
  theme_classic() + 
  ggtitle("Accidents in Nairobi by Hour of Day and Day of Week") +
  theme(plot.title = element_text(lineheight = 0.5, hjust = 0.50))

## Grouped Govt. Twitter Bars
ggplot(binded_govt_twitter_day) +
  geom_bar(aes(x = day, y = n, fill = source), color = "black", 
           stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = cbPalette, name = NULL, labels = c("Govt.", "Twitter")) + 
  theme_classic() +
  xlab("Day") +
  ylab("Number of Accidents") +
  ggtitle("Accidents in Nairobi by Day") +
  theme(plot.title = element_text(lineheight = 0.5, hjust = 0.5))

ggplot(binded_govt_twitter_hour) +
  geom_bar(aes(x = hour, y = n, fill = source), color = "black", 
           stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = cbPalette, name = NULL, labels = c("Govt.", "Twitter")) + 
  theme_classic() +
  xlab("Hour") +
  ylab("Number of Accidents") +
  ggtitle("Accidents in Nairobi by Hour of Day") +
  theme(plot.title = element_text(lineheight = 0.5, hjust = 0.5))

#### Matatu Plots
matatu_county <- ggplot(matatu_accidents) +
  geom_bar(aes(x = county, fill = injuries), color = "black") +
  scale_fill_manual(values = cbPalette, name = NULL) +
  theme_classic() + 
  ylab("Number of Accidents") +
  xlab("County") +
  ggtitle("Matatu Accidents by County (Jan-2012 to Aug-2014)") +
  theme(plot.title = element_text(lineheight = 0.5, hjust = 0.5))

matatu_total_year <- ggplot(matatu_accidents) +
  geom_bar(aes(x = year, fill = injuries), color = "black") +
  scale_fill_manual(values = cbPalette, name = NULL) +
  theme_classic() + 
  xlab("Year") +
  ylab("Number of Accidents") +
  ggtitle("Total Matatu Accidents by Year (Jan-2012 to Aug-2014)") +
  theme(plot.title = element_text(lineheight = 0.5, hjust = 0.5))

matatu_injuries_avg_year <- ggplot(matatu_injuries_avg) +
  geom_col(aes(x = year, y = mean, color = injuries, fill = injuries), color = "black",
           position = position_dodge()) +
  geom_text(aes(x = year, y = mean, label = round(mean, 2)),
            nudge_x = -0.12, nudge_y = 0.5) +
  scale_fill_manual(values = cbPalette, name = NULL) +
  theme_classic() + 
  xlab("Year") +
  ylab("Average Number of Accidents per Day") +
  ggtitle("Average Daily Matatu Accidents by Year and Injury Status (Jan-2012 to Aug-2014)",
          subtitle = "2012 and 2013 have 365 days, while 2014 has 223 days") +
  theme(plot.title = element_text(lineheight = 0.5, hjust = 0.5),
        plot.subtitle = element_text(lineheight = 0.5, hjust = 0.5))

matatu_month <- ggplot(matatu_accidents) +
  geom_bar(aes(x = month, fill = injuries), color = "black") +
  scale_fill_manual(values = cbPalette, name = NULL) +
  theme_classic() + 
  xlab("Month") +
  ylab("Number of Accidents") +
  ggtitle("Matatu Accidents by Month (Jan-2012 to Aug-2014)") +
  theme(plot.title = element_text(lineheight = 0.5, hjust = 0.5))

matatu_day <- ggplot(matatu_accidents) +
  geom_bar(aes(x = day, fill = injuries), color = "black") +
  scale_fill_manual(values = cbPalette, name = NULL) +
  theme_classic() +
  xlab("Day of Week") +
  ylab("Number of Accidents") +
  ggtitle("Matatu Accidents by Day of Week (Jan-2012 to Aug-2014)")

matatu_injuries_passengers <- ggplot(matatu_accidents) +
  geom_point(aes(x = passengers, y = injuredpersons, color = county)) + 
  scale_color_manual(values = cbPalette, name = "County") +
  theme_classic() + 
  xlab("Number of Passengers") +
  ylab("Number of Injuries") +
  ggtitle("Injuries by Number of Passengers Involved in Matatu Accidents (Jan-2012 to Aug-2014)") +
  theme(plot.title = element_text(lineheight = 0.5, hjust = 0.5))

matatu_total_passengers_year <- ggplot(matatu_passengers_total) +
  geom_bar(aes(x = year, y = n, color = passengers_cat, fill = passengers_cat), color = "black", 
           stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = cbPalette, name = "Number of Passengers") + 
  theme_classic() +
  xlab("Year") +
  ylab("Number of Accidents") +
  ggtitle("Number of Matatu Accidents by No. Passengers (Jan-2012 to Aug-2014)") +
  theme(plot.title = element_text(lineheight = 0.5, hjust = 0.5))

matatu_avg_passengers_year <- ggplot(matatu_passengers_avg) +
  geom_bar(aes(x = year, y = mean, color = passengers_cat, fill = passengers_cat), color = "black", 
           stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = cbPalette, name = "Number of Passengers") + 
  theme_classic() +
  xlab("Year") +
  ylab("Average Number of Accidents per Day") +
  ggtitle("Average Daily Matatu Accidents by No. Passengers (Jan-2012 to Aug-2014)",
          subtitle = "2012 and 2013 have 365 days, while 2014 has 223") +
  theme(plot.title = element_text(lineheight = 0.5, hjust = 0.5), 
        plot.subtitle = element_text(lineheight = 0.5, hjust = 0.5))

matatu_area <- ggplot(matatu_ts) +
  geom_area(aes(x = date, y = n, color = injuries, fill = injuries), alpha = 0.5) + 
  scale_fill_manual(values = cbPalette, name = NULL) +
  scale_color_manual(values = cbPalette, guide = FALSE) +
  xlab("Year-Month") +
  ylab("Number of Accidents") +
  ggtitle("Number of Matatu Accidents (May 2012 to May 2014)") +
  theme_classic() +
  theme(plot.title = element_text(lineheight = 0.5, hjust = 0.5))

matatu_line <- ggplot(matatu_ts) +
  geom_line(aes(x = date, y = n, color = injuries)) + 
  scale_color_manual(values = cbPalette, name = NULL) +
  xlab("Year-Month") +
  ylab("Number of Accidents") +
  ggtitle("Number of Matatu Accidents (May 2012 to May 2014)") +
  theme_classic() +
  theme(plot.title = element_text(lineheight = 0.5, hjust = 0.5))

area_animate <- ggplot(matatu_ts, aes(frame = date_int, cumulative = TRUE)) +
  geom_area(aes(x = date, y = n, color = injuries, fill = injuries), alpha = 0.5) + 
  scale_fill_manual(values = cbPalette, name = NULL) +
  scale_color_manual(values = cbPalette, guide = FALSE) +
  xlab("Year-Month") +
  ylab("Number of Accidents") +
  ggtitle("Number of Matatu Accidents (May 2012 to May 2014)") +
  theme_classic() +
  theme(plot.title = element_text(lineheight = 0.5, hjust = 0.5))

gganimate(area_animate, filename = "visualizations/matatu_plots/animated_area_monthly.gif", title_frame = FALSE, ani.width = 700, interval = 0.25)
  
line_animate <- ggplot(matatu_ts, aes(frame = date_int, cumulative = TRUE)) +
  geom_line(aes(x = date, y = n, color = injuries)) + 
  scale_color_manual(values = cbPalette, name = NULL) +
  xlab("Year-Month") +
  ylab("Number of Accidents") +
  ggtitle("Number of Matatu Accidents (May 2012 to May 2014)") +
  theme_classic() +
  theme(plot.title = element_text(lineheight = 0.5, hjust = 0.5))

gganimate(line_animate, filename = "visualizations/matatu_plots/animated_line_monthly.gif", title_frame = FALSE, ani.width = 700, interval = 0.25)

#### Twitter and Matatu Data Plots
#### Twitter Plots
source_names_matatu <- c(
  "twitter" = "Twitter \n (Feb - Apr 2018)",
  "matatu" = "Matatu \n (Feb - Apr 2014)"
)

### plot accidents by month
# for the month data, there's no overlap
matatu_twitter_month <- ggplot(binded_matatu_twitter) +
  geom_bar(aes(x = month), color = "black", fill = cbPalette[2]) +
  facet_grid(source ~ ., labeller = as_labeller(source_names_matatu)) +
  theme_classic() +
  xlab("Month") +
  ylab("Number of Accidents") +
  ggtitle("Accidents in Nairobi by Month") +
  theme(plot.title = element_text(lineheight = 1, hjust = 0.5))

### plot accidents by day of week
matatu_twitter_day <- ggplot(binded_matatu_twitter) +
  geom_bar(aes(x = day), color = "black", fill = cbPalette[2]) +
  facet_grid(source ~ ., labeller = as_labeller(source_names_matatu)) +
  xlab("Day of Week") +
  ylab("Number of Accidents") +
  theme_classic() + 
  ggtitle("Accidents in Nairobi by Day of the Week") +
  theme(plot.title = element_text(lineheight = 1, hjust = 0.5))

#### Map Matatu data
### map twitter accidents
map.2 <- get_googlemap(center = c(lon = 36.8219, lat = -1.2921), zoom = 10, scale = 2, maptype = "hybrid")

matatu_map_guide <- ggmap(map.2) +
  geom_polygon(data = nairobi_df_noNA, aes(x = long, y = lat, fill = n, group = group), alpha = 0.3,
               color = "white", size = 0.1) +
  scale_fill_continuous(low = cbPalette[1], high = cbPalette[2], "Number of \nMatatu Accidents") +
  ggtitle("Matatu Accidents at Constituency-Level (Feb. 2017 to Apr. 2017)") +
  theme(plot.title = element_text(lineheight = 1, hjust = 0.5)) +
  theme(line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank()) +
  coord_equal()

matatu_map_noGuide <- ggmap(map.2) +
  geom_polygon(data = nairobi_df_noNA, aes(x = long, y = lat, fill = n, group = group), alpha = 0.3,
               color = "white", size = 0.1) +
  scale_fill_continuous(low = cbPalette[1], high = cbPalette[2], "Number of \nMatatu Accidents", 
                        guide = FALSE) +
  ggtitle("Matatu") +
  theme(plot.title = element_text(lineheight = 1, hjust = 0.5)) +
  theme(line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank()) +
  coord_equal()

### map govt. data
govt_map_guide <- ggmap(map.2) +
  geom_polygon(data = nairobi_counties_df_noNA, aes(x = long, y = lat, fill = n, group = group), 
               alpha = 0.3, color = "white", size = 0.1) +
  scale_fill_continuous(low = cbPalette[1], high = cbPalette[2], "Number of \nFatal Accidents (Govt.)") +
  ggtitle("Govt. Fatalities at County-Level (Feb. 2018 to Apr. 2018)") +
  theme(plot.title = element_text(lineheight = 1, hjust = 0.5)) +
  theme(line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank()) +
  coord_equal()

govt_map_noGuide <- ggmap(map.2) +
  geom_polygon(data = nairobi_counties_df_noNA, aes(x = long, y = lat, fill = n, group = group), 
               alpha = 0.3, color = "white", size = 0.1) +
  scale_fill_continuous(low = cbPalette[1], 
                        high = cbPalette[2], "Number of \nFatal Accidents (Govt.)", guide = FALSE) +
  ggtitle("Govt.") +
  theme(plot.title = element_text(lineheight = 1, hjust = 0.5)) +
  theme(line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank()) +
  coord_equal()

### map twitter data
twitter_map <- ggmap(map.2) +
  geom_point(data = accidents, aes(x = lon.wgs84, y = lat.wgs84), shape = 21,
             alpha = 1, size = 2, fill = cbPalette[2], color = "black", stroke = 0.5) +
  ggtitle("Twitter Accidents (Feb. 2018 to April 2018)") +
  theme(plot.title = element_text(lineheight = 1, hjust = 0.5)) +
  theme(line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank()) +
  coord_equal()

twitter_map_short_title <- ggmap(map.2) +
  geom_point(data = accidents, aes(x = lon.wgs84, y = lat.wgs84), shape = 21,
             alpha = 1, size = 2, fill = cbPalette[2], color = "black", stroke = 0.5) +
  ggtitle("Twitter") +
  theme(plot.title = element_text(lineheight = 1, hjust = 0.5)) +
  theme(line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank()) +
  coord_equal()

### animated twitter map by hour of day
p <- ggmap(map.2) + 
  geom_point(data = accidents, aes(x = lon.wgs84, y = lat.wgs84, frame = hour_int, cumulative = TRUE), 
             alpha = 1, size = 3, color = "red") +
  theme(line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank())

gganimate(p, filename = "visualizations/crash_map_timeDay.gif", title_frame = FALSE, 
          ani.width = 700, interval = 2)

# grid (hotspot) mapping
epsg <- 21037
crs_string <- proj4string(nairobi_counties)
nairobi_counties_sf <- nairobi_counties %>% st_as_sf() %>% st_transform(epsg)

grid_1km <- st_make_grid(nairobi_counties_sf, cellsize = c(1000, 1000)) %>% 
  st_sf(grid_id = 1:length(.)) %>% st_transform(epsg)

grid_2km <- st_make_grid(nairobi_counties_sf, cellsize = c(2000, 2000)) %>% 
  st_sf(grid_id = 1:length(.)) %>% st_transform(epsg)

grid_3km <- st_make_grid(nairobi_counties_sf, cellsize = c(3000, 3000)) %>% 
  st_sf(grid_id = 1:length(.)) %>% st_transform(epsg)

grid_1km_counties <- st_intersection(grid_1km, nairobi_counties_sf)
grid_2km_counties <- st_intersection(grid_2km, nairobi_counties_sf)
grid_3km_counties <- st_intersection(grid_3km, nairobi_counties_sf)

accidents_sp <- accidents[,c("lon.wgs84", "lat.wgs84")]
coordinates(accidents_sp) <- ~ lon.wgs84 + lat.wgs84
projection(accidents_sp) <- projection(ke_counties)

pts <- accidents_sp %>% st_as_sf() %>% st_transform(epsg)

pointsNgrids_1km <- pts %>% st_join(grid_1km_counties, join = st_intersects) %>% as.data.frame
pointsNgrids_2km <- pts %>% st_join(grid_2km_counties, join = st_intersects) %>% as.data.frame
pointsNgrids_3km <- pts %>% st_join(grid_3km_counties, join = st_intersects) %>% as.data.frame

# 1km grids get us some hot spots
percentages_1km <- round(table(pointsNgrids_1km$grid_id)/sum(table(pointsNgrids_1km$grid_id)), 2)*100
max(percentages_1km)
percentages_1km <- percentages_1km[order(percentages_1km, decreasing = TRUE)]
sum(percentages_1km[c(1:10)])
top_27_1km <- percentages_1km[c(1:10)]
top_27_1km_ids <- as.numeric(names(top_27_1km))
top_27_1km_grids <- grid_1km_counties[match(top_27_1km_ids, grid_1km_counties$grid_id),]

# 2km grids
percentages_2km <- round(table(pointsNgrids_2km$grid_id)/sum(table(pointsNgrids_2km$grid_id)), 2)*100
max(percentages_2km)
percentages_2km <- percentages_2km[order(percentages_2km, decreasing = TRUE)]
top_38_2km <- percentages_2km[c(1:10)]
sum(top_38_2km)
top_38_2km_ids <- as.numeric(names(top_38_2km))
top_38_2km_grids <- grid_2km_counties[match(top_38_2km_ids, grid_2km_counties$grid_id),]

# 3km grids
percentages_3km <- round(table(pointsNgrids_3km$grid_id)/sum(table(pointsNgrids_3km$grid_id)), 2)*100
percentages_3km <- percentages_3km[order(percentages_3km, decreasing = TRUE)]
top_46_3km <- percentages_3km[c(1:10)]
sum(top_46_3km) # 34% of accidents happen in 7 3x3km areas
top_46_3km_ids <- as.numeric(names(top_46_3km))
top_46_3km_grids <- grid_3km_counties[match(top_46_3km_ids, grid_3km_counties$grid_id),]

### 2x2km grids
# in which counties are the grids?
top_38_2km_counties <- table(pointsNgrids_2km$NAME_1[match(top_38_2km_ids, pointsNgrids_2km$grid_id)])
machakos_nairobi_sf <- nairobi_counties_sf[match(names(top_38_2km_counties), nairobi_counties_sf$NAME_1),]

### 1x1km grids
# in which counties are the grids?
top_27_1km_counties <- table(pointsNgrids_1km$NAME_1[match(top_27_1km_ids, pointsNgrids_1km$grid_id)])
machakos_kiambu_nairobi_sf <- nairobi_counties_sf[match(names(top_27_1km_counties), nairobi_counties_sf$NAME_1),]

#### Map Grids
# create labels for each grid_id - these aren't working right now
grid_lab_2km <- centroid(top_38_2km_grids_sp) %>% cbind(coordinates(.)) 
grid_lab_1km <- st_centroid(top_27_1km_grids) %>% cbind(st_coordinates(.))

grid_map <- ggplot() +
  geom_sf(data = machakos_nairobi_sf, fill = 'white', lwd = 0.05) +
  geom_sf(data = top_38_2km_grids, lwd = 0.3, fill = "#56B4E9", color = "black", alpha = .5) +
  geom_text(data = grid_lab, aes(x = X, y = Y, label = grid_id), size = 2) +
  coord_sf(datum = NA)  +
  labs(x = "") +
  labs(y = "")

# use ggplot2 to map top grids over machakos and nairobi
# first convert back to sp objects because geom_sf is kind of sketchy
top_38_2km_grids_sp <- top_38_2km_grids %>% st_transform(crs_string) %>% as("Spatial")
top_27_1km_grids_sp <- top_27_1km_grids %>% st_transform(crs_string) %>% as("Spatial")

machakos_nairobi_sp <- machakos_nairobi_sf %>% st_transform(crs_string) %>% as("Spatial")
machakos_kiambu_nairobi_sp <- machakos_kiambu_nairobi_sf %>% st_transform(crs_string) %>% as("Spatial")

# then turn into data frames
top_38_2km_grids_df <- tidy(top_38_2km_grids_sp)
top_27_1km_grids_df <- tidy(top_27_1km_grids_sp)

machakos_nairobi_df <- tidy(machakos_nairobi_sp)
machakos_kiambu_nairobi_df <- tidy(machakos_kiambu_nairobi_sp)

map.3 <- get_googlemap(center = c(lon = 36.8219, lat = -1.2921), zoom = 11, scale = 2, maptype = "hybrid")

grid_map_2km <- ggmap(map.3) +
  geom_polygon(data = top_38_2km_grids_df, aes(x = long, y = lat, group = group),
               alpha = 0.5, size = 1, fill = cbPalette[1], color = "black") +
  ggtitle("Ten 2kmX2km Grids Account for 38% of Accidents on Twitter") +
  theme(plot.title = element_text(lineheight = 1, hjust = 0.5)) +
  theme(line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank()) +
  coord_equal()

map.4 <- get_googlemap(center = c(lon = 36.865, lat = -1.2921), zoom = 12, scale = 2, maptype = "hybrid")

grid_map_1km <- ggmap(map.4) +
  geom_polygon(data = top_27_1km_grids_df, aes(x = long, y = lat, group = group),
               alpha = 0.5, size = 0.5, fill = cbPalette[1], color = "black") +
  ggtitle("Ten 1kmX1km Grids Account for 27% of Accidents on Twitter") +
  theme(plot.title = element_text(lineheight = 1, hjust = 0.5)) +
  theme(line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank()) +
  coord_equal()


ggsave("visualizations/twitter_plots/twitter_hour.png", twitter_hour)
ggsave("visualizations/twitter_plots/twitter_day.png", twitter_day)
ggsave("visualizations/twitter_plots/twitter_month.png", twitter_month)
ggsave("visualizations/twitter_plots/twitter_hour_day.png", twitter_hour_day)
ggsave("visualizations/twitter_plots/twitter_line.png", twitter_line)

ggsave("visualizations/govt_plots/govt_twitter_day.png", govt_twitter_day)
ggsave("visualizations/govt_plots/govt_twitter_hour.png", govt_twitter_hour)
ggsave("visualizations/govt_plots/govt_twitter_month.png", govt_twitter_month)

ggsave("visualizations/matatu_plots/matatu_injuries_passengers.png", matatu_injuries_passengers)
ggsave("visualizations/matatu_plots/matatu_line_ts.png", matatu_line)
ggsave("visualizations/matatu_plots/matatu_area_ts.png", matatu_area)
ggsave("visualizations/matatu_plots/matatu_twitter_day.png", matatu_twitter_day)
ggsave("visualizations/matatu_plots/matatu_twitter_month.png", matatu_twitter_month)
ggsave("visualizations/matatu_plots/matatu_total_year.png", matatu_total_year)
ggsave("visualizations/matatu_plots/matatu_total_passengers_year.png", matatu_total_passengers_year)
ggsave("visualizations/matatu_plots/matatu_avg_passengers_year.png", matatu_avg_passengers_year)
ggsave("visualizations/matatu_plots/matatu_avg_injuries_year.png", matatu_injuries_avg_year)

ggsave("visualizations/maps/grid_map_2km.png", grid_map_2km)
ggsave("visualizations/maps/grid_map_1km.png", grid_map_1km)
ggsave("visualizations/maps/govt_map_counties.png", govt_map_guide)
ggsave("visualizations/maps/matatu_map_constituencies.png", matatu_map_guide)
ggsave("visualizations/maps/twitter_map.png", twitter_map)
ggsave("visualizations/maps/accident_compare_map.png", arrangeGrob(twitter_map_short_title, matatu_map_noGuide, govt_map_noGuide, 
                                                              nrow = 1, ncol = 3))


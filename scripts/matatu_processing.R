library(googleway)
library(raster)
library(readstata13)
library(sp)
library(stringdist)
library(tidyverse)

accidents <- read.dta13("matatu_data/incidents.dta") %>% filter(p_station != "")
geonames_cols <- c("geonameid", "name", "asciiname", "alternatenames", "latitude", "longitude",
                   "featureclass", "featurecode", "countrycode", "cc2", "adm1", "adm2", "adm3", "adm4",
                   "population", "elevation", "dem", "timezone", "modificationdate")
geonames <- read_tsv("supporting_files/KE/KE.txt", col_names = geonames_cols)

# how many unique police stations are there?
matatu_police_stations <- unique(accidents$p_station)
length(matatu_police_stations)

#### Geocode Police Stations using the Google Places API
geocode_stations <- function(x) {
  results <- google_places(x, place_type = "police")
  coords <- results$results$geometry$location[1,]
  
  if(!is.null(coords)) {
    tibble(p_station = x, long = coords$lng, lat = coords$lat)
  } else{
    tibble(p_station = x, long = NA, lat = NA)
  }
}

geocoded_stations <- tibble()

for(i in matatu_police_stations) {
  station <- geocode_stations(i)
  geocoded_stations <- rbind(geocoded_stations, station)
  rm(station)
}

## Now let's see if the stations that weren't geocoded can be matched with geocoded stations
stations_geo_index <- which(!is.na(geocoded_stations$long))
stations_geo <- geocoded_stations[stations_geo_index, "p_station"][[1]]
stations_no_geo <- geocoded_stations[-stations_geo_index, "p_station"][[1]]

## find matches of police stations that don't have exact matches
match_stations <- function(station) {
  maxDist <- length(str_split(station, pattern = "")[[1]])
  matched <- amatch(station, stations_geo, method = "lv", maxDist=3)
  matched_station <- stations_geo[matched]
  geocoded_matched <- geocoded_stations[match(matched_station, geocoded_stations$p_station), c("p_station", "long", "lat")]
  geocoded_matched <- cbind(geocoded_matched, original_station = station)
}

matched_geocoded_stations <- tibble()
for(i in stations_no_geo) {
  matched <- match_stations(i)
  matched_geocoded_stations <- rbind(matched_geocoded_stations, matched)
  rm(matched)
}

## Manually go through the matches and keep the ones that make sense
matches2keep <- c(66, 104, 200)
matched_geocoded_stations_verified <- matched_geocoded_stations[matches2keep,]
matched_geocoded_stations_names_unverified <- as.character(matched_geocoded_stations[-matches2keep, "original_station"])

## Now join exact matches followed by near matches
accidents_geo <- accidents %>%
  left_join(geocoded_stations[stations_geo_index,]) %>%
  left_join(matched_geocoded_stations_verified, by = c("p_station" = "original_station")) %>%
  mutate(long = case_when(!is.na(long.x) ~ long.x, is.na(long.x) ~ long.y)) %>%
  mutate(lat = case_when(!is.na(lat.x) ~ lat.x, is.na(lat.x) ~ lat.y)) %>%
  dplyr::select(-c(long.x, lat.x, long.y, lat.y)) %>%
  rename(matched_station = p_station.y)

# find how many unique police stations are in the geonames database
geonames_police_stations <- geonames %>% filter(featurecode == "PP")
length(geonames_police_stations$name)
geonames_stations <- geonames_police_stations$name

# check if these can be matched to non-geocoded stations
geonames_match_stations <- function(station) {
  
  station <- tolower(gsub("(POLICE STATION)|(POLICE POST)", "", station))
  matched <- amatch(station, geonames_stations, method = "lv", maxDist=3)
  matched_station <- matched_geocoded_stations_names_unverified[matched]
  geocoded_matched <- geonames_police_stations[match(matched_station, geonames_police_stations$name), 
                                                        c("name", "longitude", "latitude")]
  geocoded_matched <- cbind(geocoded_matched, original_station = station)
  
}

matched_geocoded_geonames_stations <- tibble()
for(i in matched_geocoded_stations_names_unverified) {
  matched <- geonames_match_stations(i)
  matched_geocoded_geonames_stations <- rbind(matched_geocoded_geonames_stations, matched)
  rm(matched)
}

#### Add a counties column
length(which(!is.na(accidents_geo$long)))/nrow(accidents_geo)

ke_country <- getData("GADM", level = 0, country = "KEN")
ke_counties <- getData("GADM", level = 1, country = "KEN")
plot(ke_counties)
points(accidents_geo[,c("long", "lat")])

names(accidents_geo)

accidents_sp <- na.omit(accidents_geo[,c("p_station", "long", "lat", 
                                         "incidentdate", "passengers", "maincategory",
                                         "injuredpersons")])
accidents_geo2 <- accidents_sp
coordinates(accidents_sp) <- ~ long + lat
projection(accidents_sp) <- projection(ke_counties)

accidents_geo2$county <- NA

counties <- c("Nairobi", "Kiambu", "Murang'a", "Kajiado", "Machakos")

for(county in counties) {
  
  ke_county <- ke_counties[ke_counties@data$NAME_1 == county,]
  over_accidents <- over(accidents_sp, ke_county)
  accidents_geo2[which(!is.na(over_accidents$ID_0)),"county"] <- county
  
  rm(over_accidents)
  
}

#### Add a constituency column
ke_constituencies <- getData("GADM", level = 2, country = "KEN")
nairobi_constituencies <- ke_constituencies[ke_constituencies@data$NAME_1 %in% counties,]

constituencies <- nairobi_constituencies@data$NAME_2

for(constituency in constituencies) {
  
  nairobi_constituency <- nairobi_constituencies[nairobi_constituencies@data$NAME_2 == constituency,]
  over_accidents <- over(accidents_sp, nairobi_constituency)
  accidents_geo2[which(!is.na(over_accidents$ID_0)),"constituency"] <- constituency
  
  rm(over_accidents)
  
}

#### save final data for matatu accidents
write_csv(accidents_geo, "matatu_data/geocoded_unprocessed_incidents.csv")
write_csv(accidents_geo2, "matatu_data/processed_incidents.csv")






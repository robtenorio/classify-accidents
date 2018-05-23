library(ggmap)
library(raster)
library(sf)
library(tidyverse)

# geo data
epsg <- 21037
crs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
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

ke_country <- getData("GADM", level = 0, country = "KEN") %>% st_as_sf() %>%
  st_transform(crs)
ke_counties <- getData("GADM", level = 1, country = "KEN") %>% st_as_sf() %>%
  st_transform(crs)
ke_constituencies <- getData("GADM", level =2, country = "KEN") %>% st_as_sf() %>% 
  st_transform(crs)

nairobi_counties <- ke_counties %>%
  filter(NAME_1 %in% counties)

greater_nairobi_constituencies <- ke_constituencies %>%
  filter(NAME_1 %in% counties)

nairobi_constituencies <- ke_constituencies %>%
  filter(NAME_1 %in% "Nairobi")





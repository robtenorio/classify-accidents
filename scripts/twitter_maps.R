library(ggmap)
library(ggplot2)
library(tidyverse)

crash_sites <- read_csv("tweets/truthdata_allrounds.csv") %>%
  select(id, accident_truth, latitude_truth, longitude_truth, landmark_c2) %>%
  set_names("id", "accident", "lat", "long", "landmark")

crash_sites_2013_2018 <- read_csv("tweets/accidents_unique_algorithm.csv") %>%
  rename(long = longitude, lat = latitude) %>%
  select(long, lat) %>%
  filter(!is.na(long)) %>%
  filter(!is.na(lat))

crash_sites_geo <- crash_sites %>%
  filter(!is.na(lat) & !is.na(long))

map <- get_googlemap(center = c(lon = 36.8219, lat = -1.2921), zoom = 12, scale = 2, maptype = "hybrid")

ggmap(map) +
  geom_point(data = crash_sites_2013_2018, aes(x = long, y = lat), alpha = 1, 
             size = 0.5, color = "skyblue")

ggsave(filename = "visualizations/crash_points.png")

crash_density <- ggmap(map) +
  geom_density2d(data = crash_sites_2013_2018,
                 aes(x = long, y = lat), size = 0.3) + 
  stat_density2d(data = crash_sites_2013_2018, aes(x = long, y = lat, fill = ..level.., alpha = ..level..), size = 0.01,
                 bins = 10, geom = "polygon") + 
  scale_fill_gradient(low = "green", high = "red", name = "Crash Density") +
  scale_alpha(range = c(0, 0.3), guide = FALSE)

ggsave(filename = "visualizations/crash_density.png")



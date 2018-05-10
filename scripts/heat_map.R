library(ggmap)
library(ggplot2)
library(tidyverse)

crash_sites <- read_csv("tweets/truthdata_allrounds.csv") %>%
  select(id, accident_truth, latitude_truth, longitude_truth, landmark_c2) %>%
  set_names("id", "accident", "lat", "long", "landmark")

crash_sites_geo <- crash_sites %>%
  filter(!is.na(lat) & !is.na(long))


map <- get_googlemap(center = c(lon = 36.8219, lat = -1.2921), zoom = 12, scale = 2, maptype = "hybrid")

ggmap(map, extent = "device") +
  geom_point(data = crash_sites_geo, aes(x = long, y = lat), alpha = 1, color = "skyblue")

ggsave(filename = "visualizations/crash_points.png")

crash_density <- ggmap(map, extent = "device") +
  geom_density2d(data = crash_sites_geo,
                 aes(x = long, y = lat), size = 0.3) + 
  stat_density2d(data = crash_sites_geo, aes(x = long, y = lat, fill = ..level.., alpha = ..level..), size = 0.01,
                 bins = 10, geom = "polygon") + 
  scale_fill_gradient(low = "green", high = "red", name = "Crash Density") +
  scale_alpha(range = c(0, 0.3), guide = FALSE)

ggsave(filename = "visualizations/crash_density.png")


ggmap(tartu_map_g_str, extent = "device") + geom_density2d(data = tartu_housing_xy_wgs84_a, 
                                                           aes(x = lon, y = lat), size = 0.3) + 
  stat_density2d(data = tartu_housing_xy_wgs84_a, 
                 aes(x = lon, y = lat, fill = ..level.., alpha = ..level..), size = 0.01, 
                 bins = 16, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0, 0.3), guide = FALSE)

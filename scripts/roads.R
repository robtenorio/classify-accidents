library(magrittr)
library(rgdal)
library(sf)
library(sp)

crs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

layer <- "ke_major-roads"
layer <- "nairobi_roads_2010"
layer <- "Nairobi_Roads_GIS"
file_name <- "roads_final_checked"
dsn <- paste(getwd(), "supporting_files", layer, sep = "/")

ke_roads <- readOGR(dsn = dsn, layer = layer) %>% st_as_sf() %>% st_transform(crs)

ke_roads <- slice(sample(1:nrow(ke_roads), 20000))

thika_roads <- ke_roads[ke_roads@data$name == "Thika Road",]
plot(ke_roads)

projection(ke_roads)
plot(thika_roads)


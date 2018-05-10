library(rgdal)
library(sp)

layer <- "ke_major-roads"
layer <- "nairobi_major_roads_2010"
layer <- "Nairobi_Roads_GIS"
file_name <- "roads_final_checked"
dsn <- paste(getwd(), "supporting_files", layer, sep = "/")

ke_roads <- readOGR(dsn = dsn, layer = file_name)

thika_roads <- ke_roads[ke_roads@data$name == "Thika Road",]
plot(ke_roads)

projection(ke_roads)
plot(thika_roads)

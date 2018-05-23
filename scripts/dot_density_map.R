### dot density map
### https://www.cultureofinsight.com/blog/2018/05/02/2018-04-08-multivariate-dot-density-maps-in-r-with-sf-ggplot2/

library(ggmap)
library(raster)
library(sf)
library(tidyverse)

# helper functions
# credit to Jens von Bergmann for this algo https://github.com/mountainMath/dotdensity/blob/master/R/dot-density.R
random_round <- function(x) {
  v=as.integer(x)
  r=x-v
  test=runif(length(r), 0.0, 1.0)
  add=rep(as.integer(0),length(r))
  add[r>test] <- as.integer(1)
  value=v+add
  ifelse(is.na(value) | value<0,0,value)
  return(value)
}

# geo data
epsg <- 21037
crs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
counties <- c("Nairobi", "Kiambu", "Murang'a", "Kajiado", "Machakos")

map <- get_googlemap(center = c(lon = 36.8219, lat = -1.2921), zoom = 12, scale = 2, maptype = "satellite")

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

# matatu data
matatu_data <- read_csv("matatu_data/matatu_aug2jul_2012_2014.csv") %>% 
  mutate(passengers_cat = as.factor(passengers_cat)) %>%
  mutate(passengers_cat = cut(passengers, c(0, 25, 50, 70)))

matatu_injuries <- matatu_data  %>% 
  filter(!is.na(injuries)) %>%
  filter(!is.na(constituency)) %>%
  group_by(constituency, injuries) %>%
  summarise(n = n()) %>%
  spread(injuries, n)

sf_data <- left_join(nairobi_constituencies, matatu_injuries, by = c("NAME_2" = "constituency"))
head(sf_data)

greater_sf_data <- left_join(greater_nairobi_constituencies, matatu_injuries, by = c("NAME_2" = "constituency"))
head(sf_data)

sf_data <- sf_data %>%
  filter(!is.na(Injuries) & !is.na(`No Injuries`))

greater_sf_data <- greater_sf_data %>%
  filter(!is.na(Injuries) & !is.na(`No Injuries`))

### Generate Coordinates for Each Dot

# data frame of number of dots to plot for each injury category
num_dots <- as.data.frame(sf_data) %>% 
  select(Injuries, `No Injuries`) %>% 
  mutate_all(random_round)

greater_num_dots <- as.data.frame(greater_sf_data) %>% 
  select(Injuries, `No Injuries`) %>% 
  mutate_all(random_round)

# generates data frame with coordinates for each point + what party it is assiciated with
sf_dots <- map_df(names(num_dots), 
                          ~ st_sample(sf_data, size = num_dots[,.x], type = "random") %>% # generate the points in each polygon
                            st_cast("POINT") %>%                                          # cast the geom set as 'POINT' data
                            st_coordinates() %>%                                          # pull out coordinates into a matrix
                            as_tibble() %>%                                               # convert to tibble
                            setNames(c("lon","lat")) %>%                                  # set column names
                            mutate(Injury = .x)                                           # add categorical Injury variable
) %>% 
  slice(sample(1:n())) # once map_df binds rows randomise order to avoid bias in plotting order

greater_sf_dots <- map_df(names(greater_num_dots), 
                          ~ st_sample(greater_sf_data, size = num_dots[,.x], type = "random") %>% # generate the points in each polygon
                            st_cast("POINT") %>%                                          # cast the geom set as 'POINT' data
                            st_coordinates() %>%                                          # pull out coordinates into a matrix
                            as_tibble() %>%                                               # convert to tibble
                            setNames(c("lon","lat")) %>%                                  # set column names
                            mutate(Injury = .x)      
) %>% 
  slice(sample(1:n())) # once map_df binds rows randomise order to avoid bias in plotting order

### passengers
matatu_passengers <- matatu_data  %>% 
  filter(!is.na(passengers_cat)) %>%
  filter(!is.na(constituency)) %>%
  group_by(constituency, passengers_cat) %>%
  summarise(n = n()) %>%
  spread(passengers_cat, n, fill = 0)

sf_data <- left_join(nairobi_constituencies, matatu_passengers, by = c("NAME_2" = "constituency")) %>% na.omit
head(sf_data)

greater_sf_data <- left_join(greater_nairobi_constituencies, matatu_passengers, by = c("NAME_2" = "constituency")) %>% na.omit
head(sf_data)

### Generate Coordinates for Each Dot

# data frame of number of dots to plot for each injury category
num_dots <- as.data.frame(sf_data) %>% 
  select(`(0,25]`:`(50,70]`) %>% 
  mutate_all(random_round)

greater_num_dots <- as.data.frame(greater_sf_data) %>% 
  select(`(0,25]`:`(50,70]`) %>% 
  mutate_all(random_round)

# generates data frame with coordinates for each point + what party it is assiciated with
sf_dots <- map_df(names(num_dots), 
                  ~ st_sample(sf_data, size = num_dots[,.x], type = "random") %>% # generate the points in each polygon
                    st_cast("POINT") %>%                                          # cast the geom set as 'POINT' data
                    st_coordinates() %>%                                          # pull out coordinates into a matrix
                    as_tibble() %>%                                               # convert to tibble
                    setNames(c("lon","lat")) %>%                                  # set column names
                    mutate(Passengers = .x)                                           # add categorical Injury variable
) %>% 
  slice(sample(1:n())) # once map_df binds rows randomise order to avoid bias in plotting order

greater_sf_dots <- map_df(names(greater_num_dots), 
                          ~ st_sample(greater_sf_data, size = num_dots[,.x], type = "random") %>% # generate the points in each polygon
                            st_cast("POINT") %>%                                          # cast the geom set as 'POINT' data
                            st_coordinates() %>%                                          # pull out coordinates into a matrix
                            as_tibble() %>%                                               # convert to tibble
                            setNames(c("lon","lat")) %>%                                  # set column names
                            mutate(Passengers = .x)      
) %>% 
  slice(sample(1:n())) # once map_df binds rows randomise order to avoid bias in plotting order

### Plot Dots
# colour palette for Injury Status
pal <- rev(c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))
pal <- c("(0,10]" = "#c4e8e2", "(10,20]" = "#b2e1d9", "(20,30]" = "#9fdad0", "(30,40]" = "#8dd3c7", 
        "(40,50]" = "#7bccbe", "(50,60]" = "#68c5b5", "(60,70]" = "#56beac")

pal <- colorspace::diverge_hsv(3)
pal <- rev(colorspace::heat_hcl(3))
pal <- colorspace::diverge_hcl(3, h = c(130,43), c = 100, l = c(70,90))
pal <- colorspace::diverge_hcl(3, l = c(50,90), c = 100, power = 1)

# plot it and save as png big enough to avoid over-plotting of the points
p_greater <- ggplot() +
  geom_point(data = greater_sf_dots, aes(lon, lat, colour = Passengers), size = 0.2, alpha = 0.7) +
  scale_colour_manual(values = pal) +
  coord_sf(datum = NA) +
  labs(x = NULL, y = NULL,
       title = "Matatu Crashes Greater Nairobi Constituencies \n(Aug. 2012 to July 2014)") +
  guides(colour = guide_legend(override.aes = list(size = 2))) +
  theme(legend.position = c(0,.03), legend.direction = "horizontal",
        legend.justification = 'left',
        legend.key.size = unit(5, "mm"),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.background = element_rect(fill = "#212121", color = NA),
        legend.key = element_rect(fill = "#212121", colour = NA),
        line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.background = element_rect(fill = "#212121", color = NA), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#212121", color = NA),
        text =  element_text(color = "white"),
        title =  element_text(color = "white"),
        plot.title = element_text(lineheight = 1, size = 10)
  )

p_greater

# plot it and save as png big enough to avoid over-plotting of the points
p <- ggplot() +
  geom_sf(data = ke_roads) +
  geom_point(data = sf_dots, aes(lon, lat, colour = Passengers), size = 0.2, alpha = 0.7) +
  scale_colour_manual(values = pal) +
  coord_sf(datum = NA) +
  labs(x = NULL, y = NULL,
       title = "Matatu Crashes Nairobi County Constituencies \n(Aug. 2012 to July 2014)",
       subtitle = "Source: Directline Insurance Provider") +
  guides(colour = guide_legend(override.aes = list(size = 2))) +
  theme(legend.position = c(0,.03), legend.direction = "horizontal",
        legend.justification = 'left',
        legend.key.size = unit(5, "mm"),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8),
        line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.background = element_rect(fill = "#212121", color = NA), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#212121", color = NA),
        legend.background = element_rect(fill = "#212121", color = NA),
        legend.key = element_rect(fill = "#212121", colour = NA),
        text =  element_text(color = "white"),
        title =  element_text(color = "white"),
        plot.title = element_text(lineheight = 1, size = 10),
        plot.subtitle = element_text(lineheight = 1, size = 6)
  )

p


####### Voronoi Polygons
matatu_points <- matatu_data %>% 
  filter(!is.na(injuries)) %>%
  group_by(long, lat, injuries) %>%
  summarise(n = n()) %>%
  spread(injuries, n, fill = 0) %>%
  ungroup() %>%
  mutate(id = 1:nrow(.)) %>%
  st_as_sf(coords = c("long", "lat"), crs = crs)

bbox_polygon <- function(x) {
  bb <- sf::st_bbox(x)
  
  p <- matrix(
    c(bb["xmin"], bb["ymin"], 
      bb["xmin"], bb["ymax"],
      bb["xmax"], bb["ymax"], 
      bb["xmax"], bb["ymin"], 
      bb["xmin"], bb["ymin"]),
    ncol = 2, byrow = T
  )
  
  sf::st_polygon(list(p))
}

box <- st_sfc(bbox_polygon(matatu_points))

voronoi <- st_voronoi(x = st_union(matatu_points), envelope = box)

plot(st_intersection(st_cast(voronoi), st_union(nairobi_counties)), col = 0)
points(matatu_data[,c("long", "lat")])

nairobi_voronoi <- st_intersection(st_cast(voronoi), st_union(nairobi_counties)) %>% 
  st_transform(crs) %>% 
  as("Spatial") %>% 
  st_as_sf() %>%
  st_transform(crs) %>%
  mutate(id = 1:nrow(.))

matatu_points_intersect <- st_intersects(matatu_points, nairobi_voronoi)

for(i in 1:length(matatu_points_intersect)) {
  matatu_points[i, "id"] <- matatu_points_intersect[[i]]
}

matatu_points_df <- matatu_points
st_geometry(matatu_points_df) <- NULL

voronoi_injuries <- left_join(nairobi_voronoi, matatu_points_df, by = "id") %>%
  mutate(Injuries = Injuries) %>%
  mutate(`No Injuries` = `No Injuries`)

# data frame of number of dots to plot for each injury category
num_dots_v <- as.data.frame(voronoi_injuries) %>% 
  select(Injuries, `No Injuries`) %>% 
  mutate_all(random_round)

# generates data frame with coordinates for each point + what party it is assiciated with
sf_dots_v <- map_df(names(num_dots_v), 
                  ~ st_sample(voronoi_injuries, size = num_dots_v[,.x], type = "random") %>% # generate the points in each polygon
                    st_cast("POINT") %>%                                          # cast the geom set as 'POINT' data
                    st_coordinates() %>%                                          # pull out coordinates into a matrix
                    as_tibble() %>%                                               # convert to tibble
                    setNames(c("lon","lat")) %>%                                  # set column names
                    mutate(Injury = .x)                                           # add categorical Injury variable
) %>% 
  slice(sample(1:n())) # once map_df binds rows randomise order to avoid bias in plotting order

# plot it and save as png big enough to avoid over-plotting of the points
p_v <- ggmap(map) +
  geom_point(data = sf_dots_v, aes(lon, lat, colour = Injury), size = 0.3, alpha = 0.7) +
  scale_colour_manual(values = pal, name = NULL) +
  coord_sf(datum = NA) +
  labs(x = NULL, y = NULL,
       title = "Injury Status of Matatu Crashes using Voronoi Polygons\n from Aug. 2012 to July 2014") +
  guides(colour = guide_legend(override.aes = list(size = 5))) +
  theme(legend.position = "bottom", legend.direction = "horizontal",
        line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.background = element_rect(fill = "#212121", color = NA), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#212121", color = NA),
        legend.background = element_rect(fill = "#212121", color = NA),
        legend.key = element_rect(fill = "#212121", colour = NA),
        text =  element_text(color = "white"),
        title =  element_text(color = "white"),
        plot.title = element_text(hjust = 0.5, lineheight = 1)
  )

p_v

ggsave("visualizations/maps/voronoi_dot_density.png", p_v, dpi = "retina")
ggsave("visualizations/maps/greater_nairobi_constituencies_dot_density.png", p_greater)
ggsave("visualizations/maps/nairobi_constituency_dot_density_major_roads.png", p)

svglite::svglite("visualizations/maps/greater_nairobi_constituencies_dot_density.svg", height = 10, width=12)
p_greater
dev.off()

svglite::svglite("visualizations/maps/nairobi_constituency_dot_density.svg", height = 10, width=12)
p
dev.off()


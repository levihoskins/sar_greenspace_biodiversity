# Read in packages

library("dplyr")
library("sf")
library("raster")
library("lubridate")
library("readr")
library("rnaturalearth")
library("rnaturalearthdata")
library("concaveman")
library("ggspatial")
library("prettymapr")
library("ggplot2")

# Read in shapefiles + add attributes
fl_shapefile <-st_read("Data/Polygons/filtered_shapefile.shp")
fl_shapefile <-st_cast(fl_shapefile, "POLYGON")
ggplot(fl_shapefile, aes(x=Park_Size_)) +
  geom_sf(data=fl_shapefile) +
  theme_bw() +
  theme(panel.grid.major=element_blank()) +
  scale_color_viridis_c(option="inferno") +
  theme(legend.position = "bottom") +
  theme(axis.text=element_text(color="black"))

fl_shapefile$geometry <- st_geometry(fl_shapefile)
fl_shapefile$area <- st_area(fl_shapefile$geometry)

# Filter based on Park_Size_ for determining greenspaces that are okay
filtered_shapefile <- fl_shapefile %>%
  dplyr::filter(Park_Size_ >=5) %>%
  dplyr::filter(Park_Size_ <= 20000)

ggplot(filtered_shapefile, aes(x=Park_Size_)) +
  geom_sf(data=filtered_shapefile) +
  theme_bw() +
  theme(panel.grid.major=element_blank()) +
  scale_color_viridis_c(option="inferno") +
  theme(legend.position = "bottom") +
  theme(axis.text=element_text(color="black"))

# Read in eBird data
dat_br <- readRDS("Data/eBird/RDS/dat_br")
dat_md <- readRDS("Data/eBird/RDS/dat_md")
dat_pb <- readRDS("Data/eBird/RDS/dat_pb")

# Convert to sf objects
br_sf <- dat_br %>%
  st_as_sf(coords=c("LONGITUDE", "LATITUDE"), crs=4326)
md_sf <- dat_md %>%
  st_as_sf(coords=c("LONGITUDE", "LATITUDE"), crs=4326)
pb_sf <- dat_pb %>%
  st_as_sf(coords=c("LONGITUDE", "LATITUDE"), crs=4326)

# Overlay the eBird data with greenspaces
## Check crs
st_crs(br_sf)
st_crs(md_sf)
st_crs(pb_sf)
st_crs(filtered_shapefile)

br_sf <- st_transform(br_sf, st_crs(filtered_shapefile))
md_sf <- st_transform(md_sf, st_crs(filtered_shapefile))
pb_sf <- st_transform(pb_sf, st_crs(filtered_shapefile))

# Filter points that are within the filtered_shapefile
br_sf_filtered <- st_intersection(br_sf, filtered_shapefile)
md_sf_filtered <- st_intersection(md_sf, filtered_shapefile)
pb_sf_filtered <- st_intersection(pb_sf, filtered_shapefile)

## Merge all point datasets
all_points <- bind_rows(br_sf, md_sf, pb_sf)

## Remove non-categorized points of filtered_shapefile
filtered_shapefile_trimmed <- filtered_shapefile %>%
  st_filter(all_points, .predicate = st_intersects)

## Plot the map of greenspaces
ggplot() +
  geom_sf(data = filtered_shapefile_trimmed, fill = "lightblue", color = "black") +
  geom_sf(data = br_sf_filtered, color = "orange", size = 0.5) +
  geom_sf(data = md_sf_filtered, color = "lightgreen", size = 0.5) +
  geom_sf(data = pb_sf_filtered, color = "pink", size = 0.5) +
  theme_minimal() +
  labs(title = "Filtered Shapefile with Points")

## Plot with map of Florida
ggplot() +
  annotation_map_tile(type = "cartolight") +
  geom_sf(data = filtered_shapefile_trimmed, fill = "lightblue", color = "black") +
  geom_sf(data = br_sf_filtered, color = "orange", size = 0.5) +
  geom_sf(data = md_sf_filtered, color = "lightgreen", size = 0.5) +
  geom_sf(data = pb_sf_filtered, color = "pink", size = 0.5) +
  theme_minimal() +
  labs(title = "Filtered Shapefile with Points")

## Count number of checklists per county
br_count <- nrow(st_filter(br_sf, filtered_shapefile_trimmed, .predicate = st_intersects))
md_count <- nrow(st_filter(md_sf, filtered_shapefile_trimmed, .predicate = st_intersects))
pb_count <- nrow(st_filter(pb_sf, filtered_shapefile_trimmed, .predicate = st_intersects))

## Count number of filtered greenspace
greenspace_count <- nrow(st_filter(filtered_shapefile_trimmed,all_points, .predicate = st_intersects))

## Total count of all points
total_count <- br_count + md_count + pb_count

## Print the results
print(paste("Number of br_sf points:", br_count)) #162606
print(paste("Number of md_sf points:", md_count)) #286788
print(paste("Number of pb_sf points:", pb_count)) #320547
print(paste("Total number of points:", total_count)) #769941
print(paste("Total number of filtered greenspace", greenspace_count)) #651


# Repeat above
## But for 20 checklists per month? Maybe 10 or just 50/year tbd (need at least 50 greenspaces)


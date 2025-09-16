# Load packages
library("tidyverse")

# Read in shapefiles + add attributes
#fl_shapefile <-st_read("Data/Polygons/filtered_shapefile.shp")
#fl_shapefile <-st_cast(fl_shapefile, "POLYGON")

#fl_shapefile$geometry <- st_geometry(fl_shapefile)
#fl_shapefile$area <- st_area(fl_shapefile$geometry)

# Filter based on Park_Size_ for determining greenspaces that are okay (m^2)
#filtered_shapefile <- fl_shapefile %>%
#  dplyr::filter(Park_Siz_1 >=5) %>%
#  dplyr::filter(Park_Siz_1 <= 1500000)

#length(unique(filtered_shapefile$Park_Addre))
#length(unique(fl_shapefile$Park_Addre))

# Read in eBird data
#dat_br <- readRDS("Data/eBird/RDS/dat_br")
#dat_md <- readRDS("Data/eBird/RDS/dat_md")
#dat_pb <- readRDS("Data/eBird/RDS/dat_pb")

# Convert to sf objects
#br_sf <- dat_br %>%
#  st_as_sf(coords=c("LONGITUDE", "LATITUDE"), crs=4326, remove = F)
#md_sf <- dat_md %>%
#  st_as_sf(coords=c("LONGITUDE", "LATITUDE"), crs=4326, remove = F)
#pb_sf <- dat_pb %>%
#  st_as_sf(coords=c("LONGITUDE", "LATITUDE"), crs=4326, remove = F)

# Overlay the eBird data with greenspaces
## Check crs
#st_crs(br_sf)
#st_crs(md_sf)
#st_crs(pb_sf)
#st_crs(filtered_shapefile)

#br_sf <- st_transform(br_sf, st_crs(filtered_shapefile))
#md_sf <- st_transform(md_sf, st_crs(filtered_shapefile))
#pb_sf <- st_transform(pb_sf, st_crs(filtered_shapefile))

# Filter points that are within the filtered_shapefile
#br_sf_filtered <- st_intersection(br_sf, filtered_shapefile)
#md_sf_filtered <- st_intersection(md_sf, filtered_shapefile)
#pb_sf_filtered <- st_intersection(pb_sf, filtered_shapefile)

## Merge all point datasets
#all_points <- bind_rows(br_sf_filtered, md_sf_filtered, pb_sf_filtered)
#saveRDS(all_points, "Data/eBird/all_points.rds")

### Read in RDS for the ### out script
all_points <- readRDS("Data/eBird/all_points.RDS")

#############################################
### Greenspaces with 5 checklists per season
#############################################
# Make season a column
valid_ids_season <- all_points %>%
  mutate(
    MONTH = factor(MONTH, levels = month.abb, ordered = TRUE),
    Season = case_when(
      MONTH %in% c("Dec", "Jan", "Feb") ~ "Overwintering",
      MONTH %in% c("Mar", "Apr", "May") ~ "Spring Migration",
      MONTH %in% c("Jun", "Jul", "Aug") ~ "Breeding",
      MONTH %in% c("Sep", "Oct", "Nov") ~ "Fall Migration"
    )
  )

## Now filter by 5 checklists per season
valid_ids_season <- valid_ids_season %>%
  st_drop_geometry() %>%
  group_by(LOCALITY.ID, Season) %>%
  summarise(lists = n_distinct(SAMPLING.EVENT.IDENTIFIER), .groups = "drop_last") %>%
  group_by(LOCALITY.ID) %>%
  filter(all(lists >= 5)) 

# Keep all data for qualifying LOCALITY.IDs
park_counts <- all_points %>%
  filter(LOCALITY.ID %in% valid_ids_season$LOCALITY.ID) %>%
  left_join(valid_ids_season %>% dplyr::select(LOCALITY.ID, Season, lists), 
            by = c("LOCALITY.ID"))

length(unique(park_counts$Park_Addre))

# Restore geometry (if lost)
final_shapefile_clean <- st_as_sf(park_counts, 
                                  geometry = st_geometry(all_points), 
                                  crs = st_crs(all_points))

length(unique(final_shapefile_clean$Park_Addre))

# Save as RDS
saveRDS(final_shapefile_clean, "Data/Intermediate_Data/final_shapefile_clean.RDS")









###########################################
### Greenspaces with at least 50 checklists
###########################################
# Filter by 50 lists
#park_counts <- all_points %>%
#  group_by(LOCALITY.ID) %>%
#  summarise(lists = length(unique(SAMPLING.EVENT.IDENTIFIER))) %>%
#  st_join(., all_points, by="Locality.ID") %>%
#  filter(lists >= 50)

#final_shapefile_clean <- na.omit(park_counts)

# Restore geometry (if lost)
#final_shapefile_clean <- st_as_sf(final_shapefile_clean, 
#                            geometry = st_geometry(all_points), 
#                            crs = st_crs(all_points))

# Save as RDS
#saveRDS(final_shapefile_clean, "Data/Intermediate_Data/final_shapefile_clean.RDS")

#########################################
### 5 checklists per greenspace per year
#########################################
### Use 5 because COVID had such a dip in numbers, as did 2010 and 2013
#########################################

# Greenspaces with at least 5 checklists in every year they appear
#valid_ids <- all_points %>%
#  mutate(YEAR = lubridate::year(OBSERVATION.DATE)) %>%
#  st_drop_geometry() %>%
#  group_by(LOCALITY.ID, YEAR) %>%
#  summarise(lists = n_distinct(SAMPLING.EVENT.IDENTIFIER), .groups = "drop_last") %>%
#  group_by(LOCALITY.ID) %>%
#  filter(all(lists >= 5))

# Keep all data for qualifying LOCALITY.IDs
#park_counts <- all_points %>%
#  filter(LOCALITY.ID %in% valid_ids$LOCALITY.ID) %>%
#  left_join(valid_ids %>% dplyr::select(LOCALITY.ID, YEAR, lists), 
#            by = c("LOCALITY.ID"))

#final_shapefile_clean <- na.omit(park_counts)

# Restore geometry (if lost)
#final_shapefile_clean <- st_as_sf(final_shapefile_clean, 
#                                  geometry = st_geometry(all_points), 
#                                  crs = st_crs(all_points))

#length(unique(final_shapefile_clean$Park_Name))

# Save as RDS
#saveRDS(final_shapefile_clean, "Data/Intermediate_Data/final_shapefile_clean.RDS")
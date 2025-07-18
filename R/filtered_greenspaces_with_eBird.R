# Load packages
library("dplyr")
library("sf")
library("lubridate")

# Read in shapefiles + add attributes
fl_shapefile <-st_read("Data/Polygons/filtered_shapefile.shp")
fl_shapefile <-st_cast(fl_shapefile, "POLYGON")

fl_shapefile$geometry <- st_geometry(fl_shapefile)
fl_shapefile$area <- st_area(fl_shapefile$geometry)

# Filter based on Park_Size_ for determining greenspaces that are okay 
filtered_shapefile <- fl_shapefile %>%
  dplyr::filter(Park_Size_ >=5) %>%
  dplyr::filter(Park_Size_ <= 1500)

# Read in eBird data
dat_br <- readRDS("Data/eBird/RDS/dat_br")
dat_md <- readRDS("Data/eBird/RDS/dat_md")
dat_pb <- readRDS("Data/eBird/RDS/dat_pb")

# Convert to sf objects
br_sf <- dat_br %>%
  st_as_sf(coords=c("LONGITUDE", "LATITUDE"), crs=4326, remove = F)
md_sf <- dat_md %>%
  st_as_sf(coords=c("LONGITUDE", "LATITUDE"), crs=4326, remove = F)
pb_sf <- dat_pb %>%
  st_as_sf(coords=c("LONGITUDE", "LATITUDE"), crs=4326, remove = F)

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
all_points <- bind_rows(br_sf_filtered, md_sf_filtered, pb_sf_filtered)
#saveRDS(all_points, "Data/eBird/all_points.rds")
all_points <- readRDS("Data/eBird/all_points.RDS")

#############################################
### Greenspaces with 15 checklists per season
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

## Now filter by 15 checklists per season
valid_ids_season <- valid_ids_season %>%
  st_drop_geometry() %>%
  group_by(LOCALITY.ID, Season) %>%
  summarise(lists = n_distinct(SAMPLING.EVENT.IDENTIFIER), .groups = "drop_last") %>%
  group_by(LOCALITY.ID) %>%
  filter(all(lists >= 15)) 

# Keep all data for qualifying LOCALITY.IDs
park_counts <- all_points %>%
  filter(LOCALITY.ID %in% valid_ids_season$LOCALITY.ID) %>%
  left_join(valid_ids_season %>% dplyr::select(LOCALITY.ID, Season, lists), 
            by = c("LOCALITY.ID"))

final_shapefile_clean <- na.omit(park_counts)

# Restore geometry (if lost)
final_shapefile_clean <- st_as_sf(final_shapefile_clean, 
                                  geometry = st_geometry(all_points), 
                                  crs = st_crs(all_points))

length(unique(final_shapefile_clean$Park_Addre))

## Rename columns to be under 10 characters for shapefile saving
final_shapefile_clean <- final_shapefile_clean %>%
  rename(
    COMMON = COMMON.NAME,
    SCIENTIFIC = SCIENTIFIC.NAME,
    LATITUDE = LATITUDE,
    LONGITUDE = LONGITUDE,
    COUNTY = COUNTY,
    STATE = STATE,
    LOCALITY = LOCALITY,
    L.ID = LOCALITY.ID,
    L.TYPE = LOCALITY.TYPE,
    DATE = OBSERVATION.DATE,
    O.COUNT = OBSERVATION.COUNT,
    OBSERV.ID = OBSERVER.ID,
    SEI = SAMPLING.EVENT.IDENTIFIER,
    MONTH = MONTH, 
    Season = Season,
    Shape_Area = Shape_Area,
    Park_Sourc = Park_Sourc,
    Park_Urban = Park_Urban,
    Park_Place = Park_Place,
    Park_Count = Park_Count,
    Park_Addre = Park_Addre,
    Park_Size_ = Park_Size_,
    Park_Siz_1 = Park_Siz_1,
    Park_Size1 = Park_Size1,
    Park_Name = Park_Name,
    area = area,
    lists = lists,
    geometry = geometry
  )

# Save as shapefile
st_write(final_shapefile_clean, "Data/Polygons/final_shapefile_clean_saved.shp",
         delete_dsn = TRUE)


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

## Rename columns to be under 10 characters for save of shp
#final_shapefile_clean <- final_shapefile_clean %>%
#  rename(
#    COMMON = COMMON.NAME,
#    SCIENTIFIC = SCIENTIFIC.NAME,
#    LATITUDE = LATITUDE,
#    LONGITUDE = LONGITUDE,
#    COUNTY = COUNTY,
#    STATE = STATE,
#    LOCALITY = LOCALITY,
#    L.ID = LOCALITY.ID.x,
#    L.TYPE = LOCALITY.TYPE,
#    DATE = OBSERVATION.DATE,
#    O.COUNT = OBSERVATION.COUNT,
#    OBSERV.ID = OBSERVER.ID,
#    SEI = SAMPLING.EVENT.IDENTIFIER,
#    MONTH = MONTH, 
#    Shape_Area = Shape_Area,
#    Park_Sourc = Park_Sourc,
#    Park_Urban = Park_Urban,
#    Park_Place = Park_Place,
#    Park_Count = Park_Count,
#    Park_Addre = Park_Addre,
#    Park_Size_ = Park_Size_,
#    Park_Siz_1 = Park_Siz_1,
#    Park_Size1 = Park_Size1,
#    Park_Name = Park_Name,
#    area = area,
#    lists = lists,
#    geometry = geometry
#  )

# Save as shapefile
#st_write(final_shapefile_clean, "Data/Polygons/final_shapefile_clean_saved.shp",
#         delete_dsn = TRUE)

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

## Rename columns to be under 10 characters for save of shp
#final_shapefile_clean <- final_shapefile_clean %>%
#  rename(
#    COMMON = COMMON.NAME,
#    SCIENTIFIC = SCIENTIFIC.NAME,
#    LATITUDE = LATITUDE,
#    LONGITUDE = LONGITUDE,
#    COUNTY = COUNTY,
#    STATE = STATE,
#    LOCALITY = LOCALITY,
#    L.ID = LOCALITY.ID,
#    L.TYPE = LOCALITY.TYPE,
#    DATE = OBSERVATION.DATE,
#    O.COUNT = OBSERVATION.COUNT,
#    OBSERV.ID = OBSERVER.ID,
#    SEI = SAMPLING.EVENT.IDENTIFIER,
#    MONTH = MONTH, 
#    Shape_Area = Shape_Area,
#    Park_Sourc = Park_Sourc,
#    Park_Urban = Park_Urban,
#    Park_Place = Park_Place,
#    Park_Count = Park_Count,
#    Park_Addre = Park_Addre,
#    Park_Size_ = Park_Size_,
#    Park_Siz_1 = Park_Siz_1,
#    Park_Size1 = Park_Size1,
#    Park_Name = Park_Name,
#    area = area,
#    lists = lists,
#    geometry = geometry
#  )

# Save as shapefile
#st_write(final_shapefile_clean, "Data/Polygons/final_shapefile_clean_saved.shp",
#         delete_dsn = TRUE)


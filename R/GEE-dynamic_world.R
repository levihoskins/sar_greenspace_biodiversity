# Load packages
library("ggplot2")
library("dplyr")
library("sf")
library("ggspatial")

# Read the shapefile and remove rows with NAs
final_shapefile_clean <- st_read("Data/Polygons/final_shapefile_clean_saved.shp")

# Rename columns because they decided to change names
final_shapefile_clean <- final_shapefile_clean %>%
  rename(
    COMMON = COMMON,
    SCIENTIFIC = SCIENTI,
    LATITUDE = LATITUD,
    LONGITUDE = LONGITU,
    COUNTY = COUNTY,
    STATE = STATE,
    LOCALITY = LOCALITY,
    L.ID = L_ID,
    L.TYPE = L_TYPE,
    DATE = DATE,
    O.COUNT = O_COUNT,
    OBSERV.ID = OBSERV_,
    SEI = SEI,
    MONTH = MONTH, 
    Shape_Area = Shap_Ar,
    Park_Sourc = Prk_Src,
    Park_Urban = Prk_Urb,
    Park_Place = Prk_Plc,
    Park_Count = Prk_Cnt,
    Park_Addre = Prk_Add,
    Park_Size_ = Prk_Sz_,
    Park_Siz_1 = Prk_S_1,
    Park_Size1 = Prk_Sz1,
    Park_Name = Park_Nm,
    area = area,
    lists = lists,
    geometry = geometry
  )

# Calculate via park, size, month
location_richness <- final_shapefile_clean %>%
  group_by(Park_Addre, MONTH) %>%
  mutate(species_richness = n_distinct(SCIENTIFIC)) %>%
  ungroup()

## Google Earth Engine - Dynamic World
d_world <- read.csv("Data/GEE/dworld.csv")

d_world %>% 
  count(Park_Addre) %>% 
  filter(n > 1)

d_world_clean <- d_world %>%
  group_by(Park_Addre) %>%
  summarise(across(everything(), ~ first(na.omit(.)), .names = "first_{.col}"))

# First, join while keeping all rows from location_richness
combined <- location_richness %>%
  left_join(d_world_clean, by = "Park_Addre", suffix = c("", ".y"))

# Keep only these columns
### NEED TO MODIFY
filtered_data <- combined %>%
  select(L.ID, lists, COMMON, SCIENTIFIC, LATITUDE, LONGITUDE, geometry, DATE, O.COUNT,
         OBSERV.ID, SEI, MONTH, Shape_Area, Park_Addre, Park_Size_, Park_Size1, Park_Siz_1,
         species_richness, first_system.index, first_GISTrkrID, first_ParkID, 
         first_.geo)


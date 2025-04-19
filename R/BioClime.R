# Load packages
library(dismo)
library(ggplot2)
library(dplyr)
library(readr)
library(raster)
library(sf)
library(sp)
library(geodata)

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

# Draw in Bioclimatic variables with 'biovars' function in dismo
bioclim_data <- geodata::worldclim_global(var = "bio", res = 10, path = "Data/BioClime")


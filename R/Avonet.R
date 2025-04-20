# Load Packages
library("ggplot2")
library("dplyr")
library("sf")
library("ggspatial")
library("ggpmisc")

# Read the shapefile and remove rows with NAs
final_shapefile_clean <- st_read("Data/Polygons/final_shapefile_clean_saved.shp")

# Rename columns because they decided to change names
final_shapefile_clean <- final_shapefile_clean %>%
  rename(
    COMMON = COMMON, SCIENTIFIC = SCIENTI, LATITUDE = LATITUD, LONGITUDE = LONGITU,
    COUNTY = COUNTY, STATE = STATE, LOCALITY = LOCALITY, L.ID = L_ID, L.TYPE = L_TYPE,
    DATE = DATE, O.COUNT = O_COUNT, OBSERV.ID = OBSERV_, SEI = SEI, MONTH = MONTH, 
    Shape_Area = Shap_Ar, Park_Sourc = Prk_Src, Park_Urban = Prk_Urb, Park_Place = Prk_Plc,
    Park_Count = Prk_Cnt, Park_Addre = Prk_Add, Park_Size_ = Prk_Sz_, Park_Siz_1 = Prk_S_1,
    Park_Size1 = Prk_Sz1, Park_Name = Park_Nm, area = area, lists = lists, geometry = geometry
  )

# Calculate via park, size, month
location_richness <- final_shapefile_clean %>%
  group_by(Park_Addre, MONTH) %>%
  mutate(species_richness = n_distinct(SCIENTIFIC)) %>%
  ungroup()

## Read AVONET
avonet <- read_csv("Data/AVONET/AVONET1_BirdLife.csv")

# Join the avonet data to the shapefile by species name
joined_data <- location_richness %>%
  left_join(avonet, by = c("SCIENTIFIC" = "Species1"))

# Drop columns that are NAs or unnecessary
joined_data <- joined_data %>%
  select(-Reference.species, -Traits.inferred, -Mass.Refs.Other, -Inference,
         -Female, -Male, -Unknown, -Total.individuals)

# If joined_data is an sf object
sum(!complete.cases(st_drop_geometry(joined_data)))

# Drop rows with NAs
joined_data_clean <- joined_data %>%
  drop_na()

## Number of species
length(unique(joined_data_clean$SCIENTIFIC))

# Calculate via park, size, month
joined_data_clean <- joined_data_clean %>%
  group_by(Park_Addre, MONTH) %>%
  mutate(species_richness = n_distinct(SCIENTIFIC)) %>%
  ungroup()

# Save as shapefile
st_write(joined_data_clean, "Data/AVONET/final_avonet.shp",
         delete_dsn = TRUE)



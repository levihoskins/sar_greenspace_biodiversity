# Load Packages
library("tidyverse")
library("sf")

# Read file
final_shapefile_clean <- readRDS("Data/Intermediate_Data/final_shapefile_clean.RDS")

# Calculate via greenspace and month
location_richness <- final_shapefile_clean %>%
  group_by(Park_Addre, MONTH) %>%
  mutate(species_richness = n_distinct(SCIENTIFIC.NAME)) %>%
  ungroup()

## Read AVONET
avonet <- read_csv("Data/AVONET/AVONET1_BirdLife.csv")

# Join the AVONET data to the shapefile by species name
joined_data <- location_richness %>%
  left_join(avonet, by = c("SCIENTIFIC.NAME" = "Species1"))

# Drop columns that are NAs or unnecessary
joined_data <- joined_data %>%
  dplyr::select(COMMON.NAME, SCIENTIFIC.NAME, LATITUDE, LONGITUDE, COUNTY, LOCALITY, LOCALITY.ID,
                OBSERVATION.DATE, OBSERVATION.COUNT, OBSERVER.ID, SAMPLING.EVENT.IDENTIFIER, MONTH,
                Shape_Area, Park_Addre, Park_Size_, Park_Siz_1, Park_Name, area,
                lists, geometry, Migration, species_richness, Season)


## Number of species and greenspaces
length(unique(joined_data$SCIENTIFIC.NAME)) #466
length(unique(joined_data$Park_Addre)) #127

# Calculate via greenspace and month
joined_data_clean <- joined_data %>%
  group_by(Park_Addre, MONTH) %>%
  mutate(species_richness = n_distinct(SCIENTIFIC.NAME)) %>%
  ungroup()

head(joined_data_clean)

# Save as shapefile
saveRDS(joined_data_clean, "Data/AVONET/final_avonet.RDS")

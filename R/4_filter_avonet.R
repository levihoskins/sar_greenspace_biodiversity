# this script will read in XXX
# and then read in avonet
# combine the two
# filter and summarize to analysis ready data

# Load Packages
library("tidyverse")
library("sf")

# Read file
final_shapefile_clean <- readRDS("Data/Intermediate_Data/final_shapefile_clean.RDS")

## Read AVONET
avonet <- read_csv("Data/AVONET/AVONET1_BirdLife.csv")

# Calculate via greenspace and month
total_richness_season <- final_shapefile_clean %>%
  st_set_geometry(NULL) %>%
  left_join(avonet, by = c("SCIENTIFIC.NAME" = "Species1")) %>%
  dplyr::select(COMMON.NAME, SCIENTIFIC.NAME, LATITUDE, LONGITUDE, COUNTY, LOCALITY, LOCALITY.ID,
                OBSERVATION.DATE, OBSERVATION.COUNT, OBSERVER.ID, SAMPLING.EVENT.IDENTIFIER, MONTH,
                lists, Migration, Season, Park_Addre) %>%
  mutate(Season = case_when(
    MONTH %in% c("Dec", "Jan", "Feb") ~ "Overwintering",
    MONTH %in% c("Mar", "Apr", "May") ~ "Spring Migration",
    MONTH %in% c("Jun", "Jul", "Aug") ~ "Breeding",
    MONTH %in% c("Sep", "Oct", "Nov") ~ "Fall Migration")) %>%
  group_by(Park_Addre, Season) %>%
  summarize(species_richness = n_distinct(SCIENTIFIC.NAME),
            number_of_checklists = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>%
  ungroup()

# same thing but stratified by migration status
total_richness_migratory_status_season <- final_shapefile_clean %>%
  st_set_geometry(NULL) %>%
  left_join(avonet, by = c("SCIENTIFIC.NAME" = "Species1")) %>%
  dplyr::select(COMMON.NAME, SCIENTIFIC.NAME, LATITUDE, LONGITUDE, COUNTY, LOCALITY, LOCALITY.ID,
                OBSERVATION.DATE, OBSERVATION.COUNT, OBSERVER.ID, SAMPLING.EVENT.IDENTIFIER, MONTH,
                lists, Migration, Season, Park_Addre) %>%
  mutate(Season = case_when(
    MONTH %in% c("Dec", "Jan", "Feb") ~ "Overwintering",
    MONTH %in% c("Mar", "Apr", "May") ~ "Spring Migration",
    MONTH %in% c("Jun", "Jul", "Aug") ~ "Breeding",
    MONTH %in% c("Sep", "Oct", "Nov") ~ "Fall Migration")) %>%
  mutate(
    migration_status = case_when(
      Migration == 1 ~ "residential",
      Migration %in% c(2, 3) ~ "migratory",
      TRUE ~ NA_character_
    )) %>%
  group_by(Park_Addre, migration_status, Season) %>%
  summarize(species_richness = n_distinct(SCIENTIFIC.NAME),
            number_of_checklists = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>%
  ungroup()

# get just park-level data (unique to each park)
park_level_data <- final_shapefile_clean %>%
  st_set_geometry(NULL) %>%
  dplyr::select(Shape_Area, Park_Addre, Park_Size_, Park_Siz_1, area) %>%
  distinct()

# now we want to create one dataset that has all the data we'll need for analysis
# combine them all
final_data_for_analysis <- total_richness_season %>%
  mutate(analysis="total") %>%
  bind_rows(total_richness_migratory_status_season %>%
              rename(analysis=migration_status)) %>%
  left_join(., park_level_data, by="Park_Addre")



# Save as shapefile
saveRDS(final_data_for_analysis, "Data/AVONET/final_data_for_analysis.RDS")

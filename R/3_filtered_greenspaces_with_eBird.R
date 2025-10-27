# this script will read in florida shapefiles, clean them up
## next read in eBird data and overlay the two
## from there we will get all_points

### and then read in avonet
### combine the two
#### filter and summarize to analysis ready data

# Load packages
library("tidyverse")
library("sf")

# Read in shapefiles + add attributes
fl_shapefile <-st_read("Data/Polygons/filtered_shapefile.shp")
fl_shapefile <-st_cast(fl_shapefile, "POLYGON")

fl_shapefile$geometry <- st_geometry(fl_shapefile)
fl_shapefile$area <- st_area(fl_shapefile$geometry)

# Filter based on Park_Size_ for determining greenspaces that are okay (m^2)
filtered_shapefile <- fl_shapefile %>%
  dplyr::filter(Park_Siz_1 >=5) %>%
  dplyr::filter(Park_Siz_1 <= 1500000)

length(unique(filtered_shapefile$Park_Addre))
length(unique(fl_shapefile$Park_Addre))

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

### Read in RDS for the ### out script
all_points <- readRDS("Data/eBird/all_points.RDS")

## Replacing empty Park_Addre with Park_Name as to get rid of NAs
all_points <- all_points %>%
  mutate(Park_Addre = if_else(Park_Addre == "" | is.na(Park_Addre), 
                              Park_Name, 
                              Park_Addre)) %>%
  filter(!grepl("sp\\.", SCIENTIFIC.NAME, ignore.case = TRUE) &
           !grepl(" x ", SCIENTIFIC.NAME, ignore.case = TRUE) &
           !grepl("/", SCIENTIFIC.NAME))
length(unique(all_points$Park_Addre))

#############################################
### Greenspaces with 10 checklists per season
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

## Now filter by 10 checklists per season
valid_ids_season <- valid_ids_season %>%
  st_drop_geometry() %>%
  group_by(Park_Addre, Season) %>%
  summarise(lists = n_distinct(SAMPLING.EVENT.IDENTIFIER), .groups = "drop_last") %>%
  group_by(Park_Addre) %>%
  filter(all(lists >= 10)) 

# Keep all data for qualifying LOCALITY.IDs
park_counts <- all_points %>%
  filter(Park_Addre %in% valid_ids_season$Park_Addre) %>%
  left_join(valid_ids_season %>% dplyr::select(Park_Addre, Season, lists),
            by = "Park_Addre") %>%
  group_by(Park_Addre) %>%
  ungroup()

length(unique(park_counts$Park_Addre)) #89

# Get unique species
unique_species <- unique(park_counts[, c("SCIENTIFIC.NAME", "COMMON.NAME")])

# Keep only one row per species
unique_species <- unique_species[!duplicated(unique_species$SCIENTIFIC.NAME), ]

# Sort by scientific name
unique_species <- unique_species[order(unique_species$SCIENTIFIC.NAME), ]
unique_species
# Remove geometry
unique_species_null <- st_set_geometry(unique_species, NULL)

# Export table
species_table <- as.data.frame(unique_species_null)
write.csv(species_table, "Figures/unique_species.csv", row.names = FALSE)

########
### Maybe remake this with migratory status, but that makes things a little difficult.

### see species richness per park
species_richness_df <- all_points %>%
  filter(Park_Addre %in% valid_ids_season$Park_Addre) %>%
  group_by(Park_Addre) %>%
  summarize(species_richness = n_distinct(SCIENTIFIC.NAME)) %>%
  arrange(desc(species_richness))

mean(species_richness_df$species_richness)
sd(species_richness_df$species_richness)

# Restore geometry (if lost)
final_shapefile_clean <- st_as_sf(park_counts, 
                                  geometry = st_geometry(all_points), 
                                  crs = st_crs(all_points))

# Save as RDS
saveRDS(final_shapefile_clean, "Data/Intermediate_Data/final_shapefile_clean.RDS")

#####################################
#### Now read in AVONET and combine scripts for migration_status
### create script ready data frame
#####################################

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

# list of the 21 species with their migratory status that are not included with AVONOT
## Adding in their migratory status with All About Birds distribution
manual_status <- tibble::tibble(
  SCIENTIFIC.NAME = c(
    "Ardea ibis", 
    "Leucophaeus atricilla",
    "Nannopterum auritum",
    "Astur cooperii",
    "Chroicocephalus philadelphia",
    "Himantopus mexicanus",
    "Thectocercus acuticaudatus",
    "Anser cygnoides",
    "Butorides virescens",
    "Dryocopus pileatus",
    "Daptrius chimachima",
    "Corthylio calendula",
    "Porphyrio martinica",
    "Anarhynchus wilsonia",
    "Botaurus exilis",
    "Nannopterum brasilianum",
    "Porphyrio poliocephalus",
    "Tyto furcata",
    "Leucophaeus pipixcan",
    "Psittacula krameri",
    "Icterus bullockii"
  ),
  migration_status = c(
    "residential",  # Cattle Egret
    "residential",  # Laughing Gull
    "residential",  # Double-crested Cormorant
    "migratory",    # Cooper’s Hawk
    "migratory",    # Bonaparte’s Gull
    "migratory",    # Black-necked Stilt
    "residential",  # Blue-crowned Parakeet (exotic)
    "residential",  # Swan Goose (domestic/exotic)
    "residential",  # Green Heron
    "residential",  # Pileated Woodpecker
    "migratory",    # Yellow-headed Caracara
    "migratory",    # Ruby-crowned Kinglet
    "residential",  # Purple Gallinule
    "migratory",    # Wilson’s Plover
    "migratory",    # Least Bittern
    "residential",  # Neotropic Cormorant
    "residential",  # Grey-headed Swamphen (exotic established in FL)
    "residential",  # Barn Owl
    "migratory",    # Franklin’s Gull
    "residential",  # Rose-ringed Parakeet (exotic)
    "migratory"     # Bullock’s Oriole (accidental)
  )
)

# same thing but stratified by migration status
total_richness_migratory_status_season <- final_shapefile_clean %>%
  st_set_geometry(NULL) %>%
  left_join(avonet, by = c("SCIENTIFIC.NAME" = "Species1")) %>%
  dplyr::select(
    COMMON.NAME, SCIENTIFIC.NAME, LATITUDE, LONGITUDE, COUNTY, LOCALITY, LOCALITY.ID,
    OBSERVATION.DATE, OBSERVATION.COUNT, OBSERVER.ID, SAMPLING.EVENT.IDENTIFIER, MONTH,
    lists, Migration, Season, Park_Addre
  ) %>%
  mutate(Season = case_when(
    MONTH %in% c("Dec", "Jan", "Feb") ~ "Overwintering",
    MONTH %in% c("Mar", "Apr", "May") ~ "Spring Migration",
    MONTH %in% c("Jun", "Jul", "Aug") ~ "Breeding",
    MONTH %in% c("Sep", "Oct", "Nov") ~ "Fall Migration"
  )) %>%
  mutate(
    migration_status = case_when(
      Migration == 1 ~ "residential",
      Migration %in% c(2, 3) ~ "migratory",
      TRUE ~ NA_character_
    )
  ) %>%
  left_join(manual_status, by = "SCIENTIFIC.NAME") %>%
  mutate(migration_status = coalesce(migration_status.y, migration_status.x)) %>%
  select(-migration_status.x, -migration_status.y) %>%
  group_by(Park_Addre, migration_status, Season) %>%
  summarize(
    species_richness = n_distinct(SCIENTIFIC.NAME),
    number_of_checklists = n_distinct(SAMPLING.EVENT.IDENTIFIER),
    .groups = "drop"
  )

# get just park-level data (unique to each park)
park_level_data <- final_shapefile_clean %>%
  st_set_geometry(NULL) %>%
  dplyr::select(Shape_Area, Park_Addre, Park_Siz_1, area) %>%
  distinct() %>%
  group_by(Park_Addre) %>%
  arrange(desc(area)) %>%
  slice(1) %>%
  ungroup()

# now we want to create one dataset that has all the data we'll need for analysis
# combine them all
final_data_for_analysis <- total_richness_season %>%
  mutate(analysis="total") %>%
  bind_rows(total_richness_migratory_status_season %>%
              rename(analysis=migration_status)) %>%
  left_join(., park_level_data, by="Park_Addre")

# Save as shapefile
saveRDS(final_data_for_analysis, "Data/AVONET/final_data_for_analysis.RDS")

## Now calculate richness by migration status
# Combine AVONET and manual species, keep scientific names
total_richness_migratory_status_scientific_name <- final_shapefile_clean %>%
  st_set_geometry(NULL) %>%
  left_join(avonet, by = c("SCIENTIFIC.NAME" = "Species1")) %>%
  dplyr::select(SCIENTIFIC.NAME, Migration) %>%
  mutate(
    migration_status = case_when(
      Migration == 1 ~ "residential",
      Migration %in% c(2, 3) ~ "migratory",
      TRUE ~ NA_character_
    )
  ) %>%
  left_join(manual_status, by = "SCIENTIFIC.NAME") %>%
  mutate(
    migration_status = coalesce(migration_status.y, migration_status.x)
  ) %>%
  select(SCIENTIFIC.NAME, migration_status) %>%
  distinct()

# Sum total unique species by migratory status
total_species_by_status <- total_richness_migratory_status_scientific_name %>%
  group_by(migration_status) %>%
  summarize(total_species = n_distinct(SCIENTIFIC.NAME), .groups = "drop")

total_species_by_status






### Ignore code below - it is for different ways to filter the data
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
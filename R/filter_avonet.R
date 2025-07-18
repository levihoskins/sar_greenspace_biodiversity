# Load Packages
library("dplyr")
library("sf")
library("readr")

# Read the shapefile and remove rows with NAs
final_shapefile_clean <- st_read("Data/Polygons/final_shapefile_clean_saved.shp")

# Rename columns
final_shapefile_clean <- final_shapefile_clean %>%
  rename(
    COMMON = COMMON, SCIENTIFIC = SCIENTI, LATITUDE = LATITUD, LONGITUDE = LONGITU,
    COUNTY = COUNTY, STATE = STATE, LOCALITY = LOCALIT, L.ID = L_ID, L.TYPE = L_TYPE,
    DATE = DATE, O.COUNT = O_COUNT, OBSERV.ID = OBSERV_, SEI = SEI, MONTH = MONTH, Season = Season,
    Shape_Area = Shap_Ar, Park_Sourc = Prk_Src, Park_Urban = Prk_Urb, Park_Place = Prk_Plc,
    Park_Count = Prk_Cnt, Park_Addre = Prk_Add, Park_Size_ = Prk_Sz_, Park_Siz_1 = Prk_S_1,
    Park_Size1 = Prk_Sz1, Park_Name = Park_Nm, area = area, lists = lists, geometry = geometry
  )

# Calculate via greenspace and month
location_richness <- final_shapefile_clean %>%
  group_by(Park_Addre, MONTH) %>%
  mutate(species_richness = n_distinct(SCIENTIFIC)) %>%
  ungroup()

## Read AVONET
avonet <- read_csv("Data/AVONET/AVONET1_BirdLife.csv")

# Join the AVONET data to the shapefile by species name
joined_data <- location_richness %>%
  left_join(avonet, by = c("SCIENTIFIC" = "Species1"))

# Drop columns that are NAs or unnecessary
joined_data <- joined_data %>%
  dplyr::select(COMMON, SCIENTIFIC, LATITUDE, LONGITUDE, COUNTY, LOCALITY, L.ID, L.TYPE,
                DATE, O.COUNT, O.COUNT, OBSERV.ID, SEI, DURATIO, EFFORT_D, EFFORT_A, MONTH,
                Shape_Area, Park_Addre, Park_Size_, Park_Siz_1, Park_Size1, Park_Name, area,
                lists, geometry, Migration, species_richness, Season)

# Specify columns to ignore NAs in
ignore_cols <- c("EFFORT_D", "EFFORT_A", "DURATIO")

joined_data_clean <- joined_data %>%
  filter(
    rowSums(is.na(dplyr::select(., -all_of(ignore_cols)))) == 0
  )

## Number of species and greenspaces
length(unique(joined_data_clean$SCIENTIFIC)) #344
length(unique(joined_data_clean$Park_Addre)) #54

# Calculate via greenspace and month
joined_data_clean <- joined_data_clean %>%
  group_by(Park_Addre, MONTH) %>%
  mutate(species_richness = n_distinct(SCIENTIFIC)) %>%
  ungroup()

# Save as shapefile
st_write(joined_data_clean, "Data/AVONET/final_avonet.shp",
         delete_dsn = TRUE)

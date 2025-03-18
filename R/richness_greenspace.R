# Read in packages
library("ggplot2")
library("dplyr")
library("readr")
library("tidyr")
library("vegan")
library("lubridate")
library("data.table")
library("sf")

# Read in final_data_df
final_data_df <- read.csv("Data/eBird/final_data.csv")

# Read the shapefile
final_shapefile <- st_read("Data/Polygons/final_shapefile.shp")

### NEED TO GET RID OF NAs ###

## Number of unique checklists
length(unique(final_shapefile$L_ID)) #6353

# Calculate richness per greenspace per locality.id per month
location_richness <- final_shapefile %>%
  group_by(Park_Addre, L_ID, MONTH) %>%
  summarise(species_richness = n_distinct(COMMON), .groups = 'drop') 

write.csv(location_richness, "Data/location_richness.csv")

# Bar plot of species richness per locality within each park, per month
ggplot(location_richness, aes(x = MONTH, y = species_richness, fill = Park_Addre)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ L_ID) + 
  theme_minimal() +
  labs(x = "Month", y = "Species Richness", title = "Species Richness per Locality by Month") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Box plot
ggplot(location_richness, aes(x = MONTH, y = species_richness, fill = Park_Addre)) +
  geom_boxplot() +
  facet_wrap(~ Park_Addre) + 
  theme_minimal() +
  labs(x = "Month", y = "Species Richness", title = "Distribution of Species Richness per Month by Park")

# Calculate species richness per area and per month
location_richness_area_monthly <- final_data %>%
  group_by(Park_Name, LOCALITY.ID, Park_Size_, MONTH) %>%
  summarise(species_richness = n_distinct(COMMON.NAME), .groups = 'drop') %>%
  mutate(species_richness_per_area = species_richness / Park_Size_)

# Bar plot of species richness per area for each month
ggplot(location_richness_area_monthly, aes(x = MONTH, y = species_richness_per_area, fill = Park_Name)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ LOCALITY.ID) +  # Create a separate plot for each locality
  theme_minimal() +
  labs(x = "Month", y = "Species Richness per Area", title = "Species Richness per Area by Month for Each Locality") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

# Box plot of species richness per area by month, grouped by Park_Name
ggplot(location_richness_area_monthly, aes(x = MONTH, y = species_richness_per_area, fill = Park_Name)) +
  geom_boxplot() +
  facet_wrap(~ Park_Name) +  # Create a separate box plot for each park
  theme_minimal() +
  labs(x = "Month", y = "Species Richness per Area", title = "Distribution of Species Richness per Area by Month and Park")

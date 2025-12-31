# creating figure 1 with points on map for each remaining greenspace

# Load packages
library(tidyverse)
library(sf)
library(tigris)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis)

# Read files
final_data_for_analysis <- readRDS("Data/AVONET/final_data_for_analysis.RDS")
final_shapefile_clean <- readRDS("Data/Intermediate_Data/final_shapefile_clean.RDS")

final_shapefile_clean <- final_shapefile_clean %>%
  dplyr::filter(!Park_Addre %in% c(
    "Enchanted Forest / Arch Creek Park",
    "Jupiter Inlet Outstanding Natural Area",
    "Seacrest Scrub Preserve"
  ))

## quick data exploration for ### checklists and ### observations
# make sure observation count is numeric
final_shapefile_clean <- final_shapefile_clean %>%
  mutate(OBSERVATION.COUNT = as.numeric(OBSERVATION.COUNT))

# sum OBSERVATION.COUNT per unique checklist (LOCALITY.ID)
checklist_sums <- final_shapefile_clean %>%
  group_by(LOCALITY.ID) %>%
  summarise(total_observations = sum(OBSERVATION.COUNT, na.rm = TRUE))

# number of unique checklists
num_checklists <- nrow(checklist_sums)

# total number of observations across all checklists
total_observations <- sum(checklist_sums$total_observations)

### resume code for figure 1

# reorder season and analysis for figures
final_data_for_analysis$analysis <- factor(
  final_data_for_analysis$analysis,
  levels = c("residential", "migratory", "total")
)

final_data_for_analysis$Season <- factor(
  final_data_for_analysis$Season,
  levels = c("Overwintering", "Spring Migration", "Breeding", "Fall Migration"),
  ordered = TRUE
)

## Clean up shapefile so that i can combine geometry back into final data frame
final_shapefile_clean <- final_shapefile_clean %>%
  dplyr::select(Park_Addre, SCIENTIFIC.NAME, SAMPLING.EVENT.IDENTIFIER, geometry) %>%
  group_by(Park_Addre) %>%
  summarise(species_richness = n_distinct(SCIENTIFIC.NAME),
            number_of_checklists = n_distinct(SAMPLING.EVENT.IDENTIFIER),
            geometry = st_union(geometry), .groups = "drop")

final_data_fig1 <- final_shapefile_clean %>%
  filter(Park_Addre %in% final_data_for_analysis$Park_Addre)

final_data_fig1 <- final_data_fig1 %>%
  drop_na()

###################################
### CODE FOR FIGURE 1 -- Study Area
###################################
# Begin graph with Florida underlay
world <- ne_countries(scale = "medium", returnclass = "sf")
usa <- ne_states(country = "united states of america", returnclass = "sf")
states <- usa %>% filter(name == "Florida")
fl_counties <- counties(state = "FL", cb = TRUE, year = 2022, class = "sf")

# Filter to South Florida counties
south_florida_counties <- fl_counties %>%
  filter(NAME %in% c("Broward", "Miami-Dade", "Palm Beach"))

final_data_fig1 <- final_data_fig1 %>%
  st_centroid(of_largest_polygon = TRUE) %>%
  cbind(st_coordinates(.))

# ggplot of study area with species richness, number of checklists per greenspace, and number of greenspaces
study_area <- ggplot() +
  geom_sf(data = south_florida_counties, fill = "white", color = "black") +
  scale_size_continuous(name = "Checklists", range = c(2, 7)) +
  geom_sf(
    data = final_data_fig1,
    aes(size = number_of_checklists, fill = species_richness),
    shape = 21, color = "black"
  ) +
  scale_fill_gradient(
    name = "Species Richness",
    low = "white",
    high = "#00441b",
    limits = range(final_data_fig1$species_richness, na.rm = TRUE),
    trans = "log",
    guide = guide_colorbar(
      label = TRUE,
      title = "Species Richness"
    )
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

study_area

# save with transparent background
ggsave('Figures/Study_Area_Figure_1.png', bg = 'transparent', plot = study_area)

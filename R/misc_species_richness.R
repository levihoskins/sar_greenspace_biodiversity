# Load packages
library(sf)
library(ggpmisc)
library(tidyverse)

##########################################
## DO NOT FORGET PARK_SIZE_ is in HECTARES
## Park_Siz_1 is in m^2, same as area
##########################################

# Read the files
park_counts <- readRDS("Data/Intermediate_Data/park_counts.rds")
final_data_for_analysis <- readRDS("Data/AVONET/final_data_for_analysis.RDS")
avonet <- read_csv("Data/AVONET/AVONET1_BirdLife.csv")

### remove the three parks with NAs for GHMI and Isolation
final_data_for_analysis <- final_data_for_analysis %>% 
  drop_na()

# Get unique species
unique_species <- unique(park_counts[, c("SCIENTIFIC.NAME", "COMMON.NAME")])
# Keep only one row per species
unique_species <- unique_species[!duplicated(unique_species$SCIENTIFIC.NAME), ]
# Sort by scientific name
unique_species <- unique_species[order(unique_species$SCIENTIFIC.NAME), ]
# Remove geometry
unique_species_null <- st_set_geometry(unique_species, NULL)

## Now combine with AVONET data to add migratory status to each
### But first add in the manual status for the 21 species
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

# Calculate via greenspace and month
species_status <- unique_species_null %>%
  left_join(avonet, by = c("SCIENTIFIC.NAME" = "Species1")) %>%
  dplyr::select(SCIENTIFIC.NAME, COMMON.NAME, Migration) %>%
  mutate(
    migration_status = case_when(
      Migration == 1 ~ "residential",
      Migration %in% c(2, 3) ~ "migratory",
      TRUE ~ NA_character_
    )
  ) %>%
  left_join(manual_status, by = "SCIENTIFIC.NAME") %>%
  mutate(migration_status = coalesce(migration_status.y, migration_status.x)) %>%
  dplyr::select(-migration_status.x, -migration_status.y) %>%
  group_by(SCIENTIFIC.NAME, COMMON.NAME, migration_status) %>%
  dplyr::select(-Migration) %>%
  arrange(migration_status)

# Export table
species_table <- as.data.frame(species_status)
write.csv(species_table, "Figures/Supplementary/unique_species_status.csv", row.names = FALSE)

## Species Richness
# Calculate species richness per park
species_richness_per_park <- park_counts %>%
  group_by(Park_Addre) %>%
  summarise(species_richness = n_distinct(SCIENTIFIC.NAME)) %>%
  arrange(desc(species_richness))

mean(species_richness_per_park$species_richness)
sd(species_richness_per_park$species_richness)

######################################
### Preliminary Graphs to explore data
######################################

## Test for correlations between richness and checklists
hist(final_data_for_analysis$species_richness)
hist(final_data_for_analysis$number_of_checklists)
hist(sqrt(final_data_for_analysis$number_of_checklists))

cor.test(final_data_for_analysis$species_richness, sqrt(final_data_for_analysis$number_of_checklists))

## Mean, SD, Range of Species Richness## Mean, SD,joined_data_clean Range of Species Richness
final_data_for_analysis$species_richness <- as.numeric(final_data_for_analysis$species_richness)

##############################################
# Look at Species Richness per park on average
##############################################

## Take the Standard Deviation per Park for species richness (for error bars)
sd_per_park <- final_data_for_analysis %>%
  group_by(Park_Addre) %>%
  summarise(species_richness_sd = sd(species_richness, na.rm = TRUE))

## Add back to original
final_data_for_analysis <- final_data_for_analysis %>%
  left_join(sd_per_park, by = "Park_Addre")

### Summmarize to one row per park (average them)
park_summary <- final_data_for_analysis %>%
  group_by(Park_Addre) %>%
  summarise(
    mean_richness = mean(species_richness, na.rm = TRUE),
    sd_richness = sd(species_richness, na.rm = TRUE)
  )

#### Plot the summarized data
ggplot(park_summary, aes(x = reorder(Park_Addre, mean_richness), y = mean_richness)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_richness - sd_richness, ymax = mean_richness + sd_richness)) +
  theme_bw() +
  ylab("Mean species richness") +
  xlab("Greenspace") +
  theme(axis.text.x = element_text(size = 5, color = "black")) +
  theme(axis.text.y = element_text(size = 5, color = "black")) +
  theme(axis.title.x = element_text(size = 12)) +
  theme(axis.title.y = element_text(size = 12)) +
  theme(panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank()) +
  theme(panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank()) +
  coord_flip()
ggsave('Figures/supplementary/Summarized_Mean_Species_Richness_Park_error_bars.png', bg = "transparent", 
       height = 6, width = 7)

# Load packages
library("ggplot2")
library("dplyr")
library("sf")
library("ggpmisc")

##########################################
## DO NOT FORGET PARK_SIZE_ is in HECTARES
## Park_Siz_1 is in m^2, same as area
##########################################

# Read the RDS
park_counts <- readRDS("Data/Intermediate_Data/park_counts.rds")
final_data_for_analysis <- readRDS("Data/AVONET/final_data_for_analysis.RDS")

# Get unique species
unique_species <- unique(park_counts[, c("SCIENTIFIC.NAME", "COMMON.NAME")])
# Keep only one row per species
unique_species <- unique_species[!duplicated(unique_species$SCIENTIFIC.NAME), ]
# Sort by scientific name
unique_species <- unique_species[order(unique_species$SCIENTIFIC.NAME), ]
# Remove geometry
unique_species_null <- st_set_geometry(unique_species, NULL)

# Export table
species_table <- as.data.frame(unique_species_null)
write.csv(species_table, "Figures/unique_species.csv", row.names = FALSE)

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
ggsave('Figures/supplementary_unused/Summarized_Mean_Species_Richness_Park_error_bars.png', bg = "transparent", 
       height = 6, width = 7)

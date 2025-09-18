# Load packages
library("ggplot2")
library("tigris")
library("dplyr")
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library("glmmTMB")
library("broom")

# Read files
final_data_for_analysis <- readRDS("Data/AVONET/final_data_for_analysis.RDS")
final_shapefile_clean <- readRDS("Data/Intermediate_Data/final_shapefile_clean.RDS")

## Clean up shapefile so that i can combine geometry back into final data frame
final_shapefile_clean <- final_shapefile_clean %>%
  dplyr::select(Park_Addre, geometry) %>%
  group_by(Park_Addre) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

final_data_with_geometry <- final_data_for_analysis %>%
  left_join(final_shapefile_clean, by = "Park_Addre") %>%
  st_as_sf()

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

# ggplot of study area with species richness, number of checklists per greenspace, and number of greenspaces
study_area <- ggplot() +
  geom_sf(data = south_florida_counties, fill = "white", color = "black") +
  geom_sf(data = final_data_with_geometry, aes(size = number_of_checklists, fill = species_richness),
          shape = 21, color = "black", alpha = 0.8) +
  scale_fill_gradient(
    name = "Species Richness",
    low = "#e5f5e0",  
    high = "#006d2c"
  ) +
  scale_size_continuous(name = "Checklists", range = c(2, 8)) +
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

####################################
### RESUME Residential vs. Migratory
####################################

# GLMM with analysis as a fixed effect
glmm_analysis <- glmmTMB(
  species_richness ~ log10(area) + analysis,
  data = final_data_with_geometry,
  family = nbinom2
)

summary(glmm_analysis)

# Add predicted richness to the dataframe
final_data_with_geometry <- final_data_with_geometry %>%
  mutate(predicted_richness = predict(glmm_analysis, newdata = ., type = "response"))

# plot predicted richness by analysis
nb_richness_plot_status <- ggplot(final_data_with_geometry, aes(x = Park_Size_, y = predicted_richness, color = analysis)) +
  geom_point(alpha = 0.5) +
  scale_x_log10() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Park Area (hectares)", y = "Predicted Species Richness", color = "Analysis") +
  theme_minimal()
nb_richness_plot_status

ggsave("Figures/nb_richness_plot_status.png", plot = nb_richness_plot_status, bg = "transparent")

# fit linear models separately by analysis group
slopes <- final_data_with_geometry %>%
  group_by(analysis) %>%
  do(tidy(lm(predicted_richness ~ log10(Park_Size_), data = .))) %>%
  filter(term == "log10(Park_Size_)") %>%
  select(analysis, estimate, std.error, p.value)
slopes



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

# Reorder season and analysis for figures
final_data_with_geometry$analysis <- factor(
  final_data_with_geometry$analysis,
  levels = c("residential", "migratory", "total")
)

final_data_with_geometry$Season <- factor(
  final_data_with_geometry$Season,
  levels = c("Overwintering", "Spring Migration", "Breeding", "Fall Migration"),
  ordered = TRUE
)

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

final_data_points <- final_data_with_geometry %>%
  st_centroid(of_largest_polygon = TRUE) %>%
  cbind(st_coordinates(.))

# ggplot of study area with species richness, number of checklists per greenspace, and number of greenspaces
study_area <- ggplot() +
  geom_sf(data = south_florida_counties, fill = "white", color = "black") +
  geom_sf(
    data = final_data_points,
    aes(size = number_of_checklists, fill = species_richness),
    shape = 21, color = "black", alpha = 0.7
  ) +
  scale_fill_gradient(
    name = "Species Richness",
    low = "white",
    high = "#00441b",
    limits = range(final_data_points$species_richness, na.rm = TRUE)
  ) +
  scale_size_continuous(name = "Checklists", range = c(2, 7)) +
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
  species_richness ~ log10(Shape_Area) + analysis,
  data = final_data_with_geometry,
  family = nbinom2
)

summary(glmm_analysis)

# Add predicted richness to the dataframe
final_data_with_geometry <- final_data_with_geometry %>%
  mutate(predicted_richness = predict(glmm_analysis, newdata = ., type = "response"))

# plot predicted richness by analysis
nb_richness_plot_status <- ggplot(final_data_with_geometry, aes(x = Shape_Area / 10000, 
                                      y = predicted_richness, color = analysis)) +
  geom_point(alpha = 0.5) +
  scale_x_log10() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "Park Area (hectares)",   # updated axis label
    y = "Predicted Species Richness", 
    color = "Analysis"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(color = "black", linewidth = 1),
    legend.position = "bottom",
    legend.box.margin = margin(t = 5, b = 5),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )
nb_richness_plot_status

ggsave("Figures/nb_richness_plot_status.png", plot = nb_richness_plot_status, bg = "transparent")

# fit linear models separately by analysis group to show slope (estimate)
final_data_with_geometry %>%
  group_by(analysis) %>%
  do(tidy(lm(predicted_richness ~ log10(Park_Siz_1), data = .))) %>%
  filter(term == "log10(Park_Siz_1)")

##############################
## Add in season as a variable 
## and number_of_checklists
##############################
glmm_analysis_season <- glmmTMB(
  species_richness ~ log10(Shape_Area) + analysis * Season + log10(number_of_checklists),
  data = final_data_with_geometry,
  family = nbinom2
)

summary(glmm_analysis_season)

# Add predicted richness from the season model
final_data_with_geometry <- final_data_with_geometry %>%
  mutate(predicted_richness = predict(glmm_analysis_season, newdata = ., type = "response"))

# Plot predicted richness by analysis and season
nb_richness_plot_season <- ggplot(final_data_with_geometry,aes(x = Shape_Area / 10000, 
                                    y = predicted_richness, color = analysis)) +
  geom_point(alpha = 0.5) +
  scale_x_log10() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "Park Area (hectares)",
    y = "Predicted Species Richness",
    color = "Analysis"
  ) +
  facet_wrap(~Season) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(color = "black", linewidth = 1),
    legend.position = "bottom",
    legend.box.margin = margin(t = 5, b = 5),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

nb_richness_plot_season

ggsave("Figures/nb_richness_plot_season.png", plot = nb_richness_plot_season, bg = "transparent")



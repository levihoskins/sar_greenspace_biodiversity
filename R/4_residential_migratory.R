# Load packages
library("ggplot2")
library("tigris")
library("dplyr")
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library("glmmTMB")

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



##### START THIS NOW
####################################
### RESUME Residential vs. Migratory
####################################

# Residential GLMM
glmm_residential <- glmmTMB(
  residential ~ log10(area),
  data = final_data_with_geometry,
  family = nbinom2
)
summary(glmm_residential)

richness_per_park <- richness_per_park %>%
  mutate(
    predicted_residential = predict(glmm_residential, newdata = ., type = "response")
  )

NBGLMM_Park_Size_Residential <- ggplot(richness_per_park, aes(x = Park_Siz_1, y = predicted_residential)) +
  geom_point(alpha = 0.5, color = "black") +
  geom_line(aes(group = Park_Addre), color = "blue", alpha = 0.6) +
  labs(x = "Park Area (m²)", y = "Predicted Residential Richness", title = "Residential") +
  theme_minimal()

ggsave('Figures/NBGLMM_Park_Size_Residential.png', bg = 'transparent', plot = NBGLMM_Park_Size_Residential)

# Migratory GLMM
glmm_migratory <- glmmTMB(
  migratory ~ log10(area),
  data = richness_per_park,
  family = nbinom2
)
summary(glmm_migratory)

richness_per_park <- richness_per_park %>%
  mutate(
    predicted_migratory = predict(glmm_migratory, newdata = ., type = "response")
  )

NBGLMM_Park_Size_Migratory <- ggplot(richness_per_park, aes(x = Park_Siz_1, y = predicted_migratory)) +
  geom_point(alpha = 0.5, color = "black") +
  geom_line(aes(group = Park_Addre), color = "red", alpha = 0.6) +
  labs(x = "Park Area (m²)", y = "Predicted Migratory Richness", title = "Migratory") +
  theme_minimal()

ggsave('Figures/NBGLMM_Park_Size_Migratory.png', bg = 'transparent', plot = NBGLMM_Park_Size_Migratory)






##### Change below to match above
##### if above runs, delete below

###########################################
# MIGRATORY + RESIDENTIAL richness ~ Park size
###########################################
both_summary <- migratory_residential %>%
  group_by(Park_Name) %>%
  summarise(
    species_richness = mean(species_richness),
    Area_m2 = mean(area)
  )

# Fit NB model
nb_model_b <- glmmTMB(
  species_richness ~ log10(Area_m2),
  data = both_summary,
  family = nbinom2
)

summary(nb_model_b)
slope_nb_b <- summary(nb_model_b)$coefficients$cond["log10(Area_m2)", "Estimate"]
cat("Slope for migratory + resident richness:", slope_nb_b, "\n")

# Predictions for migratory + resident model
pred_both <- data.frame(
  Area_m2 = seq(min(both_summary$Area_m2),
                     max(both_summary$Area_m2),
                     length.out = 100)
)
pred_both$pred_richness <- predict(nb_model_b, newdata = pred_both, type = "response")

# Plot migratory + resident model
ggplot(both_summary, aes(x = Area_m2, y = species_richness)) +
  geom_point(color = "black") +
  geom_line(data = pred_both,
            aes(x = Area_m2, y = pred_richness),
            color = "#467010", size = 1) +
  scale_x_log10() +
  labs(x = "Park Size (ha, log scale)", y = "Species Richness") +
  theme_bw()

###########################################
# COMBINED analysis (migratory vs resident vs both)
###########################################

# Prepare migratory data
migratory_df <- migratory_data %>%
  group_by(Park_Name) %>%
  summarise(
    richness = mean(migratory_richness),
    Area_m2 = mean(area)
  ) %>%
  mutate(group = "Migratory")

# Prepare residential data
residential_df <- residential_data %>%
  group_by(Park_Name) %>%
  summarise(
    richness = mean(residential_richness),
    Area_m2 = mean(area)
  ) %>%
  mutate(group = "Resident")

# Prepare combined migratory + residential data
both_df <- migratory_residential %>%
  group_by(Park_Name) %>%
  summarise(
    richness = mean(species_richness),
    Area_m2 = mean(area)
  ) %>%
  mutate(group = "Migratory + Resident")

# Combine all into one dataframe
combined_df <- bind_rows(migratory_df, residential_df, both_df)

# Fit NB model with interaction
nb_model_combined <- glmmTMB(
  richness ~ log10(Area_m2) * group,
  data = combined_df,
  family = nbinom2
)

summary(nb_model_combined)

# Generate prediction grid for all groups
pred_grid <- expand.grid(
  Area_m2 = seq(min(combined_df$Area_m2),
                     max(combined_df$Area_m2),
                     length.out = 100),
  group = unique(combined_df$group)
)
pred_grid$pred_richness <- predict(nb_model_combined, newdata = pred_grid, type = "response")

#### Generate 95% Confidence Intervals 
# Generate prediction grid
pred_grid <- expand.grid(
  Area_m2 = seq(min(combined_df$Area_m2),
                     max(combined_df$Area_m2),
                     length.out = 100),
  group = unique(combined_df$group)
)

# Get predictions with SE for NB model
pred_nb <- predict(
  nb_model_combined,
  newdata = pred_grid,
  type = "link",      
  se.fit = TRUE
)

# Convert link scale -> response scale
pred_grid$fit <- exp(pred_nb$fit)
pred_grid$lwr <- exp(pred_nb$fit - 1.96 * pred_nb$se.fit)
pred_grid$upr <- exp(pred_nb$fit + 1.96 * pred_nb$se.fit)

# Plot with CI ribbon
NBGLMM_Park_Size_Status <- ggplot(combined_df, aes(x = Area_m2, y = richness, color = group)) +
  geom_point() +
  geom_ribbon(
    data = pred_grid,
    aes(
      x = Area_m2,
      ymin = lwr,
      ymax = upr,
      fill = group
    ),
    alpha = 0.3,
    inherit.aes = FALSE
  ) +
  geom_line(
    data = pred_grid,
    aes(x = Area_m2, y = fit, color = group),
    size = 1
  ) +
  scale_x_log10() +
  scale_color_manual(values = c(
    "Migratory" = "blue",
    "Resident" = "orange",
    "Migratory + Resident" = "darkgreen"
  )) +
  scale_fill_manual(values = c(
    "Migratory" = "lightblue",
    "Resident" = "yellow",
    "Migratory + Resident" = "lightgreen"
  )) +
  labs(
    x = "Park Size (m^2, log scale)",
    y = "Species Richness",
    color = "Group",
    fill = "Group"
  ) +
  facet_wrap(~ group, ncol = 1, scales = "free_y") +
  theme_bw() +
  theme(
    panel.background = element_blank(),
    plot.background = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none"
  )
NBGLMM_Park_Size_Status

# Save as PNG
ggsave('Figures/NBGLMM_Park_Size_Status.png', bg = 'transparent', plot = NBGLMM_Park_Size_Status)

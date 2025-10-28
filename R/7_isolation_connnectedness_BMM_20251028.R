# Load packages
library("sf")
library("dplyr")
library("ggplot2")
library("glmmTMB")
library("broom.mixed")
library("emmeans")
library("stats")
library("tidyr")
library("gt")
library("RColorBrewer")
library("stringr")
library("forcats")

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

## Read in GHMI and Dynamic World
ghmi <- read_csv("Data/GHMI_Dynamic_World/GHMI.csv")
dynamic_world <- read_csv("Data/GHMI_Dynamic_World/DynamicWorld.csv")

# Clean GHMI
ghmi_clean <- ghmi %>%
  group_by(Park_Addre) %>%
  summarise(ghmi_mean = mean(mean, na.rm = TRUE), .groups = "drop")

# Clean Dynamic World
# Keep only the most common dominant_class per park -- aggregates the data
dynamic_world_clean <- dynamic_world %>%
  group_by(Park_Addre, dominant_class) %>%
  tally() %>%
  slice_max(order_by = n, n = 1, with_ties = FALSE) %>%
  ungroup()

# Join with final_avonet
gee_final_data_for_analysis <- final_data_for_analysis %>%
  left_join(dynamic_world_clean, by = "Park_Addre") %>%
  left_join(ghmi_clean, by = "Park_Addre")

# Convert dominant_class to factor
gee_final_data_for_analysis$dominant_class <- as.factor(gee_final_data_for_analysis$dominant_class)

# Reorder season categories so that they appear correct when plotted
gee_final_data_for_analysis$Season <- factor(
  gee_final_data_for_analysis$Season,
  levels = c("Overwintering", "Spring Migration", "Breeding", "Fall Migration"),
  ordered = TRUE
)

gee_final_data_for_analysis$analysis <- factor(
  gee_final_data_for_analysis$analysis,
  levels = c("residential", "migratory", "total")
)

##############
# read in ParkServe data
# we probably want to consider the distance to nearest green space or natural area, not just greenspaces
# included in this study so we will use all the ParkServe polygons
#gs_poly <- st_read("Data/Polygons/ParkServe_Park_SouthFL.shp")

# group by Park_Addre and remove small parks (<0.05ha)
#gs_poly <- gs_poly %>%
#  group_by(Park_Addre) %>%
#  summarise(geometry = st_union(geometry), .groups = "drop") %>%
#  mutate(area=as.numeric(st_area(.)/10000)) %>%
#  filter(area > 0.05)

# Get nearest distance greenspace
#nearest_distance <- numeric(nrow(gs_poly))

#for (i in seq_len(nrow(gs_poly))) {
#  others <- gs_poly[-i, ]
#  distances <- as.numeric(st_distance(gs_poly[i, ], others))
#  nearest_distance[i] <- min(distances[distances > 0])
#}

# add distances to the dataframe
#gs_poly$nearest_dist_m <- nearest_distance
#gs_poly$nearest_dist_km <- nearest_distance / 1000

# make the gs_poly a data frame
#gs_poly_df <- gs_poly %>%
#  as.data.frame()

# save this file
#saveRDS(gs_poly, "Data/Intermediate_Data/distance_to_nearest_greenspace_parkserve.RDS")

gs_poly_df <- readRDS("Data/Intermediate_Data/distance_to_nearest_greenspace_parkserve.RDS")

# now add this to the greenspaces data frame
greenspaces <- gee_final_data_for_analysis %>%
  left_join(., gs_poly_df %>% dplyr::select(Park_Addre, nearest_dist_m, nearest_dist_km), by=c("Park_Addre"))
summary(greenspaces$nearest_dist_m)

greenspaces$dominant_class <- as.numeric(as.character(greenspaces$dominant_class))

#saveRDS(greenspaces, "Data/final_data_for_big_script.RDS")


# play with the data
ggplot(greenspaces, aes(x = log10(nearest_dist_m), y = species_richness, color = analysis)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, size = 1) +
  facet_wrap(~ Season) +
  labs(
    x = "Log10 Distance to Nearest Buffer (m + 1)",
    y = "Species Richness",
    color = "Analysis Type"
  ) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "top",
    strip.background = element_rect(fill = "grey95", color = NA),
    panel.grid = element_blank()
  )

# what does the response look like?
hist(greenspaces$nearest_dist_m)
# it is postively skewed


################ 
#### Fit models
################

# polynomial or linear?
# Linear
m_linear <- glmmTMB(
  species_richness ~ log10(nearest_dist_m) * analysis +
    Season + log10(number_of_checklists),
  data = greenspaces,
  family = nbinom2
)

# Fit polynomial GLMM (quadratic term for nearest distance)
poly_glmm <- glmmTMB(
  species_richness ~ poly(log10(nearest_dist_m), 2, raw = TRUE) * analysis +
    Season +
    log10(number_of_checklists),
  data = greenspaces,
  family = nbinom2
)

AIC(m_linear, poly_glmm)
anova(m_linear, poly_glmm, test = "Chisq")
# the polynomial relationship does not improve AIC, so we should go with the simplier linear trend

# check model results
summary(m_linear)

# check for overdispersion
performance::check_overdispersion(m_linear)

library(DHARMa)

# Simulate residuals
sim_res <- simulateResiduals(fittedModel = m_linear, n = 1000)

# Plot diagnostics
plot(sim_res)

# Create a prediction grid for nearest distance (log10 transformed)
dist_grid <- with(greenspaces, 
                  list(nearest_dist_m = seq(min(nearest_dist_m, na.rm = TRUE),
                                            max(nearest_dist_m, na.rm = TRUE),
                                            length.out = 100),
                       number_of_checklists = median(number_of_checklists, na.rm = TRUE),
                       Season = levels(Season),
                       analysis = levels(analysis)))

# Get predicted response from emmeans
emm_poly <- emmeans(m_linear,
                    ~ nearest_dist_m | analysis + Season,
                    at = dist_grid,
                    type = "response")

emm_poly_df <- as.data.frame(emm_poly)

# Update the ggplot
ggplot(emm_poly_df, aes(x = nearest_dist_m, y = response, color = analysis, fill = analysis)) +
  geom_line(size = 1) +
  geom_smooth(family = poly) +
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL), alpha = 0.2, color = NA) +
  facet_wrap(~Season) +
  scale_x_log10() +
  scale_color_manual(values = c("residential" = "#006400", "migratory" = "#800080", "total" = "#1E90FF")) +
  scale_fill_manual(values = c("residential" = "#006400", "migratory" = "#800080", "total" = "#1E90FF")) +
  labs(
    x = "Nearest Neighbor Distance (m)",
    y = "Predicted Species Richness",
    color = "Analysis",
    fill = "Analysis"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    legend.position = "bottom",
    panel.background = element_rect(color = "black", linewidth = 0.5)
  )

ggsave("Figures/predicted_richness_nearest_neighbor.PNG", bg = "transparent")



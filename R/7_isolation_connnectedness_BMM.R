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

#############
## Build GLMM
#############
# Run Negative Binomial GLMMs with 'Nearest_neighbor' as random effect
model_species <- glmmTMB(
  species_richness ~ log1p(nearest_dist_m) + (1 | Park_Addre),
  data = greenspaces,
  family = nbinom2
)

summary(model_species)
Anova(model_species)

# Extract slopes
slopes_df <- tidy(model_species, effects = "fixed", conf.int = TRUE) %>% 
    filter(term == "log1p(nearest_dist_m)") %>% 
    mutate(model = "Species Richness")

# Make prediction grid for nearest_dist_m
dist_grid <- with(greenspaces,
                  list(
                    nearest_dist_m = seq(min(nearest_dist_m, na.rm = TRUE),
                                         max(nearest_dist_m, na.rm = TRUE),
                                         length.out = 100)
                  ))

# Get predictions on response scale
emm_dist <- emmeans(model_species,
                    ~ log1p(nearest_dist_m),
                    at = dist_grid,
                    type = "response")

emm_dist_df <- as.data.frame(emm_dist)

# Plot predictions + raw data
ggplot() +
  geom_point(data = greenspaces,
             aes(x = nearest_dist_m, y = species_richness),
             alpha = 0.7, color = "black") +
  geom_ribbon(data = emm_dist_df,
              aes(x = nearest_dist_m, ymin = asymp.LCL, ymax = asymp.UCL),
              alpha = 0.2, fill = "forestgreen") +
  geom_line(data = emm_dist_df,
            aes(x = nearest_dist_m, y = response),
            color = "forestgreen", size = 1) +
  scale_x_log10()+
  labs(
    x = "Nearest Neighbor Distance (m)",
    y = "Predicted Species Richness"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(color = "black", linewidth = 1),
    plot.background = element_rect(fill = "transparent", colour = NA)
  )

ggsave("Figures/predicted_richness_nearest_neighbor.PNG", bg = "transparent")

##############################################
### MODEL THIS FOR THE FULL ANNUAL CYCLE
###but as seasonality is included in the model (fixed effect)
##############################################
glmm_season_migration_isolation <- glmmTMB(
  species_richness ~ log1p(nearest_dist_m) * Season * analysis + log10(number_of_checklists),
  data = greenspaces,
  family = nbinom2
)
summary(glmm_season_migration_isolation)
Anova(glmm_season_migration_isolation)

# Create predicted trends across nearest distance
emm_distance <- emtrends(
  glmm_season_migration_isolation,
  ~ analysis | Season,              
  var = "log1p(nearest_dist_m)",  
  type = "response"
)

# Convert to data frame
emm_distance_df <- as.data.frame(emm_distance)

# Recode analysis for better labeling if needed
emm_distance_df <- emm_distance_df %>%
  mutate(analysis = fct_recode(analysis,
                               "Migratory" = "migratory_code",
                               "Resident" = "resident_code"))

# Plot marginal slopes
ggplot(emm_distance_df, aes(x = analysis, y = `log1p(nearest_dist_m).trend`, color = analysis)) +
  geom_point(size = 4, position = position_dodge(width = 0.3)) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2, position = position_dodge(width = 0.3)) +
  facet_wrap(~Season, scales = "free_y") +
  scale_color_brewer(palette = "Dark2") +
  labs(
    x = NULL,
    y = "Marginal Slope of Nearest Distance",
    title = NULL,
    color = "Migration Status"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(color = "black", linewidth = 1),
    strip.background = element_rect(fill = "grey90", color = NA),
    strip.text = element_text(face = "bold", size = 12),
    axis.title = element_text(face = "bold", size = 14),
    axis.text = element_text(color = "black", size = 12),
    legend.position = "none",
  )
ggsave("Figures/marginal_slopes_nearest_neighbor_SR_season_analysis.PNG", bg = "transparent")

########################################
### MODEL THIS FOR THE FULL ANNUAL CYCLE
### pero one model per season (random effect)
########################################
# Spring Migration
spring_model <- glmmTMB(
  species_richness ~ log1p(nearest_dist_m) * analysis + log10(number_of_checklists),
  data = greenspaces %>% filter(Season == "Spring Migration"),
  family = nbinom2
)

# Breeding
breeding_model <- glmmTMB(
  species_richness ~ log1p(nearest_dist_m) * analysis + log10(number_of_checklists),
  data = greenspaces %>% filter(Season == "Breeding"),
  family = nbinom2
)

# Fall Migration
fall_model <- glmmTMB(
  species_richness ~ log1p(nearest_dist_m) * analysis + log10(number_of_checklists),
  data = greenspaces %>%filter(Season == "Fall Migration"),
  family = nbinom2
)

# Overwintering
overwinter_model <- glmmTMB(
  species_richness ~ log1p(nearest_dist_m) * analysis + log10(number_of_checklists),
  data = greenspaces %>% filter(Season == "Overwintering"),
  family = nbinom2
)

summary(spring_model)
Anova(spring_model)
summary(breeding_model)
Anova(breeding_model)
summary(fall_model)
Anova(fall_model)
summary(overwinter_model)
Anova(overwinter_model)

# Extract slopes from emtrends
## Spring
margins_spring <- emtrends(spring_model, var = "log1p(nearest_dist_m)",
                           data = greenspaces %>% filter(Season == "Spring Migration")) %>%
  as.data.frame() %>%mutate(Season = "Spring Migration")

## Breeding
margins_breeding <- emtrends(breeding_model, var = "log1p(nearest_dist_m)",
                             data = greenspaces %>% filter(Season == "Breeding")) %>%
  as.data.frame() %>% mutate(Season = "Breeding")

## Fall
margins_fall <- emtrends(fall_model, var = "log1p(nearest_dist_m)",
                         data = greenspaces %>% filter(Season == "Fall Migration")) %>%
  as.data.frame() %>% mutate(Season = "Fall Migration")

## Overwintering
margins_overwinter <- emtrends(overwinter_model, var = "log1p(nearest_dist_m)",
                               data = greenspaces %>% filter(Season == "Overwintering")) %>%
  as.data.frame() %>% mutate(Season = "Overwintering")

# Combine all seasons
all_margins <- bind_rows(
  margins_spring,
  margins_breeding,
  margins_fall,
  margins_overwinter
)

# Function to create emmeans prediction data for a season
get_emm_predictions <- function(model, season_data, season_name) {
  # Create a grid of nearest_dist_m values and median checklists
  dist_grid <- with(season_data,
                    list(
                      nearest_dist_m = seq(min(nearest_dist_m, na.rm = TRUE),
                                           max(nearest_dist_m, na.rm = TRUE),
                                           length.out = 50),
                      number_of_checklists = median(number_of_checklists, na.rm = TRUE)
                    ))
  
  # Get predicted response from emmeans, explicitly passing the data
  emm_df <- emmeans(model,
                    ~ log1p(nearest_dist_m) * analysis,
                    at = dist_grid,
                    type = "response",
                    data = season_data) %>%   # <- Pass the data here
    as.data.frame() %>%
    mutate(Season = season_name)
  
  return(emm_df)
}

# Generate prediction data for each season
emm_spring <- get_emm_predictions(spring_model, greenspaces %>% filter(Season == "Spring Migration"), "Spring Migration")
emm_breeding <- get_emm_predictions(breeding_model, greenspaces %>% filter(Season == "Breeding"), "Breeding")
emm_fall <- get_emm_predictions(fall_model, greenspaces %>% filter(Season == "Fall Migration"), "Fall Migration")
emm_overwinter <- get_emm_predictions(overwinter_model, greenspaces %>% filter(Season == "Overwintering"), "Overwintering")

# Combine all seasons
emm_all <- bind_rows(emm_spring, emm_breeding, emm_fall, emm_overwinter)

# Plot predicted richness vs nearest distance
ggplot(emm_all, aes(x = nearest_dist_m, y = response, color = analysis, fill = analysis)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL), alpha = 0.2, color = NA) +
  facet_wrap(~Season) +
  scale_x_log10() +
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
    panel.background = element_rect(color = "black", linewidth = 0.5),
    plot.background = element_rect(fill = "transparent", color = NA)
  )

## make linear lines
ggplot() +
  geom_point(data = greenspaces, 
             aes(x = nearest_dist_m, y = species_richness, color = analysis), alpha = 0.4, size = 2) +
  geom_ribbon(data = emm_all, 
              aes(x = nearest_dist_m, ymin = asymp.LCL, ymax = asymp.UCL, fill = analysis), alpha = 0.2, color = NA) +
  geom_line(data = emm_all, 
            aes(x = nearest_dist_m, y = response, color = analysis),
            size = 1) +
  facet_wrap(~Season) +
  scale_x_log10() + 
  labs(
    x = "Nearest Neighbor Distance (m)",
    y = "Species Richness",
    color = "Analysis",
    fill = "Analysis"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    legend.position = "bottom",
    panel.background = element_rect(color = "black", linewidth = 0.5),
    plot.background = element_rect(fill = "transparent", color = NA)
  )

ggsave("Figures/isolation_predicted_richness_season_analysis.PNG", bg = "transparent")

# Compute 95% CI for each trend
all_margins <- all_margins %>%
  mutate(
    asymp.LCL = `log1p(nearest_dist_m).trend` - 1.96 * SE,
    asymp.UCL = `log1p(nearest_dist_m).trend` + 1.96 * SE
  )

### Marginal slopes
# Plot marginal slopes of nearest neighbor distance on species richness
ggplot(all_margins, aes(x = analysis, y = `log1p(nearest_dist_m).trend`, color = analysis)) +
  geom_point(size = 4, position = position_dodge(width = 0.3)) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2, position = position_dodge(width = 0.3)) +
  facet_wrap(~Season, scales = "free_y") +
  scale_color_brewer(palette = "Dark2") +
  labs(
    x = NULL,
    y = "Marginal Slope of Nearest Neighbor Distance on Species Richness",
    color = "Analysis"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(color = "black", linewidth = 1),
    strip.background = element_rect(fill = "grey90", color = NA),
    strip.text = element_text(face = "bold", size = 12),
    axis.title = element_text(face = "bold", size = 14),
    axis.text = element_text(color = "black", size = 12),
    legend.position = "none"
  )

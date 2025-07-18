# Load packages
library("ggplot2")
library("tigris")
library("dplyr")
library("sf")
library("ggpmisc")
library("rnaturalearth")
library("rnaturalearthdata")
library("concaveman")
library("glmmTMB")

# Read the shapefile
final_avonet <- st_read("Data/AVONET/final_avonet.shp")

# Rename columns to original names
final_avonet <- final_avonet %>%
  rename(
    COMMON = COMMON, SCIENTIFIC = SCIENTI, LATITUDE = LATITUD, LONGITUDE = LONGITU,
    COUNTY = COUNTY, LOCALITY = LOCALIT, L.ID = L_ID, L.TYPE = L_TYPE,
    DATE = DATE, O.COUNT = O_COUNT, OBSERV.ID = OBSERV_, SEI = SEI, MONTH = MONTH, 
    Shape_Area = Shap_Ar, Park_Addre = Prk_Add, Park_Size_ = Prk_Sz_, 
    Park_Siz_1 = Prk_S_1, Park_Size1 = Prk_Sz1, Park_Name = Park_Nm, 
    area = area, lists = lists, geometry = geometry,
    species_richness = spcs_rc, Migration = Migratn, Season = Season
  )

#############################################
### IGNORE -- CODE FOR FIGURE 1 -- Study Area
#############################################
# Ensure projected CRS for better display
final_avonet_proj <- st_transform(final_avonet, crs = 5070)

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
  geom_sf(data = final_avonet_proj, aes(size = lists, fill = species_richness),
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

## Separate into two RDS files for Migratory and Residential
migratory_residential <- final_avonet %>%
  mutate(migration_status = case_when(
    Migration == 1 ~ "residential",
    Migration %in% c(2, 3) ~ "migratory",
    TRUE ~ NA_character_  
  ))

### Migratory
migratory_data <- migratory_residential %>%
  filter(migration_status == "migratory")
length(unique(migratory_data$SCIENTIFIC)) #260

### Add richness
migratory_data <- migratory_data %>%
  group_by(Park_Addre, MONTH) %>%
  mutate(migratory_richness = n_distinct(SCIENTIFIC)) %>%
  ungroup()

#### SaveRDS
#saveRDS(migratory_data, "Data/AVONET/migratory_data.rds")

### Residential
residential_data <- migratory_residential %>%
  filter(migration_status == "residential")
length(unique(residential_data$SCIENTIFIC)) #73

### Add richness
residential_data <- residential_data %>%
  group_by(Park_Addre, MONTH) %>%
  mutate(residential_richness = n_distinct(SCIENTIFIC)) %>%
  ungroup()

#### SaveRDS
#saveRDS(residential_data, "Data/AVONET/residential_data.rds")

### GRAPHICAL REPRESENTATION FOR RESIDENTIAL VS MIGRATORY
### Using a negative binomial GLMM (glmmTMB)
# Summarize data by park
residential_summary <- residential_data %>%
  group_by(Park_Name) %>%
  summarise(
    residential_richness = mean(residential_richness),
    Park_Size_Ha = mean(Park_Size_)
  )

# Fit negative binomial model
nb_model_r <- glmmTMB(
  residential_richness ~ log10(Park_Size_Ha),
  data = residential_summary,
  family = nbinom2
)

# Model summary + slope
summary(nb_model_r)
slope_nb_r <- summary(nb_model_r)$coefficients$cond["log10(Park_Size_Ha)", "Estimate"]
cat("Slope for residential richness:", slope_nb_r, "\n")

# Generate prediction data
pred_residential <- data.frame(
  Park_Size_Ha = seq(min(residential_summary$Park_Size_Ha),
                     max(residential_summary$Park_Size_Ha),
                     length.out = 100)
)
pred_residential$pred_richness <- predict(nb_model_r, newdata = pred_residential, type = "response")

# Plot residential model
ggplot(residential_summary, aes(x = Park_Size_Ha, y = residential_richness)) +
  geom_point(color = "black") +
  geom_line(data = pred_residential,
            aes(x = Park_Size_Ha, y = pred_richness),
            color = "#467010", size = 1) +
  scale_x_log10() +
  labs(x = "Park Size (ha, log scale)", y = "Resident Richness") +
  theme_bw()

###########################################
# MIGRATORY + RESIDENTIAL richness ~ Park size
###########################################
both_summary <- migratory_residential %>%
  group_by(Park_Name) %>%
  summarise(
    species_richness = mean(species_richness),
    Park_Size_Ha = mean(Park_Size_)
  )

# Fit NB model
nb_model_b <- glmmTMB(
  species_richness ~ log10(Park_Size_Ha),
  data = both_summary,
  family = nbinom2
)

summary(nb_model_b)
slope_nb_b <- summary(nb_model_b)$coefficients$cond["log10(Park_Size_Ha)", "Estimate"]
cat("Slope for migratory + resident richness:", slope_nb_b, "\n")

# Predictions for migratory + resident model
pred_both <- data.frame(
  Park_Size_Ha = seq(min(both_summary$Park_Size_Ha),
                     max(both_summary$Park_Size_Ha),
                     length.out = 100)
)
pred_both$pred_richness <- predict(nb_model_b, newdata = pred_both, type = "response")

# Plot migratory + resident model
ggplot(both_summary, aes(x = Park_Size_Ha, y = species_richness)) +
  geom_point(color = "black") +
  geom_line(data = pred_both,
            aes(x = Park_Size_Ha, y = pred_richness),
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
    Park_Size_Ha = mean(Park_Size_)
  ) %>%
  mutate(group = "Migratory")

# Prepare residential data
residential_df <- residential_data %>%
  group_by(Park_Name) %>%
  summarise(
    richness = mean(residential_richness),
    Park_Size_Ha = mean(Park_Size_)
  ) %>%
  mutate(group = "Resident")

# Prepare combined migratory + residential data
both_df <- migratory_residential %>%
  group_by(Park_Name) %>%
  summarise(
    richness = mean(species_richness),
    Park_Size_Ha = mean(Park_Size_)
  ) %>%
  mutate(group = "Migratory + Resident")

# Combine all into one dataframe
combined_df <- bind_rows(migratory_df, residential_df, both_df)

# Fit NB model with interaction
nb_model_combined <- glmmTMB(
  richness ~ log10(Park_Size_Ha) * group,
  data = combined_df,
  family = nbinom2
)

summary(nb_model_combined)

# Generate prediction grid for all groups
pred_grid <- expand.grid(
  Park_Size_Ha = seq(min(combined_df$Park_Size_Ha),
                     max(combined_df$Park_Size_Ha),
                     length.out = 100),
  group = unique(combined_df$group)
)
pred_grid$pred_richness <- predict(nb_model_combined, newdata = pred_grid, type = "response")

#### Generate 95% Confidence Intervals 
# Generate prediction grid
pred_grid <- expand.grid(
  Park_Size_Ha = seq(min(combined_df$Park_Size_Ha),
                     max(combined_df$Park_Size_Ha),
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
NBGLMM_Park_Size_Status <- ggplot(combined_df, aes(x = Park_Size_Ha, y = richness, color = group)) +
  geom_point() +
  geom_ribbon(
    data = pred_grid,
    aes(
      x = Park_Size_Ha,
      ymin = lwr,
      ymax = upr,
      fill = group
    ),
    alpha = 0.3,
    inherit.aes = FALSE
  ) +
  geom_line(
    data = pred_grid,
    aes(x = Park_Size_Ha, y = fit, color = group),
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
    x = "Park Size (ha, log scale)",
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

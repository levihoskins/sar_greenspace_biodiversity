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

# Read the shapefile and rename columns
final_avonet <- st_read("Data/AVONET/final_avonet.shp")
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

# Project to UTM (meters)
final_avonet_proj <- st_transform(final_avonet, crs = 26917)

## Get migration status and make sure park names are correct
migratory_residential <- final_avonet %>%
  mutate(
    migration_status = case_when(
      Migration == 1 ~ "residential",
      Migration %in% c(2, 3) ~ "migratory",
      TRUE ~ NA_character_
    ),
    Park_Addre_clean = toupper(Park_Addre)
  ) %>%
  group_by(Park_Addre, Park_Size_, MONTH) %>%
  mutate(species_richness = n_distinct(SCIENTIFIC)) %>%
  ungroup()

# Group by parks
greenspaces <- migratory_residential %>%
  group_by(Park_Addre) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

# read in ParkServe data
# we probably want to consider the distance to nearest green space or natural area, not just greenspaces
# included in this study so we will use all the ParkServe polygons
gs_poly <- st_read("Data/Polygons/ParkServe_Park_SouthFL.shp")

# group by Park_Addre and remove small parks (<0.05ha)
gs_poly <- gs_poly %>%
  group_by(Park_Addre) %>%
  summarise(geometry = st_union(geometry), .groups = "drop") %>%
  mutate(area=as.numeric(st_area(.)/10000)) %>%
  filter(area > 0.05)

# Get nearest distance greenspace
nearest_distance <- numeric(nrow(gs_poly))

for (i in seq_len(nrow(gs_poly))) {
  others <- gs_poly[-i, ]
  distances <- as.numeric(st_distance(gs_poly[i, ], others))
  nearest_distance[i] <- min(distances[distances > 0])
}

# add distances to the dataframe
gs_poly$nearest_dist_m <- nearest_distance
gs_poly$nearest_dist_km <- nearest_distance / 1000

# make the gs_poly a data frame
gs_poly_df <- gs_poly %>%
  as.data.frame()

# save this file
saveRDS(gs_poly, "Data/Intermediate_Data/distance_to_nearest_greenspace_parkserve.RDS")

# now add this to the greenspaces data frame
greenspaces <- greenspaces %>%
  left_join(., gs_poly_df %>% dplyr::select(Park_Addre, nearest_dist_m, nearest_dist_km), by=c("Park_Addre"))

summary(greenspaces$nearest_dist_m)


#############
## Build GLMM
#############
# Calculate richness per migration_status per park & county from migratory_residential
richness_by_migration <- migratory_residential %>%
  filter(!is.na(migration_status)) %>%
  group_by(Park_Addre_clean, COUNTY, migration_status) %>%
  summarise(richness = n_distinct(SCIENTIFIC), .groups = "drop") %>%
  pivot_wider(names_from = migration_status, values_from = richness, values_fill = 0) %>%
  mutate(species_richness = migratory + residential)

# Join richness_by_migration to greenspaces
greenspace_model_data <- greenspaces %>%
  st_join(richness_by_migration, by = c("Park_Addre_clean", "COUNTY"))

# Replace NA richness values with zero
greenspace_model_data <- greenspace_model_data %>%
  mutate(
    migratory_richness = ifelse(is.na(migratory), 0, migratory),
    residential_richness = ifelse(is.na(residential), 0, residential)
  )

# Run Negative Binomial GLMMs with 'Nearest_neighbor' as random effect
model_species <- glmmTMB(
  species_richness ~ log1p(nearest_dist_m) + (1 | Park_Addre),
  data = greenspace_model_data,
  family = nbinom2
)

model_migratory <- glmmTMB(
  migratory ~ log1p(nearest_dist_m) + (1 | Park_Addre),
  data = greenspace_model_data,
  family = nbinom2
)

model_residential <- glmmTMB(
  residential ~ log1p(nearest_dist_m) + (1 | Park_Addre),
  data = greenspace_model_data,
  family = nbinom2
)

# Summaries
summary(model_species)
summary(model_migratory)
summary(model_residential)

# Extract slopes
slopes_df <- bind_rows(
  tidy(model_species, effects = "fixed", conf.int = TRUE) %>% 
    filter(term == "log1p(nearest_dist_m)") %>% 
    mutate(model = "Species Richness"),
  
  tidy(model_migratory, effects = "fixed", conf.int = TRUE) %>% 
    filter(term == "log1p(nearest_dist_m)") %>% 
    mutate(model = "Migratory Species"),
  
  tidy(model_residential, effects = "fixed", conf.int = TRUE) %>% 
    filter(term == "log1p(nearest_dist_m)") %>% 
    mutate(model = "Residential Species")
)

# PLOT
ggplot(slopes_df, aes(
  x = model,
  y = estimate,
  color = model
)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.3, size = 1) +
  geom_point(size = 4) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(
    title = NULL,
    x = NULL,
    y = "Nearest Distance (slope estimate)",
    color = "Model"
  ) +
  scale_color_manual(values = c(
    "Migratory Species"   = "#2c7fb8",
    "Residential Species" = "#feb24c",
    "Species Richness"    = "#2a4c09"
  )) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black")
  ) +
  coord_flip() +
  theme(aspect.ratio = 0.5)

# Save PNG
ggsave('Figures/Nearest_Distance_Overall.png', bg = 'transparent')

##############################################
### MODEL THIS FOR THE FULL ANNUAL CYCLE
###but as seasonality is included in the model (fixed effect)
##############################################
# Summarize species richness by greenspace per season
richness_by_greenspace <- migratory_residential %>%
  group_by(Park_Addre_clean, COUNTY, Park_Size_, lists, Season, migration_status) %>%
  summarise(species_richness = n_distinct(SCIENTIFIC), .groups = "drop")

# Join richness to greenspaces dataset with distance to neighbor
season_model_data <- greenspaces %>%
  st_join(richness_by_greenspace, by = "Park_Addre_clean")

#overall
glmm_season_migration <- glmmTMB(
  species_richness ~ log1p(nearest_dist_km) * Season * migration_status + lists +
    log1p(Park_Size_) + (1 | Park_Addre),
  data = season_model_data,
  family = nbinom2
)
summary(glmm_season_migration)

# Extract margin slopes
margins_all <- emtrends(
  glmm_season_migration,
  var   = "nearest_dist_km",
  specs = c("Season", "migration_status")
) %>% as.data.frame()

add_ci <- function(df) {
  alpha <- 0.05
  t_crit <- qt(1 - alpha/2, df = df$df[1])  
  df %>%
    mutate(
      lower.CL = nearest_dist_km.trend - t_crit * SE,
      upper.CL = nearest_dist_km.trend + t_crit * SE
    )
}

margins_all <- add_ci(margins_all)

# Define color
status_colors <- c(
  "migratory"   = "#b1d8b7",
  "residential" = "#2a4c09"
)

ggplot(margins_all, 
       aes(x = Season, 
           y = nearest_dist_km.trend, 
           ymin = lower.CL, ymax = upper.CL,
           color = migration_status,
           group = migration_status)) +
  geom_point(size = 3, position = position_dodge(width = 0.6)) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                width = 0.2, 
                position = position_dodge(width = 0.6)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(values = status_colors) +
  labs(
    title = NULL,
    x = NULL,
    y = "Slope (Effect of Nearest Greenspace Distance)",
    color = "Migration Status"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black")
  ) +
  coord_flip() +
  theme(aspect.ratio = 0.5)

########################################
### MODEL THIS FOR THE FULL ANNUAL CYCLE
### pero one model per season (random effect)
########################################
# Spring Migration
spring_model <- glmmTMB(
  species_richness ~ log1p(nearest_dist_km) * migration_status + log1p(Park_Size_) + lists + 
    (1 | Park_Addre) + (1 | Season),
  data = season_model_data %>% filter(Season == "Spring Migration"),
  family = nbinom2
)

# Breeding
breeding_model <- glmmTMB(
  species_richness ~ log1p(nearest_dist_km) * migration_status + log1p(Park_Size_) + lists + 
    (1 | Park_Addre) + (1 | Season),
  data = season_model_data %>% filter(Season == "Breeding"),
  family = nbinom2
)

# Fall Migration
fall_model <- glmmTMB(
  species_richness ~ log1p(nearest_dist_km) * migration_status + log1p(Park_Size_) + lists + 
    (1 | Park_Addre) + (1 | Season),
  data = season_model_data %>% filter(Season == "Fall Migration"),
  family = nbinom2
)

# Overwintering
overwinter_model <- glmmTMB(
  species_richness ~ log1p(nearest_dist_km) * migration_status + log1p(Park_Size_) + lists + 
    (1 | Park_Addre) + (1 | Season),
  data = season_model_data %>% filter(Season == "Overwintering"),
  family = nbinom2
)

summary(spring_model)
summary(breeding_model)
summary(fall_model)
summary(overwinter_model)

# Extract slopes from emtrends
margins_spring <- emtrends(spring_model, var = "nearest_dist_km") %>% 
  as.data.frame() %>% mutate(Season = "Spring Migration")

margins_breeding <- emtrends(breeding_model, var = "nearest_dist_km") %>% 
  as.data.frame() %>% mutate(Season = "Breeding")

margins_fall <- emtrends(fall_model, var = "nearest_dist_km") %>% 
  as.data.frame() %>% mutate(Season = "Fall Migration")

margins_overwinter <- emtrends(overwinter_model, var = "nearest_dist_km") %>% 
  as.data.frame() %>% mutate(Season = "Overwintering")

# Combine each season
all_margins <- bind_rows(
  margins_spring,
  margins_breeding,
  margins_fall,
  margins_overwinter
)

# Add 95% CI, t-statistic, p-value, and significance
alpha <- 0.05
all_margins <- all_margins %>%
  mutate(
    t_value = nearest_dist_km.trend / SE,
    p.value = 2 * (1 - pt(abs(t_value), df = df)),
    t_crit = qt(1 - alpha/2, df = df),
    lower.CL = nearest_dist_km.trend - t_crit * SE,
    upper.CL = nearest_dist_km.trend + t_crit * SE,
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ ""
    )
  )

# Build table for export
table_data <- all_margins %>%
  dplyr::select(
    Season,
    migration_status,
    Slope = nearest_dist_km.trend,
    SE,
    CI_low = lower.CL,
    CI_high = upper.CL,
    df,
    sig
  ) %>%
  mutate(
    CI = paste0(round(CI_low, 2), ", ", round(CI_high, 2)),
    Slope_SE = paste0(round(Slope, 2), " ± ", round(SE, 2), sig)
  ) %>%
  dplyr::select(Season, migration_status, Slope_SE, CI, df)

# Create table using gt package
gt_table <- table_data %>%
  gt(groupname_col = "Season") %>%
  cols_label(
    migration_status = "Migration Status",
    Slope_SE = "Slope ± SE",
    CI = "95% CI",
    df = "df"
  ) %>%
  tab_header(
    title = md("**Seasonal effects of nearest greenspace distance on species richness**"),
    subtitle = "Slopes ± SE (with significance stars) and 95% CI for migratory vs residential species."
  ) %>%
  tab_options(table.font.size = "small")
gt_table

# Color palette for migration status
migration_palette <- c(
  "migratory"   = "#2c7fb8",
  "residential" = "#feb24c" 
)

# Plot
season_plot <- ggplot(
  all_margins,
  aes(
    x = Season,
    y = nearest_dist_km.trend,
    ymin = lower.CL,
    ymax = upper.CL,
    color = migration_status
  )
) +
  geom_point(position = position_dodge(width = 0.6), size = 3) +
  geom_errorbar(
    aes(ymin = lower.CL, ymax = upper.CL),
    position = position_dodge(width = 0.6),
    width = 0.2
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(values = migration_palette) +
  labs(
    title = NULL,
    x = NULL,
    y = "Slope (Effect of Nearest Greenspace Distance)",
    color = "Migration Status"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black")
  ) +
  coord_flip() +
  theme(aspect.ratio = 0.5)
season_plot

# Save PNG
ggsave(
  "Figures/Nearest_Greenspace_Status.png", plot = season_plot, width = 7, height = 4, dpi = 300, bg = "transparent"
)


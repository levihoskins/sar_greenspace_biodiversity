# Load packages
library("sf")
library("dplyr")
library("glmmTMB")
library("emmeans")
library("ggplot2")

# Read shapefile & rename columns
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

# Add migration status
migratory_residential <- final_avonet %>%
  mutate(
    migration_status = case_when(
      Migration == 1 ~ "residential",
      Migration %in% c(2, 3) ~ "migratory",
      TRUE ~ NA_character_
    )
  )

# Reorder season categories so that they appear correct when plotted
migratory_residential$Season <- factor(
  migratory_residential$Season,
  levels = c("Overwintering", "Spring Migration", "Breeding", "Fall Migration"),
  ordered = TRUE
)

#######################
### Super basic NB GLMM
#######################

# Fit Negative Binomial GLMM
## Season model
glmm_season <- glmmTMB(
  species_richness ~ Season + (1 | Park_Addre),
  data = migratory_residential,
  family = nbinom2
)

summary(glmm_season)

# emmeans pairwise comparisons for Season
emm_season <- emmeans(glmm_season, ~ Season)
pairs_season <- pairs(emm_season)  

print(emm_season)
print(pairs_season)

# Convert to df for plotting
season_df <- as.data.frame(emm_season)

# Plot
ggplot(season_df, aes(x = Season, y = emmean, color = Season)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) +
  labs(
    title = "Predicted Species Richness per Season",
    y = "Predicted Richness",
    x = "Season"
  ) +
  scale_color_manual(values = c(
    "Overwintering" = "#e0f19c",
    "Spring Migration" = "#b1d8b7",
    "Breeding" = "#467010",
    "Fall Migration" = "#2a4c09"
  )) +
  theme_minimal() +
  coord_flip()

#########################################
### Add in migration status as a variable
#########################################
migratory_residential$migration_status <- factor(
  migratory_residential$migration_status,
  levels = c("residential", "migratory")
)

# Fit NB GLMMs with migration_status
## Season × migration_status
glmm_season_status <- glmmTMB(
  species_richness ~ Season * migration_status + (1 | Park_Addre),
  data = migratory_residential,
  family = nbinom2
)

summary(glmm_season_status)

# Model comparison
AIC(glmm_month_status, glmm_season_status)

# Get emmeans
emm_season_status <- emmeans(glmm_season_status, ~ Season * migration_status)
pairs_season_status <- pairs(emm_season_status)

print(emm_season_status)
print(pairs_season_status)

# Convert to df for plotting
season_status_df <- as.data.frame(emm_season_status)

# Plot (Poster)
ggplot(season_status_df,
       aes(x = Season, y = emmean, color = migration_status, group = migration_status)) +
  geom_point(position = position_dodge(width = 0.3), size = 4) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                position = position_dodge(width = 0.3), width = 0.3) +
  labs(
    title = "Predicted Species Richness by Season & Migration Status",
    y = "Predicted Species Richness",
    x = NULL,
    color = "Migration Status"
  ) +
  scale_color_manual(values = c("migratory" = "#b1d8b7", "residential" = "#2a4c09")) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.ticks = element_line(color = "grey30"),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black")
    ) +
  coord_flip()

## Plot (Paper -- but not the one I am keeping)
ggplot(season_status_df,
       aes(x = Season, y = emmean, color = migration_status, group = migration_status)) +
  geom_point(position = position_dodge(width = 0.2), size = 3.5) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                position = position_dodge(width = 0.2), width = 0.2, size = 0.7) +
  labs(
    title = "Predicted Species Richness by Season & Migration Status",
    y = "Species Richness (Slope)",
    x = NULL,
    color = "Migration Status"
  ) +
  scale_color_manual(values = c("migratory" = "#2c7fb8", "residential" = "#feb24c")) +
  theme_minimal(base_size = 14, base_family = "serif") +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.ticks = element_line(color = "grey30"),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black")
  ) +
  coord_flip()

### Validating the model
# For Season × migration_status model
sim_res_season <- simulateResiduals(glmm_season_status, n = 100)
plot(sim_res_season)
testDispersion(sim_res_season)




############################
### Adding effort covariates
############################
## Season as a fixed effect
# Season model with effort covariate and migration status
glmm_season_status_ec <- glmmTMB(
  species_richness ~ Season * migration_status + lists + (1 | Park_Addre),
  data = migratory_residential,
  family = nbinom2
)

# emmeans
emm_season_status_ec <- emmeans(glmm_season_status_ec, ~ Season * migration_status)
pairs_season_status_ec <- pairs(emm_season_status_ec)

print(emm_season_status_ec)
print(pairs_season_status_ec)

# Convert to DF for plotting
season_status_ec_df <- as.data.frame(emm_season_status_ec)

# Plot (Poster)
ggplot(season_status_ec_df,
       aes(x = Season, y = emmean, color = migration_status, group = migration_status)) +
  geom_point(position = position_dodge(width = 0.6), size = 4) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                position = position_dodge(width = 0.6), width = 0.6) +
  labs(
    title = NULL,
    y = "Predicted Species Richness (with effort covariate)",
    x = NULL,
    color = "Migration Status"
  ) +
  scale_color_manual(values = c("migratory" = "#b1d8b7", "residential" = "#2a4c09")) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.ticks = element_line(color = "grey30"),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black")
  ) +
  coord_flip() +
  theme(aspect.ratio = 0.5 )

# Plot (Figure)
ggplot(season_status_ec_df,
       aes(x = Season, y = emmean, color = migration_status, group = migration_status)) +
  geom_point(position = position_dodge(width = 0.6), size = 4) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                position = position_dodge(width = 0.6), width = 0.6) +
  labs(
    title = NULL,
    y = "Predicted Species Richness (Effort-Adjusted)",
    x = NULL,
    color = "Migration Status"
  ) +
  scale_color_manual(values = c("migratory" = "#2c7fb8", "residential" = "#feb24c")) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.ticks = element_line(color = "grey30"),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black")
  ) +
  coord_flip() +
  theme(aspect.ratio = 0.5)

ggsave('Figures/Predicted_SR_effort_covariate.png', bg = 'transparent')

################################
### Season as a random intercept ### Gives exact same as above
################################
glmm_season_random <- glmmTMB(
  species_richness ~ Season * migration_status + lists + (1 | Park_Addre) + (1 | Season),
  data = migratory_residential,
  family = nbinom2
)

# Get emmeans
emm_season_random_ec <- emmeans(glmm_season_random, 
                                ~ Season * migration_status,
                                at = list(Season = unique(migratory_residential$Season)))

# Pairwise comparisons
pairs_season_random_ec <- pairs(emm_season_random_ec)

print(emm_season_random_ec)
print(pairs_season_random_ec)

# Convert to df for plotting
season_random_ec_df <- as.data.frame(emm_season_random_ec)

head(season_random_ec_df)

# Plot
ggplot(season_random_ec_df,
       aes(x = Season, y = emmean, color = migration_status, group = migration_status)) +
  geom_point(position = position_dodge(width = 0.6), size = 4) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                position = position_dodge(width = 0.6), width = 0.6) +
  labs(
    title = NULL,
    y = "Predicted Species Richness (Effort-Adjusted)",
    x = NULL,
    color = "Migration Status"
  ) +
  scale_color_manual(values = c("migratory" = "#2c7fb8", "residential" = "#feb24c")) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.ticks = element_line(color = "grey30"),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black")
  ) +
  coord_flip() +
  theme(aspect.ratio = 0.5)

# Adding in external variables -- GHM index and Dynamic World
## GHMi shows the global human modification index which is a scale of 0-1 with one being 
## a super urban area and 0 being the most absent from urban area
### Dynamic World assigns a number to the habitat structure/type

# Load packages
library("sf")
library("dplyr")
library("glmmTMB")
library("emmeans")
library("ggplot2")
library("lme4")
library("readr")
library("MASS")
library("ggeffects")
library("patchwork")
library("broom")
library("car")
library("tidyr")
library("forcats")
library("multcomp")
library("multcompView")
library("performance")

# Read in files (ghmi and dynamic world as well)
final_data_for_analysis <- readRDS("Data/AVONET/final_data_for_analysis.RDS")
ghmi <- read_csv("Data/GHMI_Dynamic_World/mean_GHMI_parks.csv")
dynamic_world <- read_csv("Data/GHMI_Dynamic_World/DynamicWorld.csv")

# Cleaning up data and joining everything together
# Clean GHMI: mean GHMI per park
ghmi_clean <- ghmi %>%
  group_by(Park_Addre) %>%
  summarise(ghmi_mean = mean(mean, na.rm = TRUE), .groups = "drop")

# Clean Dynamic World: keep most common dominant_class per park
dynamic_world_clean <- dynamic_world %>%
  group_by(Park_Addre, dominant_class) %>%
  tally() %>%
  slice_max(order_by = n, n = 1, with_ties = FALSE) %>%
  ungroup()

# Join cleaned GHMI & Dynamic World with main data
gee_final_data_for_analysis <- final_data_for_analysis %>%
  left_join(dynamic_world_clean, by = "Park_Addre") %>%
  left_join(ghmi_clean, by = "Park_Addre")

# Convert dominant_class to factor
gee_final_data_for_analysis <- gee_final_data_for_analysis %>%
  mutate(dominant_class = as.factor(dominant_class))

# Reorder Season factor consistently for plotting
gee_final_data_for_analysis <- gee_final_data_for_analysis %>%
  mutate(
    Season = factor(
      Season,
      levels = c("Overwintering", "Spring Migration", "Breeding", "Fall Migration"),
      ordered = TRUE
    ),
    analysis = factor(analysis, levels = c("residential", "migratory", "total"))
  )

##################
## Overdispersion
#################
# Fit Poisson GLM
glm_model <- glm(species_richness ~ ghmi_mean + dominant_class + log(number_of_checklists), 
                 data = gee_final_data_for_analysis, family = poisson())

# Summary of the model
summary(glm_model)
## check model and dispersion
check_model(glm_model)
dispersion <- sum(residuals(glm_model, type = "pearson")^2) / df.residual(glm_model)
print(paste("Dispersion:", dispersion)) #9.65331189171402

#########################################
# GHMI MODELS
#########################################
# NB GLMM: GHMI × Season + log10(number_of_checklists)
nb_glmm_ghmi_season <- glmmTMB(
  species_richness ~ ghmi_mean * Season + log10(number_of_checklists),
  data = gee_final_data_for_analysis,
  family = nbinom2
)

# Summary
summary(nb_glmm_ghmi_season)
Anova(nb_glmm_ghmi_season, type = "III")

# Marginal slopes of GHMI per Season (emtrends)
emm_ghmi_season <- emtrends(
  nb_glmm_ghmi_season,
  var = "ghmi_mean",
  specs = ~ Season,
  type = "response"
)
emm_ghmi_season_df <- as.data.frame(emm_ghmi_season)
print(emm_ghmi_season_df)

# Plot Marginal slope of GHMI
## quick visualization for slopes
ggplot(emm_ghmi_season_df, aes(x = Season, y = ghmi_mean.trend, group = Season, colour = "#006400")) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.6) +
  labs(y = "Marginal Slope of GHMI", x = NULL) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.ticks = element_line(color = "black"),
    legend.position = "none",
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black")
  ) +
  coord_flip() +
  theme(aspect.ratio = 0.5)

#########################################
# GHMI + AREA MODELS (include Shape_Area)
#########################################
# GLMM: log10(Shape_Area) + Season * ghmi_mean * analysis + log10(number_of_checklists)
glmm_season_size_ghmi <- glmmTMB(
  species_richness ~ log10(Shape_Area) + Season * ghmi_mean * analysis + log10(number_of_checklists),
  data = gee_final_data_for_analysis,
  family = nbinom2
)

# Filter dataset to migratory & residential for analysis
gee_filtered <- gee_final_data_for_analysis %>%
  filter(analysis %in% c("migratory", "residential"))

# NB GLMM: GHMI * Season * analysis + log10(number_of_checklists)
nb_glmm_ghmi_season_analysis <- glmmTMB(
  species_richness ~ ghmi_mean * Season * analysis + log10(number_of_checklists),
  data = gee_filtered,
  family = nbinom2
)

# Summary & ANOVA
summary(glmm_season_size_ghmi)
Anova(glmm_season_size_ghmi, type = "III")

# Marginal slopes for GHMI by Season × analysis
emm_ghmi_trends <- emtrends(
  nb_glmm_ghmi_season_analysis,
  var = "ghmi_mean",
  specs = ~ Season * analysis,
  type = "response"
)

# Convert to data frame, add significance marker & y positions
emm_ghmi_df <- as.data.frame(emm_ghmi_trends) %>%
  mutate(
    signif = ifelse(asymp.LCL > 0 | asymp.UCL < 0, "*", ""),
    y_position = ghmi_mean.trend + 0.5
  )

# GHMI prediction sequence
ghmi_seq <- seq(
  quantile(gee_final_data_for_analysis$ghmi_mean, 0.05, na.rm = TRUE),
  quantile(gee_final_data_for_analysis$ghmi_mean, 0.95, na.rm = TRUE),
  length.out = 50
)

# Prediction grid for Size × GHMI (hold checklists at median)
pred_grid <- list(
  Shape_Area = seq(
    min(gee_final_data_for_analysis$Shape_Area, na.rm = TRUE),
    max(gee_final_data_for_analysis$Shape_Area, na.rm = TRUE),
    length.out = 40
  ),
  ghmi_mean = seq(
    min(gee_final_data_for_analysis$ghmi_mean, na.rm = TRUE),
    max(gee_final_data_for_analysis$ghmi_mean, na.rm = TRUE),
    length.out = 4
  ),
  number_of_checklists = median(gee_final_data_for_analysis$number_of_checklists, na.rm = TRUE)
)

# Predicted response surface for the Area + GHMI model
emm_size_ghmi <- emmeans(
  glmm_season_size_ghmi,
  ~ log10(Shape_Area) * ghmi_mean | Season * analysis,
  at = pred_grid,
  type = "response"
)
emm_size_ghmi_df <- as.data.frame(emm_size_ghmi)

# GHMI response curves (holding checklists at median)
ghmi_predicted_response_area <- emmip(
  glmm_season_size_ghmi,
  Season ~ ghmi_mean,
  type = "response",
  CIs = TRUE,
  at = list(
    ghmi_mean = ghmi_seq,
    number_of_checklists = median(gee_final_data_for_analysis$number_of_checklists, na.rm = TRUE)
  )
) +
  scale_color_manual(values = c(
    "Overwintering"    = "#006400", 
    "Spring Migration" = "#FF8C00",
    "Breeding"         = "#1E90FF", 
    "Fall Migration"   = "#800080"
  )) +
  labs(
    x = "GHMI (Global Human Modification Index)",
    y = "Predicted Species Richness",
    colour = "Season"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(color = "black", size = 12),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 12)
  )

ghmi_predicted_response_area

ggsave("Figures/ghmi_predicted_response_area.png", ghmi_predicted_response_area, bg = "transparent")

#########################################
# GHMI × SEASON × ANALYSIS 
#########################################
# Filter dataset to migratory & residential for analysis
gee_filtered <- gee_final_data_for_analysis %>%
  filter(analysis %in% c("migratory", "residential"))

# NB GLMM: GHMI * Season * analysis + log10(number_of_checklists) + log10(Shape_Area)
nb_glmm_ghmi_season_analysis <- glmmTMB(
  species_richness ~ ghmi_mean * Season * analysis + log10(number_of_checklists) + log10(Shape_Area),
  data = gee_filtered,
  family = nbinom2
)

# Summary & ANOVA
summary(nb_glmm_ghmi_season_analysis)
Anova(nb_glmm_ghmi_season_analysis, type = "III")

# Marginal slopes for GHMI by Season × analysis
emm_ghmi_trends <- emtrends(
  nb_glmm_ghmi_season_analysis,
  var = "ghmi_mean",
  specs = ~ Season * analysis,
  type = "response"
)

# Convert to data frame and add significance marker & y positions
emm_ghmi_df <- as.data.frame(emm_ghmi_trends) %>%
  mutate(
    signif = ifelse(asymp.LCL > 0 | asymp.UCL < 0, "*", ""),
    y_position = ghmi_mean.trend + 0.5
  )

# Create predicted response curve for plotting by analysis (use previously defined ghmi_seq)
ghmi_predicted_response_analysis <- emmip(
  nb_glmm_ghmi_season_analysis,
  Season ~ ghmi_mean | analysis,
  type = "response",
  CIs = TRUE,
  at = list(
    ghmi_mean = ghmi_seq,
    number_of_checklists = median(gee_filtered$number_of_checklists, na.rm = TRUE)
  )
)

# GHMI × SEASON × ANALYSIS Plot (Figure 3)
ghmi_plot <- ghmi_predicted_response_analysis +
  labs(
    x = "GHMI (Global Human Modification Index)",
    y = "Predicted Species Richness",
    colour = "Season",
    linetype = "Analysis"
  ) +
  scale_color_manual(values = c(
    "Overwintering"    = "#006400", 
    "Spring Migration" = "#FF8C00",
    "Breeding"         = "#1E90FF", 
    "Fall Migration"   = "#800080"  
  )) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(color = "black", linewidth = 0.5),
    axis.title = element_text(face = "bold", size = 14),
    axis.text = element_text(color = "black", size = 12),
    strip.text = element_text(face = "bold", size = 14),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 12)
  ) +
  guides(
    colour = guide_legend(override.aes = list(linetype = 1, shape = NA, alpha = 1)),
    linetype = guide_legend(override.aes = list(size = 1))
  )

ghmi_plot

# Save as png
ggsave("Figures/figure_3_ghmi_predicted_response_migratory_residential_sig.png", ghmi_plot, bg = "transparent", width = 8, height = 5)

# Get slopes and p values
ghmi_slopes_analysis <- emtrends(
  nb_glmm_ghmi_season_analysis,
  specs = c("analysis", "Season"),
  var = "ghmi_mean",
  type = "response"
)

# Convert to data frame for easier reading
ghmi_slopes_df <- as.data.frame(ghmi_slopes_analysis) %>%
  rename(slope = ghmi_mean.trend) %>%
  dplyr::select(any_of(c("analysis", "Season", "slope", "SE", "df", "t.ratio", "p.value")))

# View slopes and p-values
ghmi_slopes_df

# Pairwise comparisons of slopes
pairs(ghmi_slopes_analysis)


#########################################
# DYNAMIC WORLD: dominant_class × Season 
#########################################
# NB GLMM: dominant_class * Season + log10(number_of_checklists) + log10(Shape_Area)
nb_glmm_domclass_season <- glmmTMB(
  species_richness ~ dominant_class * Season + log10(number_of_checklists) + log10(Shape_Area),
  data = gee_final_data_for_analysis,
  family = nbinom2
)

# Summary & ANOVA
summary(nb_glmm_domclass_season)
Anova(nb_glmm_domclass_season, type = "III")

# Get emmeans for dominant_class by Season (predicted means)
emm_domclass_season <- emmeans(
  nb_glmm_domclass_season,
  ~ dominant_class | Season,
  type = "response"
) %>% as.data.frame()

# Recode numeric dominant_class to descriptive labels & reorder
plot_df_dw <- emm_domclass_season %>%
  mutate(
    dominant_class = as.numeric(dominant_class),
    dominant_label = factor(
      dominant_class,
      levels = c(0,1,2,3,4,5,6),
      labels = c("Water","Trees","Grass","Flooded Vegetation","Crops","Shrub / Scrub","Built Area")
    ),
    dominant_label = forcats::fct_reorder(dominant_label, response, .fun = mean)
  ) %>%
  drop_na(dominant_label)


# Publication-ready bar plot of predicted richness by dominant class × Season
ggplot(plot_df_dw, aes(x = dominant_label, y = response, colour = Season, fill = Season)) +
  geom_col(position = position_dodge(width = 0.9), alpha = 1) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                position = position_dodge(width = 0.8),
                width = 0.25,
                colour = "black") +
  labs(x = "Dynamic World Dominant Class", y = "Predicted Species Richness", colour = "Season", fill = "Season") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold", size = 12),
    axis.text.x = element_text(color = "black", angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(color = "black", size = 12),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 12)
  ) +
  scale_color_manual(values = c(
    "Overwintering"    = "#006400", 
    "Spring Migration" = "#FF8C00",
    "Breeding"         = "#1E90FF", 
    "Fall Migration"   = "#800080"  
  )) +
  scale_fill_manual(values = c(
    "Overwintering"    = "#006400", 
    "Spring Migration" = "#FF8C00",
    "Breeding"         = "#1E90FF", 
    "Fall Migration"   = "#800080"  
  ))


#########################################
# DYNAMIC WORLD: Full NB model with area, analysis, Season, dominant_class
#########################################
# NB GLMM: log10(Shape_Area) + analysis * Season * dominant_class + log10(number_of_checklists)
nb_model_dw <- glmmTMB(
  species_richness ~ log10(Shape_Area) + analysis * Season * dominant_class + log10(number_of_checklists),
  data = gee_filtered,
  family = nbinom2
)
summary(nb_model_dw)
Anova(nb_model_dw, type = "III")

# emmeans for dominant_class by Season * analysis (for plotting facets)
emm_plot_obj <- emmeans(
  nb_model_dw,
  ~ dominant_class | Season * analysis,
  type = "response"
)

plot_df <- as.data.frame(emm_plot_obj) %>%
  tibble::as_tibble() %>%
  mutate(
    dominant_class = as.numeric(as.character(dominant_class)),
    dominant_label = factor(
      dominant_class,
      levels = c(0,1,2,3,4,5,6),
      labels = c("Water","Trees","Grass","Flooded Vegetation","Crops","Shrub / Scrub","Built Area")
    ),
    dominant_label = fct_reorder(dominant_label, response, .fun = mean)
  )

# Pairwise contrasts for Seasons within each dominant_class × analysis (Tukey)
emm_comp_obj <- emmeans(
  nb_model_dw,
  ~ Season | dominant_class * analysis,
  type = "response"
)
emm_comp_pairs <- contrast(emm_comp_obj, method = "pairwise", adjust = "tukey")

# Compute CLD letters for significance display
cld_df <- cld(
  emm_comp_obj,
  alpha = 0.05,
  adjust = "tukey",
  Letters = letters,
  sort = FALSE
) %>%
  as.data.frame() %>%
  mutate(
    .group = gsub(" ", "", .group),
    dominant_class = as.numeric(as.character(dominant_class))
  )

# Merge CLD letters into plotting dataframe
plot_df2 <- plot_df %>%
  left_join(
    cld_df %>% dplyr::select(Season, dominant_class, analysis, .group),
    by = c("Season", "dominant_class", "analysis")
  ) %>%
  mutate(
    analysis = as.character(analysis),
    analysis = dplyr::recode(analysis,
                      "migratory" = "Migratory",
                      "residential" = "Residential"),
    letter = ifelse(is.na(.group), "", .group)
  ) %>%
  drop_na(dominant_label)

# Compute y-offsets for CLD text placement
plot_df2 <- plot_df2 %>%
  group_by(analysis, dominant_class) %>%
  mutate(
    y_offset = 0.05 * (max(asymp.UCL, na.rm = TRUE) - min(response, na.rm = TRUE)),
    y_label = asymp.UCL + ifelse(is.finite(y_offset), y_offset, 0.5)
  ) %>%
  ungroup()

# Ensure dominant_label factor is alphabetically sorted (as requested previously)
plot_df2 <- plot_df2 %>%
  mutate(dominant_label = factor(dominant_label, levels = sort(unique(dominant_label))))

# Final faceted plot: dominant class × Season, faceted by analysis, with CLD letters
ggplot(plot_df2, aes(x = dominant_label, y = response, fill = Season, colour = Season)) +
  geom_col(position = position_dodge(width = 0.9), alpha = 1) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                position = position_dodge(width = 0.9),
                width = 0.25,
                colour = "black") +
  facet_wrap(~ analysis, ncol = 1, scales = "free_y") +
  labs(x = NULL, y = "Predicted Species Richness", fill = "Season", colour = "Season") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 13),
    axis.text.x = element_text(color = "black", angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(color = "black", size = 11),
    strip.text = element_text(size = 13),
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12)
  ) +
  scale_color_manual(values = c(
    "Overwintering"    = "#006400", 
    "Spring Migration" = "#FF8C00",
    "Breeding"         = "#1E90FF", 
    "Fall Migration"   = "#800080"  
  )) +
  scale_fill_manual(values = c(
    "Overwintering"    = "#006400", 
    "Spring Migration" = "#FF8C00",
    "Breeding"         = "#1E90FF", 
    "Fall Migration"   = "#800080"  
  ))

ggsave("Figures/dominant_class_significance_analysis.png", bg = "transparent", width = 10, height = 8)

# Summarize mean predicted richness and group letters
richness_summary <- plot_df2 %>%
  group_by(analysis, dominant_class, Season) %>%
  summarise(
    mean_predicted = mean(response, na.rm = TRUE),
    lower_CI = mean(asymp.LCL, na.rm = TRUE),
    upper_CI = mean(asymp.UCL, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(analysis, dominant_class, Season)

richness_summary


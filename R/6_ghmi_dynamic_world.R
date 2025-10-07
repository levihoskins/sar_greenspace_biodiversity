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

# Read in files (ghmi and dynamic world as well)
final_data_for_analysis <- readRDS("Data/AVONET/final_data_for_analysis.RDS")
ghmi <- read_csv("Data/GHMI_Dynamic_World/mean_GHMI_parks.csv")
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

## do the same for analysis variables
gee_final_data_for_analysis$analysis <- factor(
  gee_final_data_for_analysis$analysis,
  levels = c("residential", "migratory", "total")
)

##################
## Overdispersion
#################
# Fit Poisson GLM
glm_model <- glm(species_richness ~ ghmi_mean + dominant_class + log(number_of_checklists), 
                 data = gee_final_data_for_analysis, family = poisson())

# Summary of the model
summary(glm_model)

check_model(glm_model)

# Calculate dispersion
dispersion <- sum(residuals(glm_model, type = "pearson")^2) / df.residual(glm_model)
print(paste("Dispersion:", dispersion)) #9.12868035444644

#########################################
# NB GLMM: GHMI × Season + log10(lists)
#########################################
nb_glmm_ghmi_season <- glmmTMB(
  species_richness ~ ghmi_mean * Season + log10(number_of_checklists),
  data   = gee_final_data_for_analysis,
  family = nbinom2
)

summary(nb_glmm_ghmi_season)
Anova(nb_glmm_ghmi_season, type = "III")  

# Get marginal means for plotting GHMI effects by season
emm_ghmi_season <- emtrends(nb_glmm_ghmi_season,
                            var = "ghmi_mean",
                            specs = ~ Season,
                            type = "response")

print(emm_ghmi_season)

# Convert emtrends object to data frame
emm_ghmi_season_df <- as.data.frame(emm_ghmi_season)

# Plot
ggplot(emm_ghmi_season_df,
       aes(x = Season, y = ghmi_mean.trend, group = Season)) +
  geom_point(size = 4, color = "forestgreen") +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                width = 0.6, color = "forestgreen") +
  labs(
    title = NULL,
    y = "Marginal Slope of GHMI",
    x = NULL
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.ticks = element_line(color = "grey30"),
    legend.position = "none",
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black")
  ) +
  coord_flip() +
  theme(aspect.ratio = 0.5)

# Create a response curve (predicted richness vs GHMI for each Season)
ghmi_predicted_response <- emmip(
  nb_glmm_ghmi_season,
  Season ~ ghmi_mean,
  type = "response",
  at = list(
    ghmi_mean = seq(
      quantile(gee_final_data_for_analysis$ghmi_mean, 0.05, na.rm = TRUE),
      quantile(gee_final_data_for_analysis$ghmi_mean, 0.95, na.rm = TRUE),
      length.out = 50
    ),
    lists = median(gee_final_data_for_analysis$number_of_checklists, na.rm = TRUE)
  )
) +
  labs(
    x = "GHMI (Human Modification Index)",
    y = "Predicted Species Richness",
    colour = "Season"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(), 
    legend.position = "bottom"
  )

ghmi_predicted_response

ggsave("Figures/ghmi_predicted_response.png", ghmi_predicted_response, bg = "transparent")

#### Repeat above but add in Shape_Area as a variable in the model
glmm_season_size_ghmi <- glmmTMB(species_richness ~ log10(Shape_Area) + Season * ghmi_mean * analysis
                                 + log10(number_of_checklists), family = nbinom2, data = gee_final_data_for_analysis)
summary(glmm_season_size_ghmi)
Anova(glmm_season_size_ghmi, type = "III")

# prediction grid: vary Shape_Area & ghmi_mean, hold checklists at median
pred_grid <- with(gee_final_data_for_analysis,
                  list(
                    Shape_Area = seq(min(Shape_Area, na.rm = TRUE),
                                     max(Shape_Area, na.rm = TRUE),
                                     length.out = 40),
                    ghmi_mean = seq(min(ghmi_mean, na.rm = TRUE),
                                    max(ghmi_mean, na.rm = TRUE),
                                    length.out = 4), # fewer steps to keep plot clean
                    number_of_checklists = median(number_of_checklists, na.rm = TRUE)
                  ))

# get predicted responses
emm_size_ghmi <- emmeans(
  glmm_season_size_ghmi,
  ~ log10(Shape_Area) * ghmi_mean | Season * analysis,
  at = pred_grid,
  type = "response"
)

emm_size_ghmi_df <- as.data.frame(emm_size_ghmi)

# plot
ggplot(emm_size_ghmi_df,
       aes(x = Shape_Area / 10000, y = response, colour = ghmi_mean,
           group = interaction(ghmi_mean, analysis))) +
  geom_line(size = 1) +
  facet_wrap(~Season) +
  scale_x_log10() +
  scale_colour_viridis_c(option = "plasma") +
  labs(
    x = "Park Area (hectares, log scale)",
    y = "Predicted Species Richness",
    colour = "GHMI",
    linetype = "Analysis"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA)
  )

#########################################
# NB GLMM: dominant_class × Season + log10(number_of_checklists)
#########################################
nb_glmm_domclass_season <- glmmTMB(
  species_richness ~ dominant_class * Season + log10(number_of_checklists),
  data   = gee_final_data_for_analysis,
  family = nbinom2
)

summary(nb_glmm_domclass_season)
Anova(nb_glmm_domclass_season, type = "III")  

# Get emmeans and convert to a data frame
emm_domclass_season <- emmeans(
  nb_glmm_domclass_season,
  ~ dominant_class | Season,
  type = "response"
) %>% 
  as.data.frame()

# Recode numeric dominant_class to Dynamic World labels
plot_df <- emm_domclass_season %>%
  mutate(
    dominant_class = as.numeric(dominant_class),
    dominant_label = factor(
      dominant_class,
      levels = c(0, 1, 2, 3, 4, 5, 6),
      labels = c(
        "Water",
        "Trees",
        "Grass",
        "Flooded Vegetation",
        "Crops",
        "Shrub / Scrub",
        "Built Area"
      )
    )
  ) %>%
  mutate(
    dominant_label = forcats::fct_reorder(dominant_label, response, .fun = mean)
  ) %>%
  drop_na() 

# Plot with asymptotic CIs and separate curves by season
ggplot(plot_df,
       aes(x = dominant_label, y = response, colour = Season, group = Season, fill = Season
       )) +
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL), alpha = 0.12, colour = NA) +
  geom_line(size = 0.7) +
  geom_point(size = 2) +
  labs(
    x = "Dynamic World Dominant Class",
    y = "Predicted Species Richness",
    title = NULL,
    colour = "Season",
    fill   = "Season"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(color = "black", angle = 45, hjust = 1),
    axis.text.y = element_text(color = "black"),
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

ggsave("Figures/dominant_class_predicted_richness_emmeans.png", bg = "transparent")




###Negative Binomial
### dynamic world
### with everythin else
nb_model_dw <- glmmTMB(species_richness ~ log10(Shape_Area) + analysis * Season * dominant_class + log10(number_of_checklists),
                         data = gee_final_data_for_analysis,
                         family = nbinom2)
summary(nb_model_dw)
Anova(nb_model_dw, type = "III")

# Get emmeans and convert to a data frame
emm_domclass_season <- emmeans(
  nb_model_dw,
  ~ dominant_class | Season,
  type = "response"
) %>% 
  as.data.frame()

# Recode numeric dominant_class to Dynamic World labels
plot_df <- emm_domclass_season %>%
  mutate(
    dominant_class = as.numeric(dominant_class),
    dominant_label = factor(
      dominant_class,
      levels = c(0, 1, 2, 3, 4, 5, 6),
      labels = c(
        "Water",
        "Trees",
        "Grass",
        "Flooded Vegetation",
        "Crops",
        "Shrub / Scrub",
        "Built Area"
      )
    )
  ) %>%
  mutate(
    dominant_label = forcats::fct_reorder(dominant_label, response, .fun = mean)
  ) %>%
  drop_na()

# Plot with asymptotic CIs and separate curves by season
ggplot(plot_df,
       aes(x = dominant_label, y = response, colour = Season, group = Season, fill = Season
       )) +
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL), alpha = 0.12, colour = NA) +
  geom_line(size = 0.7) +
  geom_point(size = 2) +
  labs(
    x = "Dynamic World Dominant Class",
    y = "Predicted Species Richness",
    title = NULL,
    colour = "Season",
    fill   = "Season"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(color = "black", angle = 45, hjust = 1),
    axis.text.y = element_text(color = "black"),
    legend.position = "bottom"
  )

ggsave("Figures/dominant_class_predicted_richness_emmeans.png", bg = "transparent")



#visualize ghmi mean vs species richness
ggplot(gee_final_data_for_analysis, aes(x = ghmi_mean, y = species_richness)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", method.args = list(family = "poisson"), se = TRUE) +
  theme_minimal() +
  labs(title = "Species Richness vs GHMI", x = "GHMI (Human Modification Index)", y = "Species Richness")






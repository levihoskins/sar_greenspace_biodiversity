### BIG MODEL

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

## Load files
greenspaces <- readRDS("Data/final_data_for_big_script.RDS")

# Run NB GLMM
glmm_season_migration <- glmmTMB(
  species_richness ~ log1p(nearest_dist_km) * Season * analysis + log(number_of_checklists) +
    log1p(Park_Size_) + (1 | Park_Addre),
  data = greenspaces,
  family = nbinom2
)
summary(glmm_season_migration)


glmm_season_size_ghmi <- glmmTMB(
  species_richness ~ (Park_Size_ + ghmi_mean + log1p(nearest_dist_km)) * Season * analysis
  + log10(number_of_checklists),
  family = nbinom2,
  data = greenspaces
)
summary(glmm_season_size_ghmi)
Anova(glmm_season_size_ghmi, type = "III")

# Marginal trends of GHMI by Season
emm_ghmi <- emtrends(glmm_season_size_ghmi, var = "ghmi_mean", specs = ~ Season)
emm_ghmi_df <- as.data.frame(emm_ghmi)

ggplot(emm_ghmi_df, aes(x = Season, y = ghmi_mean.trend, group = Season)) +
  geom_point(size = 4, color = "forestgreen") +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.3, color = "forestgreen") +
  labs(y = "Marginal slope of GHMI", x = NULL) +
  theme_minimal() +
  coord_flip()

# Predicted species richness across Park_Size_ values for each Season
emm_park <- emmeans(glmm_season_size_ghmi, ~ Park_Size_ | Season, at = list(Park_Size_ = seq(
  min(greenspaces$Park_Size_, na.rm = TRUE),
  max(greenspaces$Park_Size_, na.rm = TRUE),
  length.out = 50
)), type = "response")

emm_park_df <- as.data.frame(emm_park)
ggplot(emm_park_df, aes(x = Park_Size_, y = response, color = Season)) +   
  geom_smooth(method = "lm", se = TRUE) +       
  scale_x_continuous(trans = "log10") +
  labs(x = "Park Size (log scale)", y = "Predicted Species Richness") +
  theme_minimal()

## marginal effect nearest distance
emm_dist <- emmeans(glmm_season_size_ghmi, ~ log1p(nearest_dist_km) | Season, 
                    at = list(nearest_dist_km = seq(0, max(greenspaces$nearest_dist_km, na.rm=TRUE), length.out=50)),
                    type = "response")

emm_dist_df <- as.data.frame(emm_dist)

ggplot(emm_dist_df, aes(x = nearest_dist_km, y = response, color = Season)) +
  geom_smooth(method = "lm", se = TRUE) +   
  labs(x = "Log(1 + nearest distance km)", y = "Predicted Species Richness") +
  theme_minimal()





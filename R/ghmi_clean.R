### Cleaned GHMI script

#---------------------------------------
# Load packages
#---------------------------------------
library(sf)
library(dplyr)
library(glmmTMB)
library(emmeans)
library(ggplot2)
library(lme4)
library(readr)
library(MASS)
library(ggeffects)
library(patchwork)
library(broom)
library(car)

# Read file
final_avonet <- readRDS("Data/AVONET/final_avonet.RDS")

# Read and clean GHMI data
ghmi <- read_csv("Data/GHMI_Dynamic_World/GHMI.csv")

ghmi_clean <- ghmi %>%
  group_by(Park_Addre) %>%
  summarise(ghmi_mean = mean(mean, na.rm = TRUE), .groups = "drop")

#---------------------------------------
# Join GHMI with AVONET
#---------------------------------------
final_avonet_ghmi <- final_avonet %>%
  left_join(ghmi_clean, by = "Park_Addre")

#---------------------------------------
# Fit Negative Binomial GLMM: GHMI × Season
#---------------------------------------
nb_glmm_ghmi_season <- glmmTMB(
  species_richness ~ ghmi_mean * Season + log10(number_of_checklists),
  data = gee_final_data_for_analysis,
  family = nbinom2
)

summary(nb_glmm_ghmi_season)
Anova(nb_glmm_ghmi_season, type = "III")

#---------------------------------------
# Marginal slopes of GHMI by season
#---------------------------------------
med_lists <- median(gee_final_data_for_analysis$number_of_checklists, na.rm = TRUE)

emm_ghmi_season <- emtrends(
  nb_glmm_ghmi_season,
  var = "ghmi_mean",
  specs = ~ Season,
  type = "response",
  at = list(lists = med_lists)
)

emm_ghmi_season_df <- as.data.frame(emm_ghmi_season)

#---------------------------------------
# Plot slopes
#---------------------------------------
p1 <- ggplot(emm_ghmi_season_df, aes(x = Season, y = ghmi_mean.trend)) +
  geom_point(size = 4, color = "forestgreen") +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                width = 0.6, color = "forestgreen") +
  labs(title = "Slope of GHMI Effect", y = "Marginal Slope of GHMI", x = NULL) +
  theme_minimal() +
  coord_flip() +
  theme(
    panel.grid.major = element_blank(),
    axis.ticks = element_line(color = "grey30"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black")
  )

#---------------------------------------
# Plot predicted richness vs GHMI by season
#---------------------------------------
p2 <- emmip(
  nb_glmm_ghmi_season,
  Season ~ ghmi_mean,
  type = "response",
  at = list(
    ghmi_mean = seq(
      quantile(gee_final_data_for_analysis$ghmi_mean, 0.05, na.rm = TRUE),
      quantile(gee_final_data_for_analysis$ghmi_mean, 0.95, na.rm = TRUE),
      length.out = 50
    ),
    lists = med_lists
  )
) +
  labs(x = "GHMI (Human Modification Index)", y = "Predicted Species Richness", colour = "Season") +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(), legend.position = "top")

# Combine side-by-side
p1 + p2 + plot_annotation(
  title = "GHMI Effects on Species Richness by Season",
  subtitle = "Left: slopes | Right: predicted richness"
)

#---------------------------------------
# GLMM: GHMI × Park Size × Season × Migration
#---------------------------------------
glmm_season_size_ghmi <- glmmTMB(
  species_richness ~ Park_Size_ * Season * ghmi_mean * analysis + log10(number_of_checklists),
  family = nbinom2,
  data = gee_final_data_for_analysis
)

summary(glmm_season_size_ghmi)
Anova(glmm_season_size_ghmi, type = "III")



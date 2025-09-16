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
# Add migration status and reorder factors
#---------------------------------------
migratory_residential <- final_avonet_ghmi %>%
  mutate(
    migration_status = case_when(
      Migration == 1 ~ "residential",
      Migration %in% c(2, 3) ~ "migratory",
      TRUE ~ NA_character_
    ),
    Season = factor(Season,
                    levels = c("Overwintering", "Spring Migration", "Breeding", "Fall Migration"),
                    ordered = TRUE),
    migration_status = factor(migration_status, levels = c("residential", "migratory"))
  )

migratory_residential_singular <- migratory_residential %>% 
  dplyr::select(-geometry) %>%
  dplyr::select(species_richness, Season, Park_Size_, lists, migration_status) %>% 
  distinct()

#---------------------------------------
# Fit Negative Binomial GLMM: GHMI × Season
#---------------------------------------
nb_glmm_ghmi_season <- glmmTMB(
  species_richness ~ ghmi_mean * Season + log10(lists),
  data = migratory_residential_singular,
  family = nbinom2
)

summary(nb_glmm_ghmi_season)
Anova(nb_glmm_ghmi_season, type = "III")

#---------------------------------------
# Marginal slopes of GHMI by season
#---------------------------------------
med_lists <- median(migratory_residential_singular$lists, na.rm = TRUE)

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
      quantile(migratory_residential_singular$ghmi_mean, 0.05, na.rm = TRUE),
      quantile(migratory_residential_singular$ghmi_mean, 0.95, na.rm = TRUE),
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
  species_richness ~ Park_Size_ * Season * ghmi_mean * migration_status + log10(lists),
  family = nbinom2,
  data = migratory_residential_singular
)

summary(glmm_season_size_ghmi)
Anova(glmm_season_size_ghmi, type = "III")

#---------------------------------------
# Tidy model for plotting effect sizes
#---------------------------------------
species_mod_summary <- tidy(glmm_season_size_ghmi, conf.int = TRUE) %>%
  filter(effect == "fixed") %>%
  mutate(
    predictor = term,
    interaction_order = str_count(term, ":") + 1,
    Scale = case_when(
      grepl("Park_Size_", predictor) ~ "Local",
      grepl("ghmi", predictor) ~ "Landscape",
      grepl("Season|migration_status|lists", predictor) ~ "Other",
      TRUE ~ NA_character_
    ),
    model = "GHMI + Park Size + Season"
  ) %>%
  filter(!predictor %in% c("(Intercept)", "log10(lists)")) %>%
  arrange(interaction_order, predictor) %>%
  mutate(predictor = factor(predictor, levels = predictor))

# Plot effect sizes
ggplot(species_mod_summary, aes(x = predictor, y = estimate, color = Scale)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.4) +
  coord_flip() +
  theme_bw(base_size = 12) +
  scale_color_brewer(palette = "Dark2") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  ylab("Effect size") +
  xlab("") +
  theme(axis.text = element_text(color = "black"))

#---------------------------------------
# Predicted GHMI effects at median park size
#---------------------------------------
mean_ghmi <- mean(migratory_residential_singular$ghmi_mean, na.rm = TRUE)
park_vals <- quantile(migratory_residential_singular$Park_Size_, probs = c(0.1, 0.5, 0.9), na.rm = TRUE)
ghmi_vals <- quantile(migratory_residential_singular$ghmi_mean, probs = c(0.1, 0.5, 0.9), na.rm = TRUE)

nb_model_ghmi <- glmmTMB(
  species_richness ~ log10(Park_Size_) * migration_status * Season * ghmi_mean + log10(lists),
  data = migratory_residential_singular,
  family = nbinom2
)

emmip(
  nb_model_ghmi,
  migration_status ~ ghmi_mean | Season,
  by = "migration_status",
  type = "response",
  at = list(
    ghmi_mean = seq(min(migratory_residential$ghmi_mean, na.rm = TRUE),
  
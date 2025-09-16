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

# Read file
final_avonet <- readRDS("Data/AVONET/final_avonet.RDS")

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
final_avonet_gee <- final_avonet %>%
  left_join(dynamic_world_clean, by = "Park_Addre") %>%
  left_join(ghmi_clean, by = "Park_Addre")

# Convert dominant_class to factor
final_avonet_gee$dominant_class <- as.factor(final_avonet_gee$dominant_class)

# Add migration status
migratory_residential <- final_avonet_gee %>%
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

migratory_residential$migration_status <- factor(
  migratory_residential$migration_status,
  levels = c("residential", "migratory")
)

##################
## Overdispersion
#################
# Fit Poisson GLM
#glm_model <- glm(species_richness ~ ghmi_mean + dominant_class + lists, 
#                 data = final_avonet_gee, family = poisson())

# Summary of the model
#summary(glm_model)

# Calculate dispersion
#dispersion <- sum(residuals(glm_model, type = "pearson")^2) / df.residual(glm_model)
#print(paste("Dispersion:", dispersion))

#########################################
# NB GLMM: GHMI × Season + log10(lists)
#########################################
nb_glmm_ghmi_season <- glmmTMB(
  species_richness ~ ghmi_mean * Season + log10(lists),
  data   = migratory_residential,
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
emmip(
  nb_glmm_ghmi_season,
  Season ~ ghmi_mean,
  type = "response",
  at = list(
    ghmi_mean = seq(
      min(migratory_residential$ghmi_mean, na.rm = TRUE),
      max(migratory_residential$ghmi_mean, na.rm = TRUE),
      length.out = 50
    )
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
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text  = element_text(color = "black")
  )

levels(migratory_residential$Season)
contrasts(migratory_residential$Season)

emmeans(nb_glmm_ghmi_season, ~ Season)

emmip(
  nb_glmm_ghmi_season,
  Season ~ ghmi_mean,
  type = "response",
  at = list(
    ghmi_mean = seq(
      quantile(migratory_residential$ghmi_mean, 0.05, na.rm = TRUE),
      quantile(migratory_residential$ghmi_mean, 0.95, na.rm = TRUE),
      length.out = 50
    ),
    lists = median(migratory_residential$lists, na.rm = TRUE)
  )
)

#---------------------------------------
# 1. Set realistic values for predictions
#---------------------------------------
mean_ghmi  <- mean(migratory_residential$ghmi_mean, na.rm = TRUE)
med_lists  <- median(migratory_residential$lists, na.rm = TRUE)

#---------------------------------------
# 2. Slopes of GHMI by season (emtrends)
#---------------------------------------
emm_ghmi_season <- emtrends(
  nb_glmm_ghmi_season,
  var = "ghmi_mean",
  specs = ~ Season,
  type = "response",
  at = list(lists = med_lists)
)

emm_ghmi_season_df <- as.data.frame(emm_ghmi_season)

p1 <- ggplot(emm_ghmi_season_df,
             aes(x = Season, y = ghmi_mean.trend)) +
  geom_point(size = 4, color = "forestgreen") +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                width = 0.6, color = "forestgreen") +
  labs(
    title = "Slope of GHMI Effect",
    y = "Marginal Slope of GHMI",
    x = NULL
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    axis.ticks = element_line(color = "grey30"),
    axis.title = element_text(face = "bold"),
    axis.text  = element_text(color = "black")
  ) +
  coord_flip() +
  theme(aspect.ratio = 0.5)

#---------------------------------------
# 3. Predicted richness vs GHMI by season
#---------------------------------------
p2 <- emmip(
  nb_glmm_ghmi_season,
  Season ~ ghmi_mean,
  type = "response",
  at = list(
    ghmi_mean = seq(
      quantile(migratory_residential$ghmi_mean, 0.05, na.rm = TRUE),
      quantile(migratory_residential$ghmi_mean, 0.95, na.rm = TRUE),
      length.out = 50
    ),
    lists = med_lists
  )
) +
  labs(
    title = "Predicted Species Richness",
    x = "GHMI (Human Modification Index)",
    y = "Predicted Species Richness",
    colour = "Season"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text  = element_text(color = "black")
  )

#---------------------------------------
# 4. Compare side by side
#---------------------------------------
p1 + p2 + plot_annotation(
  title = "GHMI Effects on Species Richness by Season",
  subtitle = "Left: sensitivity (slopes, at median list effort) | Right: predicted richness (across observed GHMI range)"
)

#---------------------------------------
# 5. Double-check predicted marginal means
#---------------------------------------
emmeans(nb_glmm_ghmi_season, ~ Season,
        at = list(ghmi_mean = mean_ghmi,
                  lists = med_lists))


#### Repeat above but add in Park_Size_ as a variable in the model
glmm_season_size_ghmi <- glmmTMB(species_richness ~ Park_Size_ * Season * ghmi_mean * migration_status
                                 + log10(lists), family = nbinom2, data = migratory_residential)
summary(glmm_season_size_ghmi)
Anova(glmm_season_size_ghmi, type = "III")


# Tidy the new model
species_mod_summary <- tidy(glmm_season_size_ghmi, conf.int = TRUE) %>%
  filter(effect == "fixed") %>%
  # Use term names directly
  mutate(predictor = term,
         # Count ":" to determine interaction order (main effects first)
         interaction_order = str_count(term, ":") + 1,
         # Assign Scale
         Scale = case_when(
           grepl("Park_Size_", predictor) ~ "Local",
           grepl("ghmi", predictor) ~ "Landscape",
           grepl("Season|migration_status|lists", predictor) ~ "Other",
           TRUE ~ NA_character_
         ),
         model = "GHMI + Park Size + Season")

# Filter out Intercept and lists
plot_data <- species_mod_summary %>%
  filter(!predictor %in% c("(Intercept)", "log10(lists)")) %>%
  # Order predictor factor by interaction order, then alphabetically
  arrange(interaction_order, predictor) %>%
  mutate(predictor = factor(predictor, levels = predictor))


species_mod_summary %>%
  dplyr::filter(! predictor %in% c("Intercept", "log10(lists)")) %>%
  ggplot(., aes(x=predictor, y=estimate, color=Scale))+
  geom_point()+
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high))+
  coord_flip()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  ylab("Effect size")+
  xlab("")+
  geom_hline(yintercept=0, color="red", linetype="dashed")+
  scale_color_brewer(palette="Dark2")+
  ggtitle("Negative Binonmial GLMM")

# Plot
ggplot(plot_data, aes(x = predictor, y = estimate, color = Scale)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.4) +
  coord_flip() +
  theme_bw(base_size = 12) +
  scale_color_brewer(palette = "Dark2") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  ylab("Effect size") +
  xlab("") +
  theme(
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black")
  )




#---------------------------------------
# 0. Define realistic reference values
#---------------------------------------
mean_ghmi   <- mean(migratory_residential$ghmi_mean, na.rm = TRUE)
med_lists   <- median(migratory_residential$lists, na.rm = TRUE)
park_vals   <- quantile(log10(migratory_residential$Park_Size_), probs = c(0.1, 0.5, 0.9), na.rm = TRUE)
ghmi_vals   <- quantile(migratory_residential$ghmi_mean, probs = c(0.1, 0.5, 0.9), na.rm = TRUE)

#---------------------------------------
# Dominant class × Season
#---------------------------------------
nb_glmm_domclass_season <- glmmTMB(
  species_richness ~ dominant_class * Season + log10(lists),
  data   = migratory_residential,
  family = nbinom2
)

summary(nb_glmm_domclass_season)
Anova(nb_glmm_domclass_season, type = "III")

emm_domclass_season <- emmeans(
  nb_glmm_domclass_season,
  ~ dominant_class | Season,
  type = "response",
  at = list(lists = med_lists)
) %>% as.data.frame()

# Recode dominant_class into Dynamic World labels
emm_domclass_season %>%
  mutate(
    dominant_class = as.numeric(dominant_class),
    dominant_label = factor(
      dominant_class,
      levels = 0:6,
      labels = c("Water","Trees","Grass","Flooded Veg.","Crops","Shrub/Scrub","Built Area")
    )
  )

ggplot(plot_df, aes(x = dominant_label, y = response, colour = Season, group = Season)) +
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL, fill = Season),
              alpha = 0.15, colour = NA) +
  geom_line(size = 0.7) +
  geom_point(size = 2) +
  labs(
    x = "Dynamic World Dominant Class",
    y = "Predicted Species Richness",
    colour = "Season", fill = "Season"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.title = element_text(face = "bold"),
    legend.position = "top",
    legend.title = element_text(face = "bold")
  )





#########################################
# NB GLMM: dominant_class × Season + log10(lists)
#########################################
nb_glmm_domclass_season <- glmmTMB(
  species_richness ~ dominant_class * Season + log10(lists),
  data   = migratory_residential,
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
  )

# Plot with asymptotic CIs and separate curves by season
ggplot(plot_df,
       aes(
         x     = dominant_label,
         y     = response,
         colour = Season,
         group  = Season,
         fill   = Season
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
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(color = "black", angle = 45, hjust = 1),
    axis.text.y = element_text(color = "black"),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )




###Negative Binomial
### GHMI
nb_model_ghmi <- glmmTMB(species_richness ~ log10(Park_Size_) * migration_status * Season * ghmi_mean + log10(lists),
                              data = migratory_residential,
                              family = nbinom2)
summary(nb_model_ghmi)
Anova(nb_model_ghmi, type = "III")

# Choose 3 representative park sizes (change these values if desired):
park_vals <- quantile(migratory_residential$log10_Park_Size_, probs = c(0.1, 0.5, 0.9), na.rm = TRUE)

# Richness vs ghmi, faceted by Season, colored by migration_status,
emmip(
  nb_model_ghmi,
  migration_status ~ ghmi_mean | Season,
  by = "migration_status",
  type = "response",
  at = list(
    ghmi_mean = seq(min(migratory_residential$ghmi_mean, na.rm = TRUE),
                    max(migratory_residential$ghmi_mean, na.rm = TRUE),
                    length.out = 50),
    log10_Park_Size_ = park_vals
  )
) +
  facet_wrap(~ Season) +
  labs(
    x = "GHMI (human modification)",
    y = "Predicted Species Richness",
    colour = "Migration status",
    title = "Effect of GHMI at different Park Sizes (small, medium, large)"
  ) +
  theme_bw()

### Richness vs park size, colored by low / medium / high ghmi, faceted by Season
emmip(
  nb_model_ghmi,
  migration_status ~ log10(Park_Size_) | Season,
  by = "ghmi_mean",
  type = "response",
  at = list(
    `log10(Park_Size_)` = seq(min(log10(migratory_residential$Park_Size_), na.rm = TRUE),
                              max(log10(migratory_residential$Park_Size_), na.rm = TRUE),
                              length.out = 50),
    ghmi_mean = ghmi_vals
  )
) +
  facet_wrap(~ Season) +
  labs(
    x = "log10(Park Size)",
    y = "Predicted Species Richness",
    colour = "GHMI (low, med, high)",
    title = "Effect of Park Size at different levels of Human Modification"
  ) +
  theme_bw()

###Negative Binomial
### dynamic world
nb_model_dw <- glmmTMB(species_richness ~ log10(Park_Size_) * migration_status * Season * dominant_class + log10(lists),
                         data = migratory_residential,
                         family = nbinom2)
summary(nb_model_dw)
Anova(nb_model_dw, type = "III")

emmip(
  nb_model_dw,
  ~ log10(Park_Size_) | Season,
  by = "dominant_class",
  type = "response",
  at = list(
    `log10(Park_Size_)` = seq(
      min(log10(migratory_residential$Park_Size_), na.rm = TRUE),
      max(log10(migratory_residential$Park_Size_), na.rm = TRUE),
      length.out = 50
    )
  )
) +
  ggplot2::labs(
    x = "log10(Park Size)",
    y = "Predicted Species Richness",
    colour = "Dominant Class"
  ) +
  ggplot2::theme_bw()

nb_model <- glm.nb(species_richness ~ ghmi_mean + dominant_class + lists, data = final_avonet_gee)
summary(nb_model)

#visualize ghmi mean vs species richness
ggplot(final_avonet_gee, aes(x = ghmi_mean, y = species_richness)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "glm", method.args = list(family = "poisson"), se = TRUE) +
  theme_minimal() +
  labs(title = "Species Richness vs GHMI", x = "GHMI (Human Modification Index)", y = "Species Richness")

# Habitat type vs species richness
ggplot(final_avonet_gee, aes(x = dominant_class, y = species_richness)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Species Richness by Habitat Type", x = "Habitat Type", y = "Species Richness")

ghmi_effect <- ggpredict(nb_model, terms = "ghmi_mean")
habitat_effect <- ggpredict(nb_model, terms = "dominant_class")
list_effect <- ggpredict(nb_model, terms = "lists")

plot(ghmi_effect) +
  labs(title = "Marginal Effect of GHMI on Species Richness",
       x = "GHMI (Global Human Modification Index)",
       y = "Predicted Species Richness") +
  theme_minimal()

plot(habitat_effect) +
  labs(title = "Marginal Effect of Habitat Type",
       x = "Dominant Habitat Class",
       y = "Predicted Species Richness") +
  theme_minimal()

plot(list_effect) +
  labs(title = "Marginal Effect of Sampling Effort (Lists)",
       x = "Number of Sampling Lists",
       y = "Predicted Species Richness") +
  theme_minimal()

plot(ghmi_effect) + plot(habitat_effect) + plot(list_effect) +
  plot_layout(ncol = 1) 




#### Adding marginal effects
glmm_env_effort <- glmmTMB(
  species_richness ~ ghmi_mean + dominant_class + log10(lists) + (1 | Park_Addre),
  data = final_avonet_gee,
  family = nbinom2
)
summary(glmm_env_effort)

## marginal effects with emmeans
### ghmi
ghmi_effect <- ggpredict(glmm_env_effort, terms = "ghmi_mean")
### dynamic_world
emm_dw_class <- emmeans(glmm_env_effort, ~ dominant_class)
dw_class_df <- as.data.frame(emm_dw_class)

## plot ghmit
plot(ghmi_effect) +
  labs(
    title = "Marginal Effect of GHMI on Species Richness",
    x = "GHMI (Global Human Modification Index)",
    y = "Predicted Species Richness"
  ) +
  theme_minimal()

ggplot(ghmi_effect, aes(x = x, y = predicted)) +
  geom_line(color = "#2c7fb8", size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.3, fill = "#2c7fb8") +
  labs(
    title = "Marginal Effect of GHMI",
    x = "GHMI (Global Human Modification Index)",
    y = "Predicted Species Richness"
  ) +
  theme_minimal()

## plot dynamic world
ggplot(dw_class_df,
       aes(x = dominant_class, y = emmean)) +
  geom_point(size = 4, color = "#2c7fb8") +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.3, color = "#2c7fb8") +
  labs(
    title = "Predicted Species Richness by Habitat Type (Dynamic World)",
    x = "Dominant Habitat Class",
    y = "Predicted Species Richness"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ghmi_plot <- ggplot(ghmi_effect, aes(x = x, y = predicted)) +
  geom_line(color = "#2c7fb8", size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.3, fill = "#2c7fb8") +
  labs(x = "GHMI", y = "Predicted Richness") +
  theme_minimal()

dw_plot <- ggplot(dw_class_df, aes(x = dominant_class, y = emmean)) +
  geom_point(size = 4, color = "#2c7fb8") +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.3, color = "#2c7fb8") +
  labs(x = "Habitat Type", y = "Predicted Richness") +
  theme_minimal()

ghmi_plot + dw_plot + plot_layout(ncol = 1)

########
# FIX THIS




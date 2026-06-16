### big model with effect sizes of each attribute of residents versus migrants per season
### separation of migratory and residential species by dataframe to show which attributes are most important
###### for each migratory status
### code for supplementary figure

### keep X

# Load packages
library(sf)
library(tidyverse)
library(glmmTMB)
library(emmeans)
library(lme4)
library(performance)
library(car)
library(DHARMa)
library(patchwork)

## Load files
greenspaces_x <- readRDS("Data/final_data_for_big_script_x.RDS")

### remove the three parks with NAs for GHMI and Isolation
greenspaces <- greenspaces_x %>% 
  drop_na()

##############################
## standardize predictors   ##
##############################
# log-transform first, then z-transform
greenspaces <- greenspaces %>%
  mutate(
    # log transformed variables
    log_area = log10(Shape_Area),
    log_isolation = log10(nearest_dist_m),
    log_effort = log10(number_of_checklists),
    
    # z-transformed variables
    z_area = as.numeric(scale(log_area)),
    z_isolation = as.numeric(scale(log_isolation)),
    z_ghmi = as.numeric(scale(ghmi_mean)),
    z_effort = as.numeric(scale(log_effort))
  )

#################
##  big model  ##
#################
# first separate data frames
# residential birds
residential_df <- greenspaces %>%
  filter(analysis == "residential")
residential_df <- residential_df %>% dplyr::select(-analysis)

# migratory birds
migratory_df <- greenspaces %>%
  filter(analysis == "migratory")
migratory_df <- migratory_df %>% dplyr::select(-analysis)

##### now create a model wiht all three greenspace attributes
## residential
combined_model_residential <- glmmTMB(
  species_richness ~ 
    (z_isolation * Season) +
    (z_ghmi * Season) +
    (z_area * Season) +
    z_effort +
    (1 | Park_Addre),
  data = residential_df,
  family = poisson(link = "log")
)

summary(combined_model_residential)
Anova(combined_model_residential, type = "III")
performance(combined_model_residential) # R^2 (cond.) = 0.735 and R^2 (marg) = 0.670

### repeat without sampling effort
combined_model_residential_sn <- glmmTMB(
  species_richness ~ 
    (z_isolation * Season) +
    (z_ghmi * Season) +
    (z_area * Season) +
    (1 | Park_Addre),
  data = residential_df,
  family = poisson(link = "log")
)

performance(combined_model_residential_sn) # R^2 (cond.) = 0.740 and R^2 (marg) = 0.221

## migratory
combined_model_migratory <- glmmTMB(
  species_richness ~ 
    (z_isolation * Season) +
    (z_ghmi * Season) +
    (z_area * Season) +
    z_effort +
    (1 | Park_Addre),
  data = migratory_df,
  family = poisson(link = "log")
)

summary(combined_model_migratory)
Anova(combined_model_migratory, type = "III")
performance(combined_model_migratory) # R^2 (cond.) = 0.928 and R^2 (marg) = 0.872

### repeat without sampling effort
combined_model_migratory_sn <- glmmTMB(
  species_richness ~ 
    (z_isolation * Season) +
    (z_ghmi * Season) +
    (z_area * Season) +
    (1 | Park_Addre),
  data = migratory_df,
  family = poisson(link = "log")
)

performance(combined_model_migratory_sn) # R^2 (cond.) = 0.934 and R^2 (marg) = 0.394


### create figure
# extract season specific slope sizes:
##### resdients first
## Area (residents)
res_area <- emtrends(
  combined_model_residential, 
  ~ Season, 
  var = "z_area"
) %>%
  as.data.frame() %>%
  rename(Effect = z_area.trend) %>%
  mutate(
    Attribute = "Area",
    Group = "Residents",
    lower = Effect - 1.96 * SE,
    upper = Effect + 1.96 * SE,
    signif = ifelse(lower > 0 | upper < 0, "*", "")
  )

# Isolation (residents)
res_iso <- emtrends(
  combined_model_residential, 
  ~ Season, 
  var = "z_isolation"
) %>%
  as.data.frame() %>%
  rename(Effect = z_isolation.trend) %>%
  mutate(
    Attribute = "Isolation",
    Group = "Residents",
    lower = Effect - 1.96 * SE,
    upper = Effect + 1.96 * SE,
    signif = ifelse(lower > 0 | upper < 0, "*", "")
  )

# GHMI (residents)
res_ghmi <- emtrends(
  combined_model_residential, 
  ~ Season, 
  var = "z_ghmi"
) %>%
  as.data.frame() %>%
  rename(Effect = z_ghmi.trend) %>%
  mutate(
    Attribute = "GHMI",
    Group = "Residents",
    lower = Effect - 1.96 * SE,
    upper = Effect + 1.96 * SE,
    signif = ifelse(lower > 0 | upper < 0, "*", "")
  )


### now migrants
## Area
mig_area <- emtrends(
  combined_model_migratory,
  ~ Season,
  var = "z_area"
) %>%
  as.data.frame() %>%
  rename(Effect = z_area.trend) %>%
  mutate(
    Attribute = "Area",
    Group = "Migrants",
    lower = Effect - 1.96 * SE,
    upper = Effect + 1.96 * SE,
    signif = ifelse(lower > 0 | upper < 0, "*", "")
  )

## GHMI
mig_ghmi <- emtrends(
  combined_model_migratory,
  ~ Season,
  var = "z_ghmi"
) %>%
  as.data.frame() %>%
  rename(Effect = z_ghmi.trend) %>%
  mutate(
    Attribute = "GHMI",
    Group = "Migrants",
    lower = Effect - 1.96 * SE,
    upper = Effect + 1.96 * SE,
    signif = ifelse(lower > 0 | upper < 0, "*", "")
  )

## Isolation
mig_iso <- emtrends(
  combined_model_migratory,
  ~ Season,
  var = "z_isolation"
) %>%
  as.data.frame() %>%
  rename(Effect = z_isolation.trend) %>%
  mutate(
    Attribute = "Isolation",
    Group = "Migrants",
    lower = Effect - 1.96 * SE,
    upper = Effect + 1.96 * SE,
    signif = ifelse(lower > 0 | upper < 0, "*", "")
  )


## cmbine into one df
effects_df <- bind_rows(
  res_area, res_ghmi, res_iso,
  mig_area, mig_ghmi, mig_iso
) %>%
  mutate(
    Attribute = factor(
      Attribute,
      levels = c("Area", "GHMI", "Isolation")
    ),
    Group = factor(
      Group,
      levels = c("Residents", "Migrants")
    )
  )

### color palette
season_colors <- c(
  "Overwintering"    = "#006400",
  "Spring Migration" = "#FF8C00",
  "Breeding"         = "#1E90FF",
  "Fall Migration"   = "#800080"
)

### plot
## residential:
res_effects_df <- effects_df %>%
  filter(Group == "Residents")

res_plot <- ggplot(
  res_effects_df,
  aes(x = Effect, y = Attribute, color = Season)
) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "black"
  ) +
  geom_point(
    position = position_dodge(width = 0.6),
    size = 3
  ) +
  geom_errorbarh(
    aes(xmin = lower, xmax = upper),
    position = position_dodge(width = 0.6),
    height = 0.2,
    linewidth = 0.8
  ) +
  geom_text(
    aes(x = Effect + 0.05, label = signif),
    position = position_dodge(width = 0.6),
    hjust = 0,
    size = 5,
    show.legend = FALSE
  ) +
  scale_color_manual(values = season_colors) +
  labs(
    x = "Standardized effect size (β ± 95% CI)",
    y = NULL,
    color = "Season",
    title = "Residential"
  ) +
  theme_bw(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

## migrants:
mig_effects_df <- effects_df %>%
  filter(Group == "Migrants")

mig_plot <- ggplot(
  mig_effects_df,
  aes(x = Effect, y = Attribute, color = Season)
) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "black"
  ) +
  geom_point(
    position = position_dodge(width = 0.6),
    size = 3
  ) +
  geom_errorbarh(
    aes(xmin = lower, xmax = upper),
    position = position_dodge(width = 0.6),
    height = 0.2,
    linewidth = 0.8
  ) +
  geom_text(
    aes(x = Effect + 0.05, label = signif),
    position = position_dodge(width = 0.6),
    hjust = 0,
    size = 5,
    show.legend = FALSE
  ) +
  scale_color_manual(values = season_colors) +
  labs(
    x = "Standardized effect size (β ± 95% CI)",
    y = NULL,
    color = "Season",
    title = "Migratory"
  ) +
  theme_bw(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )


### combined plot
combined_plot_x <- mig_plot + res_plot +
  plot_layout(ncol = 2, guides = "collect") & 
  theme(
    legend.position = "bottom"
  )
combined_plot_x

# Save as png
ggsave("Figures/supplementary/figure_2_comparison_keep_X.png", 
       combined_plot_x, bg = "transparent", width = 8, height = 5)

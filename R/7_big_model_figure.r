### big model with effect sizes of each attribute of residents versus migrants per season
### separation of migratory and residential species by dataframe to show which attributes are most important
###### for each migratory status
### code for figure 5

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
greenspaces <- readRDS("Data/final_data_for_big_script.RDS")

### remove the three parks with NAs for GHMI and Isolation
greenspaces <- greenspaces %>% 
  drop_na()

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
    (log10(nearest_dist_m) * Season) +
    (ghmi_mean * Season) +
    (log10(Shape_Area) * Season) +
    log10(number_of_checklists) + (1 | Park_Addre),
  data = residential_df,
  family = poisson(link = "log")
)

summary(combined_model_residential)
Anova(combined_model_residential, type = "III")
performance(combined_model_residential) # R^2 (cond.) = 0.740 and R^2 (marg) = 0.671

## migratory
combined_model_migratory <- glmmTMB(
  species_richness ~ 
    (log10(nearest_dist_m) * Season) +
    (ghmi_mean * Season) +
    (log10(Shape_Area) * Season) +
    log10(number_of_checklists) + (1 | Park_Addre),
  data = migratory_df,
  family = poisson(link = "log")
)

summary(combined_model_migratory)
Anova(combined_model_migratory, type = "III")
performance(combined_model_migratory) # R^2 (cond.) = 0.930 and R^2 (marg) = 0.876

### create figure
# extract season specific slope sizes:
##### resdients first
## Area (residents)
res_area <- emtrends(
  combined_model_residential, 
  ~ Season, 
  var = "log10(Shape_Area)"
) %>%
  as.data.frame() %>%
  rename(Effect = `log10(Shape_Area).trend`) %>%
  mutate(
    Attribute = "Area",
    Group = "Residents",
    lower = Effect - 1.96 * SE,
    upper = Effect + 1.96 * SE
  )

# Isolation (residents)
res_iso <- emtrends(
  combined_model_residential, 
  ~ Season, 
  var = "log10(nearest_dist_m)"
) %>%
  as.data.frame() %>%
  rename(Effect = `log10(nearest_dist_m).trend`) %>%
  mutate(
    Attribute = "Isolation",
    Group = "Residents",
    lower = Effect - 1.96 * SE,
    upper = Effect + 1.96 * SE
  )

# GHMI (residents)
res_ghmi <- emtrends(
  combined_model_residential, 
  ~ Season, 
  var = "ghmi_mean"
) %>%
  as.data.frame() %>%
  rename(Effect = `ghmi_mean.trend`) %>%
  mutate(
    Attribute = "GHMI",
    Group = "Residents",
    lower = Effect - 1.96 * SE,
    upper = Effect + 1.96 * SE
  )


### now migrants
## Area
mig_area <- emtrends(
  combined_model_migratory,
  ~ Season,
  var = "log10(Shape_Area)"
) %>%
  as.data.frame() %>%
  rename(Effect = `log10(Shape_Area).trend`) %>%
  mutate(
    Attribute = "Area",
    Group = "Migrants",
    lower = Effect - 1.96 * SE,
    upper = Effect + 1.96 * SE
  )

## GHMI
mig_ghmi <- emtrends(
  combined_model_migratory,
  ~ Season,
  var = "ghmi_mean"
) %>%
  as.data.frame() %>%
  rename(Effect = `ghmi_mean.trend`) %>%
  mutate(
    Attribute = "GHMI",
    Group = "Migrants",
    lower = Effect - 1.96 * SE,
    upper = Effect + 1.96 * SE
  )

## Isolation
mig_iso <- emtrends(
  combined_model_migratory,
  ~ Season,
  var = "log10(nearest_dist_m)"
)  %>%
  as.data.frame() %>%
  rename(Effect = `log10(nearest_dist_m).trend`) %>%
  mutate(
    Attribute = "Isolation",
    Group = "Migrants",
    lower = Effect - 1.96 * SE,
    upper = Effect + 1.96 * SE
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
    Group = factor(Group, levels = c("Residents", "Migrants"))
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
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(position = position_dodge(width = 0.6), size = 3) +
  geom_errorbarh(aes(xmin = lower, xmax = upper),
                 position = position_dodge(width = 0.6),
                 height = 0.2, linewidth = 0.8) +
  scale_color_manual(values = season_colors) +
  labs(
    x = "Effect size (β ± 95% CI)",
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
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(position = position_dodge(width = 0.6), size = 3) +
  geom_errorbarh(aes(xmin = lower, xmax = upper),
                 position = position_dodge(width = 0.6),
                 height = 0.2, linewidth = 0.8) +
  scale_color_manual(values = season_colors) +
  labs(
    x = "Effect size (β ± 95% CI)",
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
combined_plot <- mig_plot + res_plot +
  plot_layout(ncol = 2, guides = "collect") & 
  theme(
    legend.position = "bottom"
  )
combined_plot

# Save as png
ggsave("Figures/Fig_5/figure_5_effects_attributes.png", 
       combined_plot, bg = "transparent", width = 8, height = 5)


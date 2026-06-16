##### creating a comparison figure for with and without X checklists

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
library(reshape2)

## Load files for inclusion of X
greenspaces_x <- readRDS("Data/final_data_for_big_script_x.RDS")

### remove the three parks with NAs for GHMI and Isolation
greenspaces_x <- greenspaces_x %>% 
  drop_na()

##############################
## standardize predictors   ##
##############################
# log-transform first, then z-transform
greenspaces_x <- greenspaces_x %>%
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
residential_df_x <- greenspaces_x %>%
  filter(analysis == "residential")
residential_df_x<- residential_df_x %>% dplyr::select(-analysis)

# migratory birds
migratory_df_x <- greenspaces_x %>%
  filter(analysis == "migratory")
migratory_df_x <- migratory_df_x %>% dplyr::select(-analysis)

##### now create a model wiht all three greenspace attributes
## residential
combined_model_residential_x <- glmmTMB(
  species_richness ~ 
    (z_isolation * Season) +
    (z_ghmi * Season) +
    (z_area * Season) +
    z_effort +
    (1 | Park_Addre),
  data = residential_df_x,
  family = poisson(link = "log")
)

## migratory
combined_model_migratory_x <- glmmTMB(
  species_richness ~ 
    (z_isolation * Season) +
    (z_ghmi * Season) +
    (z_area * Season) +
    z_effort +
    (1 | Park_Addre),
  data = migratory_df_x,
  family = poisson(link = "log")
)


################################
# load files for exclusion of x
################################
## Load files
greenspaces <- readRDS("Data/final_data_for_big_script.RDS")

### remove the three parks with NAs for GHMI and Isolation
greenspaces <- greenspaces %>% 
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

###############################
### create supplementary figure
###############################
# residential with x
res_x_area <- emtrends(
  combined_model_residential_x,
  ~ Season,
  var = "z_area"
) %>%
  as.data.frame() %>%
  mutate(Attribute = "Area",
         Group = "Residents",
         X = "Included X",
         lower = z_area.trend - 1.96 * SE,
         upper = z_area.trend + 1.96 * SE)

res_x_ghmi <- emtrends(
  combined_model_residential_x,
  ~ Season,
  var = "z_ghmi"
) %>%
  as.data.frame() %>%
  mutate(Attribute = "GHMI",
         Group = "Residents",
         X = "Included X",
         lower = z_ghmi.trend - 1.96 * SE,
         upper = z_ghmi.trend + 1.96 * SE)

res_x_iso <- emtrends(
  combined_model_residential_x,
  ~ Season,
  var = "z_isolation"
) %>%
  as.data.frame() %>%
  mutate(Attribute = "Isolation",
         Group = "Residents",
         X = "Included X",
         lower = z_isolation.trend - 1.96 * SE,
         upper = z_isolation.trend + 1.96 * SE)

# residential without x
res_area <- emtrends(
  combined_model_residential,
  ~ Season,
  var = "z_area"
) %>%
  as.data.frame() %>%
  mutate(Attribute = "Area",
         Group = "Residents",
         X = "Excluded X",
         lower = z_area.trend - 1.96 * SE,
         upper = z_area.trend + 1.96 * SE)

res_ghmi <- emtrends(
  combined_model_residential,
  ~ Season,
  var = "z_ghmi"
) %>%
  as.data.frame() %>%
  mutate(Attribute = "GHMI",
         Group = "Residents",
         X = "Excluded X",
         lower = z_ghmi.trend - 1.96 * SE,
         upper = z_ghmi.trend + 1.96 * SE)

res_iso <- emtrends(
  combined_model_residential,
  ~ Season,
  var = "z_isolation"
) %>%
  as.data.frame() %>%
  mutate(Attribute = "Isolation",
         Group = "Residents",
         X = "Excluded X",
         lower = z_isolation.trend - 1.96 * SE,
         upper = z_isolation.trend + 1.96 * SE)

# migratory with x 
mig_x_area <- emtrends(combined_model_migratory_x, ~ Season, var = "z_area") %>%
  as.data.frame() %>%
  mutate(Attribute = "Area",
         Group = "Migrants",
         X = "Included X",
         lower = z_area.trend - 1.96 * SE,
         upper = z_area.trend + 1.96 * SE)

mig_x_ghmi <- emtrends(combined_model_migratory_x, ~ Season, var = "z_ghmi") %>%
  as.data.frame() %>%
  mutate(Attribute = "GHMI",
         Group = "Migrants",
         X = "Included X",
         lower = z_ghmi.trend - 1.96 * SE,
         upper = z_ghmi.trend + 1.96 * SE)

mig_x_iso <- emtrends(combined_model_migratory_x, ~ Season, var = "z_isolation") %>%
  as.data.frame() %>%
  mutate(Attribute = "Isolation",
         Group = "Migrants",
         X = "Included X",
         lower = z_isolation.trend - 1.96 * SE,
         upper = z_isolation.trend + 1.96 * SE)

# migratory without x
mig_area <- emtrends(combined_model_migratory, ~ Season, var = "z_area") %>%
  as.data.frame() %>%
  mutate(Attribute = "Area",
         Group = "Migrants",
         X = "Excluded X",
         lower = z_area.trend - 1.96 * SE,
         upper = z_area.trend + 1.96 * SE)

mig_ghmi <- emtrends(combined_model_migratory, ~ Season, var = "z_ghmi") %>%
  as.data.frame() %>%
  mutate(Attribute = "GHMI",
         Group = "Migrants",
         X = "Excluded X",
         lower = z_ghmi.trend - 1.96 * SE,
         upper = z_ghmi.trend + 1.96 * SE)

mig_iso <- emtrends(combined_model_migratory, ~ Season, var = "z_isolation") %>%
  as.data.frame() %>%
  mutate(Attribute = "Isolation",
         Group = "Migrants",
         X = "Excluded X",
         lower = z_isolation.trend - 1.96 * SE,
         upper = z_isolation.trend + 1.96 * SE)


#### combine everything
effects_df <- bind_rows(
  res_x_area, res_x_ghmi, res_x_iso,
  res_area, res_ghmi, res_iso,
  mig_x_area, mig_x_ghmi, mig_x_iso,
  mig_area, mig_ghmi, mig_iso
) %>%
  mutate(
    Attribute = factor(Attribute, levels = c("Area", "GHMI", "Isolation")),
    Group = factor(Group, levels = c("Residents", "Migrants")),
    X = factor(X, levels = c("Excluded X", "Included X"))
  )

### color palette
season_colors <- c(
  "Overwintering"    = "#006400",
  "Spring Migration" = "#FF8C00",
  "Breeding"         = "#1E90FF",
  "Fall Migration"   = "#800080"
)

### fix plot
effects_df <- effects_df %>%
  mutate(
    effect = case_when(
      Attribute == "Area" ~ z_area.trend,
      Attribute == "GHMI" ~ z_ghmi.trend,
      Attribute == "Isolation" ~ z_isolation.trend
    )
  )

ggplot(
  effects_df,
  aes(x = effect, y = Attribute, color = Season)
) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(
    aes(shape = X),
    position = position_dodge(width = 0.6),
    size = 3
  ) +
  geom_errorbarh(
    aes(xmin = lower, xmax = upper),
    position = position_dodge(width = 0.6),
    height = 0.2,
    linewidth = 0.8
  ) +
  facet_grid(Group ~ X) +
  scale_color_manual(values = season_colors) +
  labs(
    x = "Standardized effect size (β ± 95% CI)",
    y = NULL,
    color = "Season",
    shape = "Model"
  ) +
  theme_bw(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    legend.position = "bottom"
  )


#####
# plot both together in one to show effects sizeeee
#####
# create a single effect-size column
effects_plot <- effects_df %>%
  mutate(
    effect = case_when(
      Attribute == "Area" ~ z_area.trend,
      Attribute == "GHMI" ~ z_ghmi.trend,
      Attribute == "Isolation" ~ z_isolation.trend
    )
  ) %>%
  select(Group, Season, Attribute, X, effect) %>%
  pivot_wider(
    names_from = X,
    values_from = effect
  )

# plot
both_plot <- ggplot(effects_plot, aes(x = `Excluded X`, y = `Included X`, color = Season, shape = Attribute)) +
  geom_point(size = 4) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed"
  ) +
  facet_wrap(~ Group) +
  labs(
    x = "Effect size (Without X)",
    y = "Effect size (With X)",
    color = "Season",
    shape = "Attribute"
  ) +
  theme_classic(base_size = 13)

### save as png
ggsave("Figures/supplementary/effect_size_with_without_x.png", 
       both_plot, bg = "transparent", width = 8, height = 5)

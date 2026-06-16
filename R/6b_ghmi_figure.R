## GHMi shows the global human modification index which is a scale of 0-1 with one being 
### code for figure 5
###### calculation predicted species richness, slopes, and glmm for ghmi

# Load packages
library(sf)
library(tidyverse)
library(glmmTMB)
library(emmeans)
library(lme4)
library(car)
library(performance)
library(forcats)

# Read file
greenspaces <- readRDS("Data/final_data_for_big_script.RDS")

### remove the three parks with NAs for GHMI and Isolation
greenspaces <- greenspaces %>% 
  drop_na()

# Filter dataset to migratory & residential for analysis
greenspaces <- greenspaces %>%
  filter(analysis %in% c("migratory", "residential"))

# Reorder season and analysis for figures
greenspaces$analysis <- factor(
  greenspaces$analysis,
  levels = c("residential", "migratory")
)

greenspaces$Season <- factor(
  greenspaces$Season,
  levels = c("Overwintering", "Spring Migration", "Breeding", "Fall Migration"),
  ordered = TRUE
)

# fit model for GHMI
## poisson model
ghmi_model_poisson <- glmmTMB(
  species_richness ~ 
    (ghmi_mean * Season) + (ghmi_mean * analysis) + (ghmi_mean * Season * analysis) +
    log10(number_of_checklists) + (1 | Park_Addre),
  data = greenspaces,
  family = poisson(link = "log")
)

summary(ghmi_model_poisson)
Anova(ghmi_model_poisson, type = "III")
performance(ghmi_model_poisson)

summary(
  emtrends(
    ghmi_model_poisson,
    ~ analysis,
    var = "ghmi_mean"
  ),
  infer = c(TRUE, TRUE)
)

#### compare R^2 value without effort covariate
ghmi_model_poisson_sn <- glmmTMB(
  species_richness ~ 
    (ghmi_mean * Season) + (ghmi_mean * analysis) + (ghmi_mean * Season * analysis) +
    (1 | Park_Addre),
  data = greenspaces,
  family = poisson(link = "log")
)

performance(ghmi_model_poisson_sn)

#### resume original model and start making prediciton grid/marginal slopes
# marginal slopes
emm_ghmi_trends <- emtrends(
  ghmi_model_poisson,
  var = "ghmi_mean",
  specs = ~ Season * analysis,
  type = "response"
)

# convert to data frame and add significance marker & y positions
emm_ghmi_df <- as.data.frame(emm_ghmi_trends) %>%
  mutate(
    signif = ifelse(asymp.LCL > 0 | asymp.UCL < 0, "*", ""),
    y_position = ghmi_mean.trend + 0.5
  )

# create prediction sequence for GHMI
ghmi_seq <- seq(
  quantile(greenspaces$ghmi_mean, 0.05, na.rm = TRUE),
  quantile(greenspaces$ghmi_mean, 0.95, na.rm = TRUE),
  length.out = 50
)

# generate predictions manually for GHMI × Season × Analysis
ghmi_emm <- emmeans(
  ghmi_model_poisson,
  ~ ghmi_mean | Season + analysis,
  at = list(
    ghmi_mean = ghmi_seq,
    number_of_checklists = median(greenspaces$number_of_checklists, na.rm = TRUE)
  ),
  type = "response"
)

ghmi_df <- as.data.frame(ghmi_emm)

# capitalize this
ghmi_df <- ghmi_df %>%
  mutate(
    analysis = dplyr::recode(analysis,
                             migratory = "Migratory",
                             residential = "Residential")
  )

## significance
ghmi_trends <- emtrends(
  ghmi_model_poisson,
  ~ Season * analysis,
  var = "ghmi_mean"
)

ghmi_trends_df <- as.data.frame(summary(ghmi_trends, infer = TRUE)) %>%
  mutate(
    analysis = dplyr::recode(analysis,
                             migratory = "Migratory",
                             residential = "Residential")
  )
ghmi_trends_df

# add labels
sig_lookup <- ghmi_trends_df %>%
  mutate(sig = case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01  ~ "**",
    p.value < 0.05  ~ "*",
    TRUE            ~ NA_character_
  )) %>%
  select(analysis, Season, sig)


### add to plot
ghmi_df <- ghmi_df %>%
  left_join(sig_lookup, by = c("analysis", "Season"))

ghmi_df <- ghmi_df %>%
  mutate(
    analysis = factor(analysis,
                      levels = c("Migratory", "Residential"))
  )

# clip it
sig_points <- ghmi_df %>%
  group_by(analysis, Season) %>%
  summarise(
    x = max(ghmi_mean) * 1.05,
    y = max(rate, na.rm = TRUE),
    sig = first(sig),
    .groups = "drop"
  ) %>%
  filter(!is.na(sig))

# now plot with shaded CIs
ghmi_plot <- ggplot(ghmi_df, aes(x = ghmi_mean, y = rate, color = Season)) +
  geom_ribbon(
    aes(ymin = asymp.LCL, ymax = asymp.UCL, fill = Season),
    alpha = 0.2, color = NA
  ) +
  geom_line(linewidth = 1) +
  geom_text(
    data = sig_points, aes(x = x, y = y, label = sig, color = Season),
    inherit.aes = FALSE, size = 5, hjust = 0, show.legend = FALSE
  ) +
  facet_wrap(~analysis, scales = "free_y") +
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
  )) +
  labs(
    x = "GHMI (Global Human Modification Index)",
    y = "Predicted Species Richness",
    color = "Season",
    fill = "Season"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 0.5
    ),
    axis.title = element_text(size = 14),
    axis.text = element_text(color = "black", size = 12),
    strip.text = element_text(size = 14),
    legend.position = "bottom",
    plot.margin = margin(5.5, 30, 5.5, 5.5)
  ) +
  coord_cartesian(clip = "off")

ghmi_plot

# Save as png
ggsave("Figures/ghmi/ghmi_predicted_response_migratory_residential_sig.png", 
       ghmi_plot, bg = "transparent", width = 8, height = 5)

### calculate slopes
ghmi_slopes <- ghmi_emmip_data %>%
  filter(analysis %in% c("Migratory", "Residential")) %>%
  group_by(analysis, Season) %>%
  do({
    m <- lm(log(yvar) ~ ghmi_mean, data = .)
    data.frame(
      slope = coef(m)[2],
      intercept = coef(m)[1],
      r2 = summary(m)$r.squared
    )
  }) %>%
  ungroup()

ghmi_slopes


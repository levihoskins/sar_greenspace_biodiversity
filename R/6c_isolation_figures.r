### modeling isolation effects and plotting the predicted richness
### code for figure 4
###### calculation predicted species richness, slopes, and glmm for isolation

# Load packages
library(sf)
library(tidyverse)
library(glmmTMB)
library(emmeans)
library(lme4)
library(car)
library(performance)
library(scales)

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

## run model
## poisson model
isolation_model_poisson <- glmmTMB(
  species_richness ~ 
    (log10(nearest_dist_m) * Season) + (log10(nearest_dist_m) * analysis) + 
    (log10(nearest_dist_m) * Season * analysis) +
    log10(number_of_checklists) + (1 | Park_Addre),
  data = greenspaces,
  family = poisson(link = "log")
)

summary(isolation_model_poisson)
Anova(isolation_model_poisson, type = "III")
performance(isolation_model_poisson)

summary(
  emtrends(
    isolation_model_poisson,
    ~ analysis,
    var = "log10(nearest_dist_m)"
  ),
  infer = c(TRUE, TRUE)
)

### run model without effort covariate for R^2 comparison
isolation_model_poisson_sn <- glmmTMB(
  species_richness ~ 
    (log10(nearest_dist_m) * Season) + (log10(nearest_dist_m) * analysis) + 
    (log10(nearest_dist_m) * Season * analysis) +
    (1 | Park_Addre),
  data = greenspaces,
  family = poisson(link = "log")
)

performance(isolation_model_poisson_sn)

#### resume original model and start making prediction grid
# Create a prediction grid for nearest distance (log10 transformed)
dist_grid <- with(greenspaces, 
                  list(
                    nearest_dist_m = seq(
                      min(nearest_dist_m, na.rm = TRUE),
                      max(nearest_dist_m, na.rm = TRUE),
                      length.out = 100
                    ),
                    number_of_checklists = median(number_of_checklists, na.rm = TRUE),
                    Season = levels(Season),
                    analysis = levels(analysis)
                  ))

# Remove invalid analysis levels
dist_grid$analysis <- intersect(
  dist_grid$analysis,
  levels(model.frame(isolation_model_poisson)$analysis)
)

## get signficance
isolation_trends <- emtrends(
  isolation_model_poisson,
  ~ Season * analysis,
  var = "nearest_dist_m"
)

isolation_trends_df <- as.data.frame(
  summary(isolation_trends, infer = TRUE)
)

isolation_trends_df

## create labels
sig_lookup <- isolation_trends_df %>%
  mutate(
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE            ~ NA_character_
    )
  ) %>%
  select(analysis, Season, sig)

# Get predicted response from emmeans
emm_poly <- emmeans(isolation_model_poisson,
                    ~ nearest_dist_m | analysis + Season,
                    at = dist_grid,
                    type = "response")

emm_poly_df <- as.data.frame(emm_poly)

emm_poly_df <- emm_poly_df %>%
  left_join(sig_lookup, by = c("analysis", "Season"))

## star positions
sig_points <- emm_poly_df %>%
  group_by(analysis, Season) %>%
  summarise(
    x = max(nearest_dist_m) * 1.05,
    y = max(rate, na.rm = TRUE),
    sig = first(sig),
    .groups = "drop"
  ) %>%
  filter(!is.na(sig))

# plot
nn_distance_plot_log <- ggplot(
  emm_poly_df %>%
    filter(analysis %in% c("migratory", "residential")) %>%
    mutate(analysis = ifelse(analysis == "migratory", "Migratory", "Residential")),
  aes(x = nearest_dist_m, y = rate, color = Season, fill = Season)
) +
  geom_line(size = 1) +
  geom_text(
    data = sig_points %>%
      mutate(
        analysis = ifelse(
          analysis == "migratory",
          "Migratory",
          "Residential"
        )
      ),
    aes(x = x, y = y, label = sig, color = Season),
    inherit.aes = FALSE, size = 5, hjust = 0, show.legend = FALSE
  ) +
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL), alpha = 0.2, color = NA) +
  facet_wrap(~analysis, scales="free_y") +
  scale_x_log10(
    labels = scales::label_number(accuracy = 1, big.mark = ","),
    breaks = scales::log_breaks(n = 4)
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
  )) +
  labs(
    x = "Isolation (m)",
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
  coord_cartesian(clip = "off") +
  theme(
    plot.margin = margin(5.5, 30, 5.5, 5.5)
  )
nn_distance_plot_log

# Save as png
ggsave("Figures/isolation/isolation_predicted_response_migratory_residential_sig.png", 
       nn_distance_plot_log, bg = "transparent", width = 8, height = 5)


### calculate slopes
isolation_slopes <- emm_poly_df %>%
  filter(analysis %in% c("migratory", "residential")) %>%
  group_by(analysis, Season) %>%
  summarise(
    slope = coef(lm(rate ~ log10(nearest_dist_m)))[2],
    intercept = coef(lm(rate ~ log10(nearest_dist_m)))[1],
    r2 = summary(lm(rate ~ log10(nearest_dist_m)))$r.squared,
    .groups = "drop"
  ) %>%
  mutate(
    analysis = dplyr::recode(
      analysis,
      migratory   = "Migratory",
      residential = "Residential"
    )
  )

isolation_slopes

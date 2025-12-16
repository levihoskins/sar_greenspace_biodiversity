

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
library("performance")

# Read file
greenspaces <- readRDS("Data/final_data_for_big_script.RDS")

### remove the three parks with NAs for GHMI and Isolation
greenspaces <- greenspaces %>% 
  drop_na()

# Reorder season and analysis for figures
greenspaces$analysis <- factor(
  greenspaces$analysis,
  levels = c("residential", "migratory", "total")
)

greenspaces$Season <- factor(
  greenspaces$Season,
  levels = c("Overwintering", "Spring Migration", "Breeding", "Fall Migration"),
  ordered = TRUE
)

# Filter dataset to migratory & residential for analysis
greenspaces <- greenspaces %>%
  filter(analysis %in% c("migratory", "residential"))

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

# Get predicted response from emmeans
emm_poly <- emmeans(isolation_model_poisson,
                    ~ nearest_dist_m | analysis + Season,
                    at = dist_grid,
                    type = "response")

emm_poly_df <- as.data.frame(emm_poly)

# plot
nn_distance_plot_log <- ggplot(
  emm_poly_df %>%
    filter(analysis %in% c("migratory", "residential")) %>%
    mutate(analysis = ifelse(analysis == "migratory", "Migratory", "Residential")),
  aes(x = nearest_dist_m, y = rate, color = Season, fill = Season)
) +
  geom_line(size = 1) +
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
    x = "Nearest Neighbor Distance (m)",
    y = "Predicted Species Richness",
    color = "Season",
    fill = "Season"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(color = "black", linewidth = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(color = "black", size = 12),
    strip.text = element_text(size = 14),
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12)
  ) +
  guides(
    colour = guide_legend(override.aes = list(linetype = 1, shape = NA, alpha = 1)),
    linetype = guide_legend(override.aes = list(size = 1))
  )
nn_distance_plot_log

# Save as png
ggsave("Figures/Fig_3/figure_3_isolation_predicted_response_migratory_residential_sig.png", 
       nn_distance_plot_log, bg = "transparent", width = 8, height = 5)

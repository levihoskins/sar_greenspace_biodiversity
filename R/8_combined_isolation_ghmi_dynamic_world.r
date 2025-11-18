### combined isolation, ghmi, and dynamic world:

# Load packages
library("sf")
library("dplyr")
library("ggplot2")
library("glmmTMB")
library("broom.mixed")
library("emmeans")
library("stats")
library("tidyr")
library("gt")
library("RColorBrewer")
library("stringr")
library("forcats")
library("DHARMa")
library("readr")
library("patchwork")
library("scales")

# Read files
greenspaces <- readRDS("Data/final_data_for_big_script.RDS")

##############
# Isolation
##############
# nb glmm
m_linear <- glmmTMB(
  species_richness ~ log10(nearest_dist_m) * analysis * Season + 
    log10(number_of_checklists) + log10(Shape_Area),
  data = greenspaces,
  family = nbinom2
)

# Create a prediction grid for nearest distance (log10 transformed)
dist_grid <- with(greenspaces, 
                  list(nearest_dist_m = seq(min(nearest_dist_m, na.rm = TRUE),
                                            max(nearest_dist_m, na.rm = TRUE),
                                            length.out = 100),
                       number_of_checklists = median(number_of_checklists, na.rm = TRUE),
                       Season = levels(Season),
                       analysis = levels(analysis)))

# Get predicted response from emmeans
emm_poly <- emmeans(m_linear,
                    ~ nearest_dist_m | analysis + Season,
                    at = dist_grid,
                    type = "response")

emm_poly_df <- as.data.frame(emm_poly)

# Update the ggplot
## matching ghmi style — log-transformed axis with whole-number labels
nn_distance_plot_log <- ggplot(
  emm_poly_df %>%
    filter(analysis %in% c("residential", "migratory")),
  aes(x = nearest_dist_m, y = response, color = Season, fill = Season)
) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL), alpha = 0.2, color = NA) +
  facet_wrap(~analysis) +
  scale_x_log10(
    labels = scales::label_number(accuracy = 1, big.mark = ","),
    breaks = scales::log_breaks(n = 6)
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

#######################
# GHMI
################
# Filter dataset to migratory & residential for analysis
greenspaces <- greenspaces %>%
  filter(analysis %in% c("migratory", "residential"))

# NB GLMM: GHMI * Season * analysis + log10(number_of_checklists)
nb_glmm_ghmi_season_analysis <- glmmTMB(
  species_richness ~ ghmi_mean * Season * analysis + log10(number_of_checklists),
  data = greenspaces,
  family = nbinom2
)

# Marginal slopes for GHMI by Season × analysis
emm_ghmi_trends <- emtrends(
  nb_glmm_ghmi_season_analysis,
  var = "ghmi_mean",
  specs = ~ Season * analysis,
  type = "response"
)

# Convert to data frame and add significance marker & y positions
emm_ghmi_df <- as.data.frame(emm_ghmi_trends) %>%
  mutate(
    signif = ifelse(asymp.LCL > 0 | asymp.UCL < 0, "*", ""),
    y_position = ghmi_mean.trend + 0.5
  )

# Create prediction sequence for GHMI
ghmi_seq <- seq(
  quantile(greenspaces$ghmi_mean, 0.05, na.rm = TRUE),
  quantile(greenspaces$ghmi_mean, 0.95, na.rm = TRUE),
  length.out = 50
)

# Generate predictions manually for GHMI × Season × Analysis
ghmi_emmip_data <- as.data.frame(
  emmip(
    nb_glmm_ghmi_season_analysis,
    Season ~ ghmi_mean | analysis,
    type = "response",
    at = list(
      ghmi_mean = ghmi_seq,
      number_of_checklists = median(greenspaces$number_of_checklists, na.rm = TRUE)
    ),
    CIs = TRUE,
    plotit = FALSE
  )
)

# Now plot with shaded CIs
ghmi_plot <- ggplot(ghmi_emmip_data, aes(x = ghmi_mean, y = yvar, color = Season)) +
  geom_ribbon(aes(ymin = LCL, ymax = UCL, fill = Season), alpha = 0.2, color = NA) +
  geom_line(size = 1) +
  facet_wrap(~analysis) +
  labs(
    x = "GHMI (Global Human Modification Index)",
    y = "Predicted Species Richness",
    colour = "Season",
    fill = "Season"
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
  )

ghmi_plot

# Hide legend in nn_distance_plot
nn_distance_plot_noleg <- nn_distance_plot_log + theme(legend.position = "none")

# Combine the plots vertically
combined_plot <- nn_distance_plot_noleg / ghmi_plot +
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag.position = c(0.08, 0.95),
        plot.tag = element_text(size = 14, face = "bold"))

combined_plot

ggsave("Figures/figure_3_combined_ghmi_isolation.png", combined_plot, bg = "transparent", width = 7, height = 8)

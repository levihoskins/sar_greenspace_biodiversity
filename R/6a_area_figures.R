#### ghmi figures


# Load packages
library("sf")
library("dplyr")
library("glmmTMB")
library("emmeans")
library("ggplot2")
library("lme4")
library("performance")
library("car")
library("DHARMa")

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

### run the same model as other script
area_model_poisson <- glmmTMB(
  species_richness ~ 
    (log10(Shape_Area) * Season) + (log10(Shape_Area) * analysis) + (log10(Shape_Area) * Season * analysis) +
    log10(number_of_checklists) + (1 | Park_Addre),
  data = greenspaces,
  family = poisson(link = "log")
)

summary(area_model_poisson)
Anova(area_model_poisson, type = "III")
performance(area_model_poisson)

# Create a prediction grid for area (log10 transformed)
area_seq <- 10 ^ seq(
  log10(min(greenspaces$Shape_Area, na.rm = TRUE)),
  log10(max(greenspaces$Shape_Area, na.rm = TRUE)),
  length.out = 200
)

dist_grid <- list(
  Shape_Area = area_seq,
  number_of_checklists = median(greenspaces$number_of_checklists, na.rm = TRUE),
  Season = levels(greenspaces$Season),
  analysis = levels(model.frame(area_model_poisson)$analysis)
)

# Get predicted response from emmeans
emm_area <- emmeans(area_model_poisson,
                    ~ Shape_Area | analysis + Season,
                    at = dist_grid,
                    type = "response")

emm_area_df <- as.data.frame(emm_area)

# plot
area_plot <- ggplot(
  emm_area_df %>%
    filter(analysis %in% c("migratory", "residential")) %>%
    mutate(analysis = ifelse(analysis == "migratory", "Migratory", "Residential")),
  aes(x = Shape_Area, y = rate, color = Season, fill = Season)
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
    x = "Greenspace Area (m^2)",
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
area_plot

# Save as png
ggsave("Figures/Fig_2/figure_2_area_predicted_response_migratory_residential_sig.png", 
       area_plot, bg = "transparent", width = 8, height = 5)

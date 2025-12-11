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
ghmi_emmip_data <- as.data.frame(
  emmip(
    ghmi_model_poisson,
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

# now plot with shaded CIs
ghmi_plot <- ggplot(ghmi_emmip_data, aes(x = ghmi_mean, y = yvar, color = Season)) +
  geom_ribbon(aes(ymin = LCL, ymax = UCL, fill = Season), alpha = 0.2, color = NA) +
  geom_line(size = 1) +
  facet_wrap(~analysis, scales="free_y") +
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

# Save as png
ggsave("Figures/figure_3_ghmi_predicted_response_migratory_residential_sig.png", ghmi_plot, bg = "transparent", width = 8, height = 5)


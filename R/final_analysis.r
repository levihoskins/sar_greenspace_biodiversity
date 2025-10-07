### BIG MODEL

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

## Load files
greenspaces <- readRDS("Data/final_data_for_big_script.RDS")

### create the big model with all the interactions
glmm_big_model <- glmmTMB(
  species_richness ~ 
    (log1p(nearest_dist_m) * Season * analysis) + (Season * analysis * ghmi_mean) + 
    (Season * analysis * dominant_class) + log10(number_of_checklists),
  data = greenspaces,
  family = nbinom2
)

summary(glmm_big_model)
Anova(glmm_big_model)

# Summarize the model with confidence intervals
big_mod_summary_glmm <- broom.mixed::tidy(glmm_big_model, conf.int = TRUE) %>%
  mutate(predictor = term) %>%
  mutate(Scale = case_when(
    grepl("nearest_dist_m", predictor) ~ "Isolation",
    grepl("ghmi_mean", predictor) ~ "Landscape",
    grepl("dominant_class", predictor) ~ "Landscape",
    grepl("number_of_checklists", predictor) ~ NA_character_,
    TRUE ~ NA_character_
  ))

# Plot effect sizes (skip intercept and checklists)
big_mod_summary_glmm %>%
  filter(! predictor %in% c("(Intercept)", "log10(number_of_checklists)")) %>%
  ggplot(aes(x = predictor, y = estimate, color = Scale)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  coord_flip() +
  theme_bw() +
  theme(axis.text = element_text(color = "black")) +
  ylab("Effect size") +
  xlab("") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  scale_color_brewer(palette = "Dark2") +
  ggtitle("NB GLMM Effect Sizes")

# Export table of model outputs
big_mod_res <- as.data.frame(broom.mixed::tidy(glmm_big_model))
write.csv(big_mod_res, file = "Figures/big_mod_table_glmm.csv", row.names = FALSE)







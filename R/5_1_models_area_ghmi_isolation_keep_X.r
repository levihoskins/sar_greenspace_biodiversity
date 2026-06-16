# Final Combined Model with all interactions for each variable with season and migratory status

## Keep X

### This script runs every single model, and then is used to create supplementary tables for each model.

# Load packages
library(sf)
library(tidyverse)
library(glmmTMB)
library(lme4)
library(car)
library(performance)
library(DHARMa)
library(broom.mixed)
library(gt)
library(webshot2)

## Load files
greenspaces <- readRDS("Data/final_data_for_big_script_x.RDS")

### remove the three parks with NAs for GHMI and Isolation
greenspaces <- greenspaces %>% 
  drop_na() %>%
  filter(analysis != "total")

####################
### Exploratory data
####################
# Number of Greenspaces
length(unique(greenspaces$Park_Addre)) #72

## size of greensapces
mean(greenspaces$Shape_Area/10000)
range(greenspaces$Shape_Area/10000)
sd(greenspaces$Shape_Area/10000)

### quick visualization of park size median (in red)
ggplot(greenspaces, aes(x=Shape_Area/10000))+
  geom_histogram(fill="gray80", color="black")+
  geom_vline(xintercept = 19.80487, col = "red")+
  theme_bw()+
  scale_x_log10()

# Quick visualization to show lists and richness 
# Histogram of 'lists'
hist(greenspaces$number_of_checklists,
     main = "Distribution of Lists",
     xlab = "Number of Lists",
     col = "skyblue")

# Histogram of 'species_richness'
hist(greenspaces$species_richness,
     main = "Distribution of Species Richness",
     xlab = "Species Richness",
     col = "pink")

###histogram eliminates lm as an option because not normal

### let's model
############
##  area  ##
############
## start with poisson
area_model_poisson <- glmmTMB(
  species_richness ~ 
    (log10(Shape_Area) * Season) + 
    (log10(Shape_Area) * analysis) + 
    (log10(Shape_Area) * Season * analysis) +
    log10(number_of_checklists) + (1 | Park_Addre),
  data = greenspaces,
  family = poisson(link = "log")
)

summary(area_model_poisson)
Anova(area_model_poisson, type = "III")
performance(area_model_poisson) # R^2 (cond.) = 0.922 and R^2 (marg) = 0.882

# simulate residuals
sim_res <- simulateResiduals(area_model_poisson)
# plot residuals
plot(sim_res)
# test dispersion parameters
testDispersion(sim_res)
#### slightly underdispersed which is suprising and means do not use negative binomial, instead keep poisson

############
##  GHMI  ##
############
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
performance(ghmi_model_poisson) # R^2 (cond.) = 0.920 and R^2 (marg) = 0.870

#################
##  isolation  ##
#################
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
performance(isolation_model_poisson) # R^2 (cond.) = 0.920 and R^2 (marg) = 0.869

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
performance(combined_model_residential) # R^2 (cond.) = 0.735 and R^2 (marg) = 0.670

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

##############
##  Tables  ##
##############
## create a funciton to not have to repeat code each time
tidy_glmmTMB_signif <- function(model) {
  
  # Extract model coefficients
  coef_tbl <- broom.mixed::tidy(model, effects = "fixed") %>%
    dplyr::mutate(
      # Round numeric columns
      estimate = round(estimate, 3),
      std.error = round(std.error, 3),
      statistic = round(statistic, 3),
      # Format p-values
      p.value.formatted = dplyr::case_when(
        p.value < 0.001 ~ "<0.001*",
        p.value < 0.05  ~ paste0(round(p.value, 3), "*"),
        TRUE            ~ as.character(round(p.value, 3))
      )
    )
  
  # Extract R²
  r2_vals <- performance::r2(model)
  r2_text <- paste0("R² (marginal) = ", round(r2_vals$R2_marginal, 3),
                    ", R² (conditional) = ", round(r2_vals$R2_conditional, 3))
  
  # Create gt table
  coef_tbl %>%
    gt::gt() %>%
    gt::tab_header(
      title = paste("Model Summary:", deparse(substitute(model))),
      subtitle = r2_text
    ) %>%
    gt::cols_label(
      term = "Predictor",
      estimate = "Estimate",
      std.error = "Std. Error",
      statistic = "z value",
      p.value.formatted = "p value"
    ) %>%
    gt::cols_hide(columns = "p.value")
}

## apply to each model
tab_area_x <- tidy_glmmTMB_signif(area_model_poisson)
tab_ghmi_x <- tidy_glmmTMB_signif(ghmi_model_poisson)
tab_isolation_x <- tidy_glmmTMB_signif(isolation_model_poisson)
tab_comb_res_x <- tidy_glmmTMB_signif(combined_model_residential)
tab_comb_mig_x <- tidy_glmmTMB_signif(combined_model_migratory)

# save tables as PDFs
gtsave(tab_area_x, "Figures/supplementary/models/area_model_x.pdf")
gtsave(tab_ghmi_x, "Figures/supplementary/models/ghmi_model_x.pdf")
gtsave(tab_isolation_x, "Figures/supplementary/models/isolation_model_x.pdf")
gtsave(tab_comb_res_x, "Figures/supplementary/models/combined_residential_model_X.pdf")
gtsave(tab_comb_mig_x, "Figures/supplementary/models/combined_migratory_model_x.pdf")

### clean tables for Ben using ANOVA instead
extract_interaction_pvals <- function(model) {
  car::Anova(model, type = "III") %>%
    as.data.frame() %>%
    tibble::rownames_to_column("term") %>%
    dplyr::filter(grepl(":", term)) %>%
    dplyr::select(term, `Pr(>Chisq)`) %>%
    dplyr::mutate(
      `Pr(>Chisq)` = dplyr::case_when(
        `Pr(>Chisq)` < 0.001 ~ "<0.001*",
        `Pr(>Chisq)` < 0.05  ~ paste0(round(`Pr(>Chisq)`, 3), "*"),
        TRUE                ~ as.character(round(`Pr(>Chisq)`, 3))
      )
    )
}

## apply to each model
tab_area_anova_x <- extract_interaction_pvals(area_model_poisson)
tab_ghmi_anova_x <- extract_interaction_pvals(ghmi_model_poisson)
tab_isolation_anova_x <- extract_interaction_pvals(isolation_model_poisson)
tab_comb_res_anova_x <- extract_interaction_pvals(combined_model_residential)
tab_comb_mig_anova_x <- extract_interaction_pvals(combined_model_migratory)

### or this way with main effects
tidy_glmmTMB_main_effects <- function(model) {
  
  coef_tbl <- broom.mixed::tidy(model, effects = "fixed") %>%
    dplyr::filter(!grepl(":", term)) %>% 
    dplyr::mutate(
      estimate = round(estimate, 3),
      std.error = round(std.error, 3),
      statistic = round(statistic, 3),
      p.value.formatted = dplyr::case_when(
        p.value < 0.001 ~ "<0.001*",
        p.value < 0.05  ~ paste0(round(p.value, 3), "*"),
        TRUE            ~ as.character(round(p.value, 3))
      )
    )
  
  r2_vals <- performance::r2(model)
  r2_text <- paste0(
    "R² (marginal) = ", round(r2_vals$R2_marginal, 3),
    ", R² (conditional) = ", round(r2_vals$R2_conditional, 3)
  )
  
  coef_tbl %>%
    gt::gt() %>%
    gt::tab_header(
      title = "Model Summary (Main Effects Only)",
      subtitle = r2_text
    ) %>%
    gt::cols_label(
      term = "Predictor",
      estimate = "Estimate",
      std.error = "Std. Error",
      statistic = "z value",
      p.value.formatted = "p value"
    ) %>%
    gt::cols_hide(columns = "p.value")
}

## apply to each model
tab_area_clean_x <- tidy_glmmTMB_main_effects(area_model_poisson)
tab_ghmi_clean_x <- tidy_glmmTMB_main_effects(ghmi_model_poisson)
tab_isolation_clean_x <- tidy_glmmTMB_main_effects(isolation_model_poisson)
tab_comb_res_clean_x <- tidy_glmmTMB_main_effects(combined_model_residential)
tab_comb_mig_clean_x <- tidy_glmmTMB_main_effects(combined_model_migratory)

# save tables as PDFs
gtsave(tab_area_clean_x, "Figures/supplementary/models/area_model_clean_x.pdf")
gtsave(tab_ghmi_clean_x, "Figures/supplementary/models/ghmi_model_clean_x.pdf")
gtsave(tab_isolation_clean_x, "Figures/supplementary/models/isolation_model_clean_x.pdf")
gtsave(tab_comb_res_clean_x, "Figures/supplementary/models/combined_residential_model_clean_x.pdf")
gtsave(tab_comb_mig_clean_x, "Figures/supplementary/models/combined_migratory_model_clean_x.pdf")


### interaction table
interaction_table <- function(model) {
  extract_interaction_pvals(model) %>%
    gt::gt() %>%
    gt::tab_header(
      title = "Global Interaction Tests (Type III Wald χ²)"
    ) %>%
    gt::cols_label(
      term = "Interaction term",
      `Pr(>Chisq)` = "p value"
    )
}

## apply to each model
tab_area_interaction_x <- interaction_table(area_model_poisson)
tab_ghmi_interaction_x <- interaction_table(ghmi_model_poisson)
tab_isolation_interaction_x <- interaction_table(isolation_model_poisson)
tab_comb_res_interaction_x <- interaction_table(combined_model_residential)
tab_comb_mig_interaction_x <- interaction_table(combined_model_migratory)

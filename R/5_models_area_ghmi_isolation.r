# Final Combined Model with all interactions for each variable with season and migratory status

# Load packages
library("dplyr")
library("glmmTMB")
library("emmeans")
library("ggplot2")
library("readr")
library("MASS")
library("ggeffects")
library("patchwork")
library("broom.mixed")
library("car")
library("tidyr")
library("performance")
library("gt")
library("webshot2")
library("sf")

## Load files
greenspaces <- readRDS("Data/final_data_for_big_script.RDS")

### remove the three parks with NAs for GHMI and Isolation
greenspaces <- greenspaces %>% 
  drop_na()

####################
### Exploratory data
####################
# Number of Greenspaces
length(unique(greenspaces$Park_Addre)) #66

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
performance(area_model_poisson) # R^2 (cond.) = 0.952 and R^2 (marg) = 0.905

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
performance(ghmi_model_poisson) # R^2 (cond.) = 0.950 and R^2 (marg) = 0.892

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
performance(isolation_model_poisson) # R^2 (cond.) = 0.950 and R^2 (marg) = 0.891

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
tab_area <- tidy_glmmTMB_signif(area_model_poisson)
tab_ghmi <- tidy_glmmTMB_signif(ghmi_model_poisson)
tab_isolation <- tidy_glmmTMB_signif(isolation_model_poisson)
tab_comb_res <- tidy_glmmTMB_signif(combined_model_residential)
tab_comb_mig <- tidy_glmmTMB_signif(combined_model_migratory)

# save tables as PDFs
gtsave(tab_area, "Figures/supplementary/models/area_model.pdf")
gtsave(tab_ghmi, "Figures/supplementary/models/ghmi_model.pdf")
gtsave(tab_isolation, "Figures/supplementary/models/isolation_model.pdf")
gtsave(tab_comb_res, "Figures/supplementary/models/combined_residential_model.pdf")
gtsave(tab_comb_mig, "Figures/supplementary/models/combined_migratory_model.pdf")

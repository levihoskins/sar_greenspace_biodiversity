### Basically just looking at the data and doing quick visualization to see which model
## only using SAR and nothing else here


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
final_data_for_analysis <- readRDS("Data/AVONET/final_data_for_analysis.RDS")

# Reorder season and analysis for figures
final_data_for_analysis$analysis <- factor(
  final_data_for_analysis$analysis,
  levels = c("residential", "migratory", "total")
)

final_data_for_analysis$Season <- factor(
  final_data_for_analysis$Season,
  levels = c("Overwintering", "Spring Migration", "Breeding", "Fall Migration"),
  ordered = TRUE
)

####################
### Exploratory data
####################
# Number of Greenspaces
length(unique(final_data_for_analysis$Park_Addre)) #69

## size of greensapces
mean(final_data_for_analysis$Shape_Area/10000)
range(final_data_for_analysis$Shape_Area/10000)
sd(final_data_for_analysis$Shape_Area/10000)

### quick visualization of park size median (in red)
ggplot(final_data_for_analysis, aes(x=Shape_Area/10000))+
  geom_histogram(fill="gray80", color="black")+
  geom_vline(xintercept = 19.80487, col = "red")+
  theme_bw()+
  scale_x_log10()

# Quick visualization to show lists and richness 
# Histogram of 'lists'
hist(final_data_for_analysis$number_of_checklists,
     main = "Distribution of Lists",
     xlab = "Number of Lists",
     col = "skyblue")

# Histogram of 'species_richness'
hist(final_data_for_analysis$species_richness,
     main = "Distribution of Species Richness",
     xlab = "Species Richness",
     col = "pink")

###histogram elimantes lm as an option because not normal

### run a simple model to check glmm (poisson or nb)
poisson_glmm <- glm(species_richness ~ log10(Shape_Area) + log10(number_of_checklists), 
                family=poisson, 
                data = final_data_for_analysis)
summary(poisson_glmm)
check_model(poisson_glmm)
###### overdispersed so go with negative binomial

### try negaitve binomial
SAR_nb_glmm <- glmmTMB(species_richness ~ log10(Shape_Area) + log10(number_of_checklists), 
                family=nbinom2, 
                data = final_data_for_analysis)
summary(SAR_nb_glmm)
Anova(SAR_nb_glmm, type = "II")
#### much better fit via AIC and dispersion parameters

#######################################
# GLMM for migration status interaction
#######################################
MR_glmm <- glmmTMB(species_richness ~ log10(Shape_Area) + analysis + log10(number_of_checklists), 
                   family = nbinom2, 
                   data = final_data_for_analysis)
summary(MR_glmm)
Anova(MR_glmm, type = "III")

##################################
# glmm + migration + seasonality
# Adding effort covariates (lists)
##################################
# Fit NB GLMMs with migration_status
## Season × migration_status
glmm_season_status <- glmmTMB(species_richness ~ log10(Shape_Area) + analysis * Season + log10(number_of_checklists),
                              data = final_data_for_analysis,
                              family = nbinom2)
summary(glmm_season_status)
Anova(glmm_season_status, type = "III")

### Validating the model
sim_res_season <- simulateResiduals(glmm_season_status, n = 100)
plot(sim_res_season)
testDispersion(sim_res_season)

# Get emmeans
emm_season_status <- emmeans(glmm_season_status, ~ Season | analysis)
pairs_season_status <- pairs(emm_season_status)

print(emm_season_status)
print(pairs_season_status)

# Convert to df for plotting
season_status_df <- as.data.frame(emm_season_status)

# change to response scale (exponential)
season_status_df <- season_status_df %>%
  mutate(
    emmean_resp = exp(emmean),
    lower_resp = exp(asymp.LCL),
    upper_resp = exp(asymp.UCL)
  )

ggplot(season_status_df,
       aes(x = Season, y = emmean_resp, color = analysis, group = analysis)) +
  geom_point(position = position_dodge(width = 0.25), size = 3.5) +
  geom_errorbar(aes(ymin = lower_resp, ymax = upper_resp),
                position = position_dodge(width = 0.25), width = 0.2, size = 0.7) +
  labs(
    y = "Predicted Species Richness",
    x = NULL,
    color = "Migration Status"
  ) +
  scale_color_manual(values = c(
    "migratory" = "#2c7fb8",
    "residential" = "#feb24c",
    "total" = "#467010"
  )) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(color = "black", linewidth = 0.7),
    axis.ticks = element_line(color = "black"),
    legend.position = "bottom",
    axis.text = element_text(color = "black", size = 11),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 11)
  ) +
  coord_flip()

ggsave('Figures/Migratory_Residential_SAR_emmeans.png', bg = 'transparent')




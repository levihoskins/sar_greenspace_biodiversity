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

# Reorder season categories so that they appear correct when plotted
final_data_for_analysis$Season <- factor(
  final_data_for_analysis$Season,
  levels = c("Overwintering", "Spring Migration", "Breeding", "Fall Migration"),
  ordered = TRUE
)

####################
### Exploratory data
####################
# Number of Greenspaces
length(unique(final_data_for_analysis$Park_Addre)) #89

## size of greensapces
median(final_data_for_analysis$Park_Size_) # 48.93889
range(final_data_for_analysis$Park_Size_) # 5.582461 #364.582466
sd(final_data_for_analysis$Park_Size_) #86.82179

### quick visualization of park size median (in red)
ggplot(final_data_for_analysis, aes(x=Park_Size_))+
  geom_histogram(fill="gray80", color="black")+
  geom_vline(xintercept = 48.93889, col = "red")+
  theme_bw()+
  scale_x_log10()

### Find Mean, SD, Highest, and Lowest SR
mean(final_data_for_analysis$species_richness) #42.17094
sd(final_data_for_analysis$species_richness) #30.69588

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
poisson_glmm <- glm(species_richness ~ log10(Park_Size_) + log10(number_of_checklists), 
                family=poisson, 
                data = final_data_for_analysis)
summary(poisson_glmm)

check_model(poisson_glmm)
###### overdispersed so go with negative binomial

SAR_nb_glmm <- glmmTMB(species_richness ~ log10(Park_Size_) + log10(number_of_checklists), 
                family=nbinom2, 
                data = final_data_for_analysis)
summary(SAR_nb_glmm)
#### much better fit

#######################################
# GLMM for migration status interaction
#######################################
MR_glmm <- glmmTMB(species_richness ~ log10(Park_Size_) * analysis + log10(number_of_checklists), 
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
glmm_season_status <- glmmTMB(species_richness ~ log10(Park_Size_) * analysis * Season + log10(number_of_checklists),
                              data = final_data_for_analysis,
                              family = nbinom2)
summary(glmm_season_status)
Anova(glmm_season_status, type = "III")

# Get emmeans
emm_season_status <- emmeans(glmm_season_status, ~ Season | analysis)
pairs_season_status <- pairs(emm_season_status)

print(emm_season_status)
print(pairs_season_status)

# Convert to df for plotting
season_status_df <- as.data.frame(emm_season_status)

# Plot (Poster)
ggplot(season_status_df,
       aes(x = Season, y = emmean, color = analysis, group = analysis)) +
  geom_point(position = position_dodge(width = 0.3), size = 4) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                position = position_dodge(width = 0.3), width = 0.3) +
  labs(
    title = "Predicted Species Richness by Season & Migration Status",
    y = "Predicted Species Richness",
    x = NULL,
    color = "Migration Status"
  ) +
  scale_color_manual(values = c("migratory" = "#b1d8b7", "residential" = "#2a4c09", "total" = "#467010")) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.ticks = element_line(color = "grey30"),
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black")
    ) +
  coord_flip()

# If your emmeans are on the log scale, exponentiate to get response scale
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
  scale_color_manual(values = c("migratory" = "#2c7fb8", "residential" = "#feb24c", "total" = "#467010")) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),   
    panel.grid.major.x = element_blank(),        
    axis.ticks = element_line(color = "grey30"),
    legend.position = "top",
    axis.text = element_text(color = "black", size = 11),
    axis.title = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 11)
  ) +
  coord_flip()

ggsave('Figures/Migratory_Residential_SAR_emmeans.png', bg = 'transparent')


### Validating the model
# For Season × migration_status model
sim_res_season <- simulateResiduals(glmm_season_status, n = 100)
plot(sim_res_season)
testDispersion(sim_res_season)

### Quick visualization
ggplot(data = final_data_for_analysis, aes(x = Park_Size_, y = species_richness)) +
  geom_point(aes(color = analysis)) +
  geom_smooth(aes(color = analysis), method = 'lm', se = T) + 
  theme_bw() +
  xlab("log Area (m2)") +
  ylab("Total Richness") +
  scale_x_log10()

ggsave('Figures/Migratory_Residential_SR_Area.png', bg = 'transparent')



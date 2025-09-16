# Load packages
library("sf")
library("dplyr")
library("glmmTMB")
library("emmeans")
library("ggplot2")
library("lme4")
library("performance")
library("car")

#### THE ISSUE CAN BE SOLVED IN THIS SCRIPT, OCCURS IN either script 4 or 5 -- need to look into

# Read file
final_avonet <- readRDS("Data/AVONET/final_avonet.RDS")

# Add migration status
migratory_residential <- final_avonet %>%
  mutate(
    migration_status = case_when(
      Migration == 1 ~ "residential",
      Migration %in% c(2, 3) ~ "migratory",
      TRUE ~ NA_character_
    )
  )

# Reorder season categories so that they appear correct when plotted
migratory_residential$Season <- factor(
  migratory_residential$Season,
  levels = c("Overwintering", "Spring Migration", "Breeding", "Fall Migration"),
  ordered = TRUE
)

migratory_residential$migration_status <- factor(
  migratory_residential$migration_status,
  levels = c("residential", "migratory")
)

# Create a clean, distinct dataset for plotting or modeling
clean_avonet <- migratory_residential %>%
  dplyr::select(species_richness, Season, Park_Siz_1, lists, migration_status) %>%
  distinct()

# View the result
head(clean_avonet)

####################
### Exploratory data
####################
# Number of Greenspaces
length(unique(final_avonet$Park_Addre)) #127

# Total number of observers
length(unique(final_avonet$SAMPLING.EVENT.IDENTIFIER)) #55003

## size of greensapces
median(migratory_residential$Park_Size_) #82.79207
range(migratory_residential$Park_Size_) #5.157864 #364.582466
sd(migratory_residential$Park_Size_) #87.81135

### quick visualization of park size median (in red)
ggplot(migratory_residential, aes(x=Park_Size_))+
  geom_histogram(fill="gray80", color="black")+
  geom_vline(xintercept = 82.79207, col = "red")+
  theme_bw()+
  scale_x_log10()

### Find Mean, SD, Highest, and Lowest SR
mean(final_avonet$species_richness) #109.4772
sd(final_avonet$species_richness) #34.26076

## Get name of park
richness_summary <- final_avonet %>%
  st_drop_geometry() %>%
  group_by(Park_Name) %>%
  summarise(total_species = n_distinct(SCIENTIFIC)) %>%
  ungroup()

lowest_SR_park  <- richness_summary %>% filter(total_species == min(total_species))
lowest_SR_park # Water Oaks Park -- 30
highest_SR_park <- richness_summary %>% filter(total_species == max(total_species)) 
highest_SR_park # Bill Baggs Cape Florida State Park - 234

# Quick visualization to show lists and richness 
# Histogram of 'lists'
hist(final_avonet$lists,
     main = "Distribution of Lists",
     xlab = "Number of Lists",
     col = "skyblue")

# Histogram of 'species_richness'
hist(final_avonet$species_richness,
     main = "Distribution of Species Richness",
     xlab = "Species Richness",
     col = "pink")

###histogram elimantes lm as an option because not normal

### run a simple model to check glmm (poisson or nb)
poisson_glmm <- glm(species_richness ~ log10(Park_Size_) + log10(lists), 
                family=poisson, 
                data = final_avonet)
summary(poisson_glmm)

check_model(poisson_glmm)
###### overdispersed so go with negative binomial

nb_glmm <- glmmTMB(species_richness ~ log10(Park_Size_) + log10(lists), 
                family=nbinom2, 
                data = final_avonet)
summary(nb_glmm)

check_model(nb_glmm)
#### much better fit

##############################
# Fitting a model for SAR only
##############################
SAR_glmm <- glmmTMB(species_richness ~ log10(Park_Size_) + log10(lists), 
                family=nbinom2, 
                data = final_avonet)
summary(SAR_glmm)

#######################################
# GLMM for migration status interaction
#######################################
MR_glmm <- glmmTMB(species_richness ~ log10(Park_Size_) * migration_status + log10(lists), 
                   family = nbinom2, 
                   data = migratory_residential)
summary(MR_glmm)
Anova(MR_glmm, type = "II")

##################################
# Glmm + migration + seasonality
# Adding effort covariates (lists)
##################################
# Fit NB GLMMs with migration_status
## Season × migration_status
glmm_season_status <- glmmTMB(species_richness ~ log10(Park_Size_) * migration_status * Season + log10(lists),
                              data = migratory_residential,
                              family = nbinom2)
summary(glmm_season_status)
Anova(glmm_season_status, type = "III")

# Get emmeans
emm_season_status <- emmeans(glmm_season_status, ~ Season | migration_status)
pairs_season_status <- pairs(emm_season_status)

print(emm_season_status)
print(pairs_season_status)

# Convert to df for plotting
season_status_df <- as.data.frame(emm_season_status)

# Plot (Poster)
ggplot(season_status_df,
       aes(x = Season, y = emmean, color = migration_status, group = migration_status)) +
  geom_point(position = position_dodge(width = 0.3), size = 4) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                position = position_dodge(width = 0.3), width = 0.3) +
  labs(
    title = "Predicted Species Richness by Season & Migration Status",
    y = "Predicted Species Richness",
    x = NULL,
    color = "Migration Status"
  ) +
  scale_color_manual(values = c("migratory" = "#b1d8b7", "residential" = "#2a4c09")) +
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
       aes(x = Season, y = emmean_resp, color = migration_status, group = migration_status)) +
  geom_point(position = position_dodge(width = 0.25), size = 3.5) +
  geom_errorbar(aes(ymin = lower_resp, ymax = upper_resp),
                position = position_dodge(width = 0.25), width = 0.2, size = 0.7) +
  labs(
    y = "Predicted Species Richness",
    x = NULL,
    color = "Migration Status"
  ) +
  scale_color_manual(values = c("migratory" = "#2c7fb8", "residential" = "#feb24c")) +
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
ggplot(data = migratory_residential, aes(x = area, y = species_richness)) +
  geom_point(aes(color = migration_status)) +
  geom_smooth(aes(color = migration_status), method = 'lm', se = T) + 
  theme_bw() +
  xlab("log Area (m2)") +
  ylab("Total Richness") +
  scale_x_log10()

ggsave('Figures/Migratory_Residential_SR_Area.png', bg = 'transparent')



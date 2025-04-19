# Load packages
library("lme4")
library("broom.mixed")
library("lmerTest")
library("arm")

# Read the shapefile and remove rows with NAs
final_shapefile_clean <- st_read("Data/Polygons/final_shapefile_clean_saved.shp")

# Rename columns because they decided to change names
final_shapefile_clean <- final_shapefile_clean %>%
  rename(
    COMMON = COMMON,
    SCIENTIFIC = SCIENTI,
    LATITUDE = LATITUD,
    LONGITUDE = LONGITU,
    COUNTY = COUNTY,
    STATE = STATE,
    LOCALITY = LOCALITY,
    L.ID = L_ID,
    L.TYPE = L_TYPE,
    DATE = DATE,
    O.COUNT = O_COUNT,
    OBSERV.ID = OBSERV_,
    SEI = SEI,
    MONTH = MONTH, 
    Shape_Area = Shap_Ar,
    Park_Sourc = Prk_Src,
    Park_Urban = Prk_Urb,
    Park_Place = Prk_Plc,
    Park_Count = Prk_Cnt,
    Park_Addre = Prk_Add,
    Park_Size_ = Prk_Sz_,
    Park_Siz_1 = Prk_S_1,
    Park_Size1 = Prk_Sz1,
    Park_Name = Park_Nm,
    area = area,
    lists = lists,
    geometry = geometry
  )

# Calculate via park, size, month
location_richness <- final_shapefile_clean %>%
  group_by(Park_Addre, MONTH) %>%
  mutate(species_richness = n_distinct(SCIENTIFIC)) %>%
  ungroup()

#############################
# Create a Linear-mixed Model
#############################

# Basic LMM for richness size and month per greenspace
lmm_full <- lmer(species_richness ~ Park_Size_ + MONTH + (1 | Park_Addre), 
                 data = location_richness)

summary(lmm_full)
display(lmm_full)

## Plot fitted vs residual values of model (homogeneity)
plot(lmm_full, add.smooth = FALSE, which = 1)

E <- resid(lmm_full)
hist(E, xlab="Residuals", main="")

# Create a tidy table for the LMM results
model_results1 <- tidy(lmm_full)

# Print the formatted table
print(model_results1)
write.csv(model_results1, "model_results1.csv", row.names = FALSE)

#################################################################
# Species richness drops a lot in summer (Jul, Aug), 
# likely due to ecological or sampling dynamics.
# Species richness increases with park size, but the effect is small.
# There's meaningful variation among parks, justifying the use of a mixed model.
# The model is handling both fixed seasonal effects and
# park-specific differences in richness.
#################################################################

# Fit model with Park_Size * MONTH interaction to show how size impacts via month
lmm_interaction <- lmer(species_richness ~ Park_Size_ * MONTH + (1 | Park_Addre), 
                        data = location_richness)

summary(lmm_interaction)
display(lmm_interaction)

## Plot fitted vs residua values of model (homogeneity)
plot(lmm_interaction, add.smooth = FALSE, which = 1)

## Checking for normality of residuals of null model
R <- resid(lmm_interaction)
hist(R, xlab="Residuals", main="")

# Create a tidy table for the LMM results
model_results2 <- tidy(lmm_interaction)

# Print the formatted table
print(model_results2)
write.csv(model_results2, "model_results2.csv", row.names = FALSE)

#############################################################################
# SPECIES RICHNESS IS NEGATIVELY IMPACTED BY PARK SIZE DURING FALL MIGRATION
# There's a clear seasonal effect on species richness, 
# with months like August and July showing significant drops in richness, 
# possibly due to migration patterns, while October shows a peak in  richness, 
# which could indicate migration or other seasonal dynamics.
#############################################################################

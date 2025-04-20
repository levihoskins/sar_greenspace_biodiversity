# Load packages
library("lme4")
library("broom.mixed")
library("lmerTest")
library("arm")

# Read the shapefile
final_avonet <- st_read("Data/AVONET/final_avonet.shp")

# Rename columns to original names
final_avonet <- final_avonet %>%
  rename(
    COMMON = COMMON, SCIENTIFIC = SCIENTI, LATITUDE = LATITUD, LONGITUDE = LONGITU,
    COUNTY = COUNTY, STATE = STATE, LOCALITY = LOCALITY, L.ID = L_ID, L.TYPE = L_TYPE,
    DATE = DATE, O.COUNT = O_COUNT, OBSERV.ID = OBSERV_, SEI = SEI, MONTH = MONTH, 
    Shape_Area = Shap_Ar, Park_Sourc = Prk_Src, Park_Urban = Prk_Urb,
    Park_Place = Prk_Plc, Park_Count = Prk_Cnt, Park_Addre = Prk_Addr_x,
    Park_Size_ = Prk_Sz_, Park_Siz_1 = Prk_S_1, Park_Size1 = Prk_Sz1,
    Park_Name = Park_Nm, area = area, lists = lists, geometry = geometry,
    species_richness = spcs_rc, Sequence = Sequenc, Family1 = Family1, Order1 = Order1,
    Avibase_ID1 = Avb_ID1, Complete.measures = Cmplt_m, Beak.Length_Culman = Bk_Ln_C,
    Beak.Length_Nares = Bk_Ln_N, Beak.Width = Bk_Wdth, Beak.Depth = Bk_Dpth, 
    Tarsus.Length = Trss_Ln, Wing.Length = Wng_Lng, Kipps.Distance = Kpps_Ds,
    Secondary1 = Scndry1, Hand_Wing.Index = Hnd.W_I, Tail.Length = Tl_Lngt,
    Mass = Mass, Habitat = Habitat, Habitat.Density = Hbtt_Dn, Migration = Migratn,
    Trophic.Level = Trphc_L, Tropic.Niche = Trphc_N, Primary.Lifestyle = Prmry_L,
    Min.Lattitude = Mn_Lttd, Max.Lattitude = Mx_Lttd, Centroid.Lattitude = Cntrd_Lt,
    Centroid.Longitude = Cntrd_Ln, Range.Size = Rang_Sz
  )

migratory_residential <- final_avonet %>%
  mutate(migration_status = case_when(
    Migration == 1 ~ "residential",
    Migration %in% c(2, 3) ~ "migratory",
    TRUE ~ NA_character_  
  ))

#############################
# Create a Linear-mixed Model
#############################

# Basic LMM for richness size and month per greenspace
lmm_full <- lmer(species_richness ~ Park_Size_ + MONTH + (1 | Park_Addre), 
                 data = final_avonet)

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
write.csv(model_results1, "Data/GLM_Results/model_results1.csv", row.names = FALSE)

#################################################################
# Species richness drops a lot in summer (Jul, Aug).
# Species richness increases with park size, but the effect is small.
#################################################################

# Fit model with Park_Size * MONTH interaction to show how size impacts via month
lmm_interaction <- lmer(species_richness ~ Park_Size_ * MONTH + (1 | Park_Addre), 
                        data = final_avonet)

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
write.csv(model_results2, "Data/GLM_Results/model_results2.csv", row.names = FALSE)

#############################################################################
# SPECIES RICHNESS IS NEGATIVELY IMPACTED BY PARK SIZE DURING FALL MIGRATION
# There's a seasonal effect on species richness, 
# with August and July showing significant drops in richness, 
# while October shows a peak in  richness.
#############################################################################

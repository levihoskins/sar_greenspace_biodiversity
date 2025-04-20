# Load packages
library("ggplot2")
library("dplyr")
library("sf")

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

## Separate into two RDS files for Migratory and Residential
migratory_residential <- final_avonet %>%
  mutate(migration_status = case_when(
    Migration == 1 ~ "residential",
    Migration %in% c(2, 3) ~ "migratory",
    TRUE ~ NA_character_  
  ))

### Migratory
migratory_data <- migratory_residential %>%
  filter(migration_status == "migratory")
length(unique(migratory_data$SCIENTIFIC)) #263

### Add richness
migratory_data <- migratory_data %>%
  group_by(Park_Addre, MONTH) %>%
  mutate(migratory_richness = n_distinct(SCIENTIFIC)) %>%
  ungroup()

#### SaveRDS
#saveRDS(migratory_data, "Data/AVONET/migratory_data.rds")

### Residential
residential_data <- migratory_residential %>%
  filter(migration_status == "residential")
length(unique(residential_data$SCIENTIFIC)) #87

### Add richness
residential_data <- residential_data %>%
  group_by(Park_Addre, MONTH) %>%
  mutate(residential_richness = n_distinct(SCIENTIFIC)) %>%
  ungroup()

#### SaveRDS
#saveRDS(residential_data, "Data/AVONET/residential_data.rds")

### GRAPHICAL REPRESENTATION FOR RESIDENTIAL VS MIGRATORY

### Plot between richness and Park size but make Log for MIGRATORY
migratory_data %>%
  group_by(Park_Name) %>%
  summarise(migratory_richness = mean(migratory_richness),
            Park_Size_Ha = mean(Park_Size_)) %>%
  mutate(log_Park_Size_Ha = log(Park_Size_Ha)) %>%
  ggplot(aes(x = log_Park_Size_Ha, y = migratory_richness)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "green", fill = "lightgreen", level = 0.95) +
  stat_poly_eq(
    formula = y ~ x,
    aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")),
    parse = TRUE,
    size = 3,
    label.x = "left",
    label.y = "top"
  ) +
  labs(x = "log(Park Size)", y = "Species Richness") +
  theme_bw() +
  theme(panel.background = element_blank(), 
        plot.background = element_blank(),
        panel.grid = element_blank())

### Plot between richness and Park size but make Log for RESIDENTIAL
residential_data %>%
  group_by(Park_Name) %>%
  summarise(residential_richness = mean(residential_richness),
            Park_Size_Ha = mean(Park_Size_)) %>%
  mutate(log_Park_Size_Ha = log(Park_Size_Ha)) %>%
  ggplot(aes(x = log_Park_Size_Ha, y = residential_richness)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "green", fill = "lightgreen", level = 0.95) +
  stat_poly_eq(
    formula = y ~ x,
    aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")),
    parse = TRUE,
    size = 3,
    label.x = "left",
    label.y = "top"
  ) +
  labs(x = "log(Park Size)", y = "Species Richness") +
  theme_bw() +
  theme(panel.background = element_blank(), 
        plot.background = element_blank(),
        panel.grid = element_blank())

# Load packages
library("ggplot2")
library("dplyr")
library("sf")
library("ggspatial")

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

## Google Earth Engine - Human Modification Index
mean_gHM <- read.csv("Data/GEE/mean_gHM_South_FL.csv")

mean_gHM %>% 
  count(Park_Addre) %>% 
  filter(n > 1)

mean_gHM_clean <- mean_gHM %>%
  group_by(Park_Addre) %>%
  summarise(across(everything(), ~ first(na.omit(.)), .names = "first_{.col}"))

# First, join while keeping all rows from location_richness
combined <- final_avonet %>%
  left_join(mean_gHM_clean, by = "Park_Addre", suffix = c("", ".y"))

# Keep only these columns
filtered_data <- combined %>%
  select(L.ID, lists, COMMON, SCIENTIFIC, LATITUDE, LONGITUDE, geometry, DATE, O.COUNT,
         OBSERV.ID, SEI, MONTH, Shape_Area, Park_Addre, Park_Size_, Park_Size1, Park_Siz_1,
         species_richness, first_system.index, first_GISTrkrID, first_ParkID, 
         first_.geo, first_mean)


# Assuming your data is already in an sf object, which is important for spatial plotting
filtered_data_sf <- st_as_sf(filtered_data)

# Currently this shows species richness and park size, still need land sat variables
ggplot(data = filtered_data_sf) +
  annotation_map_tile(type = "cartolight") +
  geom_sf(aes(size = Park_Size_, color = species_richness), alpha = 0.7) +
  scale_color_viridis_c(name = "Species Richness", option = "plasma") +
  scale_size_continuous(name = "Park Size", range = c(1, 10)) +
  scale_fill_viridis_c(name = "Land Cover Average (first_mean)", option = "inferno") +
  labs(
    title = "",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  coord_sf(datum = st_crs(filtered_data_sf)) 

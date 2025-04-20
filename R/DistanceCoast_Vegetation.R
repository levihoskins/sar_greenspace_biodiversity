# Distance to water

library("nhdplusTools")
library("sf")
library("dplyr")
library("rnaturalearth")
library("rnaturalearthdata")
library("rgee")

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


# Make sure it's in a projected CRS (meters)
final_shapefile_clean <- st_transform(final_shapefile_clean, crs = 3857)

coastline <- ne_download(scale = 10, type = "coastline", category = "physical",
                         returnclass = "sf")

# Also transform to match
coastline <- st_transform(coastline, crs = st_crs(final_shapefile_clean))

# This gives a vector of distances in meters
final_shapefile_clean$dist_to_coast_m <- st_distance(final_shapefile_clean, coastline) %>%
  apply(1, min)

head(final_shapefile_clean$dist_to_coast_m)

####################################
# Google Earth Engine for Vegetation
####################################

# Initialize Earth Engine
ee_Initialize()

# Reproject your shapefile to WGS84 (EE uses EPSG:4326)
final_shapefile_clean <- st_transform(final_shapefile_clean, crs = 4326)

# Convert sf object to Earth Engine FeatureCollection
parks_ee <- sf_as_ee(final_shapefile_clean)

# Load MODIS NDVI collection
modis_ndvi <- ee$ImageCollection("MODIS/006/MOD13Q1")$
  filterDate("2020-01-01", "2020-12-31")$
  select("NDVI")

# Reduce the NDVI collection to a single image (mean NDVI in 2020)
mean_ndvi <- modis_ndvi$mean()

# Extract mean NDVI value for each park polygon
ndvi_per_park <- mean_ndvi$reduceRegions(
  collection = parks_ee,
  reducer = ee$Reducer$mean(),
  scale = 250 
)

# Convert back to sf
ndvi_sf <- ee_as_sf(ndvi_per_park)

# Merge NDVI values into your original sf dataframe
final_shapefile_clean <- final_shapefile_clean %>%
  left_join(ndvi_sf %>% st_drop_geometry() %>% select(Park_Addre, mean), 
            by = "Park_Addre") %>%
  rename(mean_ndvi = mean)
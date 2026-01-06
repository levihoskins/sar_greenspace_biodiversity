### this script is used for adding in GHMI and Isolation.

### some things are coded out, but it's just the save files, so just un# them when you run script.

# Load packages
library(sf)
library(tidyverse)

# Read files
final_data_for_analysis <- readRDS("Data/AVONET/final_data_for_analysis.RDS")
final_shapefile_clean <- readRDS("Data/Intermediate_Data/final_shapefile_clean.RDS")

## Clean up shapefile so that i can combine geometry back into final data frame
final_shapefile_clean <- final_shapefile_clean %>%
  dplyr::select(Park_Addre, geometry) %>%
  group_by(Park_Addre) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

final_data_with_geometry <- final_data_for_analysis %>%
  left_join(final_shapefile_clean, by = "Park_Addre") %>%
  st_as_sf()

## Read in GHMI and Dynamic World
ghmi <- read_csv("Data/GHMI_Dynamic_World/GHMI.csv")

# Clean GHMI
ghmi_clean <- ghmi %>%
  group_by(Park_Addre) %>%
  summarise(ghmi_mean = mean(mean, na.rm = TRUE), .groups = "drop")

# Join with final_avonet
gee_final_data_for_analysis <- final_data_for_analysis %>%
  left_join(ghmi_clean, by = "Park_Addre")

# Reorder season categories so that they appear correct when plotted
gee_final_data_for_analysis$Season <- factor(
  gee_final_data_for_analysis$Season,
  levels = c("Overwintering", "Spring Migration", "Breeding", "Fall Migration"),
  ordered = TRUE
)

gee_final_data_for_analysis$analysis <- factor(
  gee_final_data_for_analysis$analysis,
  levels = c("residential", "migratory", "total")
)

##############
# read in ParkServe data
# we probably want to consider the distance to nearest green space or natural area, not just greenspaces
# included in this study so we will use all the ParkServe polygons
#gs_poly <- st_read("Data/Polygons/ParkServe_Park_SouthFL.shp")

# group by Park_Addre and remove small parks (<0.05ha)
#gs_poly <- gs_poly %>%
#  group_by(Park_Addre) %>%
#  summarise(geometry = st_union(geometry), .groups = "drop") %>%
#  mutate(area=as.numeric(st_area(.)/10000)) %>%
#  filter(area > 0.05)

# Get nearest distance greenspace
#nearest_distance <- numeric(nrow(gs_poly))

#for (i in seq_len(nrow(gs_poly))) {
#  others <- gs_poly[-i, ]
#  distances <- as.numeric(st_distance(gs_poly[i, ], others))
#  nearest_distance[i] <- min(distances[distances > 0])
#}

# add distances to the dataframe
#gs_poly$nearest_dist_m <- nearest_distance
#gs_poly$nearest_dist_km <- nearest_distance / 1000

# make the gs_poly a data frame
#gs_poly_df <- gs_poly %>%
#  as.data.frame()

# save this file
#saveRDS(gs_poly, "Data/Intermediate_Data/distance_to_nearest_greenspace_parkserve.RDS")

gs_poly_df <- readRDS("Data/Intermediate_Data/distance_to_nearest_greenspace_parkserve.RDS")

# now add this to the greenspaces data frame
greenspaces <- gee_final_data_for_analysis %>%
  left_join(., gs_poly_df %>% dplyr::select(Park_Addre, nearest_dist_m, nearest_dist_km), by=c("Park_Addre"))
summary(greenspaces$nearest_dist_m)

#saveRDS(greenspaces, "Data/final_data_for_big_script.RDS")

# play with the data
ggplot(greenspaces, aes(x = log10(nearest_dist_m), y = species_richness, color = analysis)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, size = 1) +
  facet_wrap(~ Season) +
  labs(
    x = "Log10 Distance to Nearest Buffer (m + 1)",
    y = "Species Richness",
    color = "Analysis Type"
  ) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "top",
    strip.background = element_rect(fill = "grey95", color = NA),
    panel.grid = element_blank()
  )

# what does the response look like?
hist(greenspaces$nearest_dist_m)
# it is postively skewed

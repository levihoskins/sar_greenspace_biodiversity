# Load Packages
library("leaflet")
install.packages("htmlwidgets")

# Read the shapefile and remove rows with NAs
final_shapefile_clean <- st_read("Data/Polygons/final_shapefile_clean_saved.shp")

# Calculate via park, size, month
location_richness <- final_shapefile_clean %>%
  group_by(Park_Name, Park_Size_, MONTH) %>%
  summarise(species_richness = n_distinct(COMMON), .groups = 'drop')

location_richness <- location_richness %>%
  filter(Park_Size_ <= 1500)

##### Start Leaflet
# Calculate via park, size, month 
location_richness_area_monthly <- final_shapefile_clean %>%
  group_by(Park_Name, Park_Size_, MONTH) %>%
  summarise(species_richness = n_distinct(COMMON), .groups = 'drop') %>%
  mutate(
    Season = case_when(
      MONTH %in% c("Mar", "Apr", "May") ~ "SM", 
      MONTH %in% c("Jun", "Jul") ~ "S",                                    
      MONTH %in% c("Dec", "Jan", "Feb") ~ "W",                              
      MONTH %in% c("Aug", "Sep", "Oct", "Nov") ~ "FM" 
    )
  )

# Join shp with location richness
final_shapefile_with_richness <- final_shapefile_clean %>%
  st_join(location_richness_area_monthly, by = c("Park_Name", "Park_Size_", "MONTH"))

# Define season colors
season_colors <- c("SM" = "lightgreen", 
                   "S" = "lavender",
                   "W" = "lightblue", 
                   "FM" = "magenta",
                   "Unknown" = "gray") 

# Create a leaflet map
leaflet(final_shapefile_with_richness) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~LONGITUDE, 
    lat = ~LATITUDE, 
    radius = ~sqrt(species_richness) * 2,
    fillOpacity = 0.7,
    popup = ~paste0("<b>", Park_Name.x, "</b><br>",
                    "Species Richness: ", species_richness, "<br>",
                    "Month: ", MONTH.y)
  ) %>%
  addLegend(
    position = "bottomright",
    colors = unname(season_colors), 
    labels = names(season_colors),  
    title = "Seasonal Status"
  )

# Create a color palette based on species richness
color_pal <- colorNumeric(palette = "RdYlBu", domain = final_shapefile_with_richness$species_richness)

leaflet(final_shapefile_with_richness) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~LONGITUDE, 
    lat = ~LATITUDE, 
    radius = ~sqrt(species_richness) * 2, 
    color = ~color_pal(species_richness), 
    fillOpacity = 0.7,
    popup = ~paste0("<b>", Park_Name.x, "</b><br>",
                    "Species Richness: ", species_richness, "<br>",
                    "Month: ", MONTH.y)
  ) %>%
  addLegend(
    position = "bottomright",
    pal = color_pal,
    values = ~species_richness,
    title = "Species Richness"
  )
ggsave('Leaflet.png')

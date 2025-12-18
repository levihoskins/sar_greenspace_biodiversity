# load packages
library(tigris)
library(sf)
library(tidyverse)

#####################
### Figure for poster
#####################

# Load all Florida counties
fl_counties <- counties(state = "FL", cb = TRUE, class = "sf")

# Identify target counties (Broward, MD, and PB)
highlight_counties <- fl_counties %>%
  filter(NAME %in% c("Broward", "Miami-Dade", "Palm Beach"))

# Create a new column for fill color: black for target counties, white for others
fl_counties <- fl_counties %>%
  mutate(fill_color = ifelse(NAME %in% c("Broward", "Miami-Dade", "Palm Beach"), "#006d2c", "white"))

# Plot
ggplot() +
  geom_sf(data = fl_counties, aes(fill = fill_color), color = "black", size = 0.3) +
  scale_fill_identity() +  
  theme_void() +
  theme(
    panel.border = element_blank(),
    plot.margin = margin(0, 0, 0, 0)
  )

# Save as PNG
ggsave(filename = "Figures/fl_counties_map.png", width = 8, height = 6, dpi = 300)

# Read in packages
library("ggplot2")
library("dplyr")
library("readr")
library("tidyr")
library("vegan")
library("lubridate")
library("data.table")

# Read in greenspaces and eBird data


# Richness per LOCALITY.ID
length(unique(all_points$LOCALITY.ID)) #51931

# Perform the join using `inner_join()` for non-spatial data
location_richness <- all_points %>%
  st_join(greenspace_events, by = "LOCALITY.ID") %>%  # Match by LOCALITY.ID
  filter(CATEGORY %in% c("COMMON.NAME", "COMMON.NAME")) %>%
  group_by(Park_Name) %>%
  summarise(species_richness = length(unique(COMMON.NAME)), .groups = "drop")


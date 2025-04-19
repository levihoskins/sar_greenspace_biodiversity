# Load packages
library("ggplot2")
library("dplyr")
library("sf")
library("ggspatial")
library("ggpmisc")

##########################################
## DO NOT FORGET PARK_SIZE_ is in HECTARES
##########################################

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


#################################
### Preliminary Graphs for Poster
#################################

# Line map
### Ensure the MONTH variable is a factor and set the correct order
location_richness$MONTH <- factor(location_richness$MONTH, 
                                  levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                             "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                                  ordered = TRUE)

### Plot with correctly ordered months
### Plot with a continuous color scale for park size
ggplot(location_richness, aes(x = MONTH, y = species_richness, group = Park_Name, color = log(Park_Size_))) +
  geom_line(alpha = 0.7) +
  scale_color_viridis_c(option = "viridis") +
  labs(title = "Species Richness by Season and Month",
       x = "Month",
       y = "Species Richness",
       color = "Park Size") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid = element_blank()) +
  guides(color = guide_colorbar(title = "Park Size"))
ggsave('Figures/Transparent_LineMap_Log.png', bg = 'transparent')

### Location_Richness_Seasonality
location_richness_area_season <- final_shapefile_clean %>%
  group_by(Park_Name, Park_Size_, MONTH) %>%
  summarise(species_richness = n_distinct(SCIENTIFIC), .groups = 'drop') %>%
  mutate(
    Season = case_when(
      MONTH %in% c("Mar", "Apr", "May") ~ "Spring Migration", 
      MONTH %in% c("Jun", "Jul", "Aug") ~ "Breeding",                                    
      MONTH %in% c("Dec", "Jan", "Feb") ~ "Overwintering",                              
      MONTH %in% c("Sep", "Oct", "Nov") ~ "Fall Migration" 
    )
  )

### Graphing Species_richness per season of full-annual cycle
location_richness_area_season %>%
  ggplot(aes(x = log(Park_Size_), y = species_richness, color = Season)) +
  geom_point(alpha = 0.6, size = 3, color = "black") +
  geom_smooth(method = "lm", se = FALSE, size = 1.2) +
  stat_poly_eq(
    formula = y ~ x,
    aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")),
    parse = TRUE,
    size = 3,
    label.x = "left",
    label.y = "top"
  ) +
  scale_color_manual(values = c("Spring Migration" = "lightgreen", 
                                "Breeding" = "lavender",  
                                "Fall Migration" = "pink",  
                                "Overwintering" = "lightblue")) + 
  facet_wrap(~ factor(Season, levels = c("Spring Migration", "Breeding", "Fall Migration", "Overwintering")), 
             scales = "free") +  
  labs(title = "Species Richness across Park Size through Full-annual Cycle",
       x = "log(Park Size)",
       y = "Species Richness",
       color = "Season") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    legend.position = "none"
  )
ggsave('Figures/_Log_Transparent_Species_Richness_Seasonality.png', bg = 'transparent')

### Graphing species Richness across park per month
location_richness_area_season %>%
  ggplot(aes(x = log(Park_Size_), y = species_richness)) +
  geom_point(alpha = 0.6, size = 2, color = "black") +
  geom_smooth(method = "lm", se = FALSE, size = 1.2, color = "lightgreen") +
  stat_poly_eq(
    formula = y ~ x,
    aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")),
    parse = TRUE,
    size = 3,
    label.x = "left",
    label.y = "top"
  ) +
  facet_wrap(~ factor(MONTH, 
                      levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                 "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")), 
             scales = "free") +  
  labs(title = "Species Richness vs. Park Size by Month",
       x = "log(Park Size)",
       y = "Species Richness") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    legend.position = "none"
  )

#########
## EDA ##
#########

## Number of sites
length(unique(location_richness$Park_Addre))

## Test for correlations between richness and checklists (avoiding bias)
hist(location_richness$species_richness)
hist(location_richness$lists)
hist(sqrt(location_richness$lists))

cor.test(location_richness$species_richness, sqrt(location_richness$lists))

## Mean, SD, Range of Species Richness
location_richness$species_richness <- as.numeric(location_richness$species_richness)

summary_richness <- location_richness %>%
  group_by(Park_Name) %>%
  summarise(SR=mean(species_richness)) %>%
  summarise(mean=mean(SR),
            sd=sd(SR),
            min=min(SR),
            max=max(SR))

## Number of data points by location
test <- location_richness %>%
  group_by(Park_Name) %>%
  summarise(N=length(unique(SEI))) %>%
  arrange(N) %>%
  ggplot(., aes(x=Park_Name, y=N)) +
  geom_bar(stat = 'identity') +
  coord_flip()

### Plot between richness and Park size but make Log
location_richness %>%
  group_by(Park_Name) %>%
  summarise(Park_size = mean(Park_Size_),
            species_richness = mean(species_richness)) %>%
  ggplot(., aes(x=species_richness, y=Park_size)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "green", fill = "lightgreen") +
  theme_bw()

### Flip of graph above
location_richness %>%
  group_by(Park_Name) %>%
  summarise(species_richness = mean(species_richness),
            Park_Size_Ha = mean(Park_Size_)) %>%
  ggplot(., aes(x = Park_Size_Ha, y = species_richness)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "green", fill = "lightgreen", level = 0.95) +
  theme_bw()

### Take log of Park_Size_ and rename axises for figure
location_richness %>%
  group_by(Park_Name) %>%
  summarise(species_richness = mean(species_richness),
            Park_Size_Ha = mean(Park_Size_)) %>%
  mutate(log_Park_Size_Ha = log(Park_Size_Ha)) %>%
  ggplot(aes(x = log_Park_Size_Ha, y = species_richness)) +
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
ggsave('Figures/Transparent_Histogram_log.png', bg = 'transparent')

##############################################
# Look at Species Richness per park on average
##############################################

## Take the Standard Deviation per Park for species richness (for error bars)
sd_per_park <- location_richness %>%
  group_by(Park_Addre) %>%
  summarise(species_richness_sd = sd(species_richness, na.rm = TRUE))

## Add back to shapefile
location_richness <- location_richness %>%
  st_join(sd_per_park, by = "Park_Addre")

### Summmarize to one row per park (average them)
park_summary <- location_richness %>%
  group_by(Park_Addre.x) %>%
  summarise(
    mean_richness = mean(species_richness, na.rm = TRUE),
    sd_richness = sd(species_richness, na.rm = TRUE)
  )

#### Plot the summarized data
ggplot(park_summary, aes(x = reorder(Park_Addre.x, mean_richness), y = mean_richness)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_richness - sd_richness, ymax = mean_richness + sd_richness)) +
  theme_bw() +
  ylab("Total species richness") +
  xlab("Greenspace") +
  theme(axis.text.x = element_text(size = 5, color = "black")) +
  theme(axis.text.y = element_text(size = 5, color = "black")) +
  theme(axis.title.x = element_text(size = 12)) +
  theme(axis.title.y = element_text(size = 12)) +
  theme(panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank()) +
  theme(panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank()) +
  coord_flip()
ggsave('Figures/Summarized_Species_Richness_Park_error_bars.png', bg='transparent')
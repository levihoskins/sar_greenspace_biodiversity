# Load packages
library("ggplot2")
library("dplyr")
library("sf")
library("ggspatial")
library("ggpmisc")

##########################################
## DO NOT FORGET PARK_SIZE_ is in HECTARES
##########################################

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

#################################
### Preliminary Graphs for Poster
#################################

# Line map
### Ensure the MONTH variable is a factor and set the correct order
joined_data_clean$MONTH <- factor(joined_data_clean$MONTH, 
                                  levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                             "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                                  ordered = TRUE)

### Plot with correctly ordered months
### Plot with a continuous color scale for park size
ggplot(joined_data_clean, aes(x = MONTH, y = species_richness, group = Park_Name, color = log(Park_Size_))) +
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

### Location_Richness_Seasonality
location_richness_area_season <- joined_data_clean %>%
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

## Number of sites
length(unique(joined_data_clean$Park_Addre))

## Test for correlations between richness and checklists (avoiding bias)
hist(joined_data_clean$species_richness)
hist(joined_data_clean$lists)
hist(sqrt(joined_data_clean$lists))

cor.test(joined_data_clean$species_richness, sqrt(joined_data_clean$lists))

## Mean, SD, Range of Species Richness## Mean, SD,joined_data_clean Range of Species Richness
joined_data_clean$species_richness <- as.numeric(joined_data_clean$species_richness)

summary_richness <- joined_data_clean %>%
  group_by(Park_Name) %>%
  summarise(SR=mean(species_richness)) %>%
  summarise(mean=mean(SR),
            sd=sd(SR),
            min=min(SR),
            max=max(SR))

## Number of data points by location
test <- joined_data_clean %>%
  group_by(Park_Name) %>%
  summarise(N=length(unique(SEI))) %>%
  arrange(N) %>%
  ggplot(., aes(x=Park_Name, y=N)) +
  geom_bar(stat = 'identity') +
  coord_flip()

### Plot between richness and Park size but make Log
joined_data_clean %>%
  group_by(Park_Name) %>%
  summarise(Park_size = mean(Park_Size_),
            species_richness = mean(species_richness)) %>%
  ggplot(., aes(x=species_richness, y=Park_size)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "green", fill = "lightgreen") +
  theme_bw()

### Flip of graph above
joined_data_clean %>%
  group_by(Park_Name) %>%
  summarise(species_richness = mean(species_richness),
            Park_Size_Ha = mean(Park_Size_)) %>%
  ggplot(., aes(x = Park_Size_Ha, y = species_richness)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "green", fill = "lightgreen", level = 0.95) +
  theme_bw()

### Take log of Park_Size_ and rename axises for figure
joined_data_clean %>%
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
sd_per_park <- joined_data_clean %>%
  group_by(Park_Addre) %>%
  summarise(species_richness_sd = sd(species_richness, na.rm = TRUE))

## Add back to shapefile
joined_data_clean <- joined_data_clean %>%
  st_join(sd_per_park, by = "Park_Addre")

### Summmarize to one row per park (average them)
park_summary <- joined_data_clean %>%
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
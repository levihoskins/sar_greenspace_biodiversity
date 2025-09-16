# Load packages
library("ggplot2")
library("dplyr")
library("sf")
library("ggpmisc")

##########################################
## DO NOT FORGET PARK_SIZE_ is in HECTARES
## Park_Siz_1 is in m^2, same as area
##########################################

# Read the RDS
final_avonet <- readRDS("Data/AVONET/final_avonet.RDS")

######################################
### Preliminary Graphs to explore data
######################################

length(unique(final_avonet$Park_Addre)) #127

# Line map
### Ensure the MONTH variable is a factor and set the correct order
final_avonet$MONTH <- factor(final_avonet$MONTH, 
                                  levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                             "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                                  ordered = TRUE)

### Plot with correctly ordered months
### Plot with a continuous color scale for park size
ggplot(final_avonet, aes(x = MONTH, y = species_richness, group = LOCALITY.ID, color = log(Park_Size_))) +
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
location_richness_area_season <- final_avonet %>%
  group_by(Park_Name, Park_Size_, MONTH) %>%
  summarise(species_richness = n_distinct(SCIENTIFIC.NAME), .groups = 'drop') %>%
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
length(unique(final_avonet$Park_Addre)) #127

## Test for correlations between richness and checklists
hist(final_avonet$species_richness)
hist(final_avonet$lists)
hist(sqrt(final_avonet$lists))

cor.test(final_avonet$species_richness, sqrt(final_avonet$lists))

## Mean, SD, Range of Species Richness## Mean, SD,joined_data_clean Range of Species Richness
final_avonet$species_richness <- as.numeric(final_avonet$species_richness)

summary_richness <- final_avonet %>%
  group_by(Park_Name) %>%
  summarise(SR=mean(species_richness)) %>%
  summarise(mean=mean(SR),
            sd=sd(SR),
            min=min(SR),
            max=max(SR))

## Number of data points by location
test <- final_avonet %>%
  group_by(Park_Name) %>%
  summarise(N=length(unique(SAMPLING.EVENT.IDENTIFIER))) %>%
  arrange(N) %>%
  ggplot(., aes(x=Park_Name, y=N)) +
  geom_bar(stat = 'identity') +
  coord_flip()

### Plot between richness and Park size but make Log
final_avonet %>%
  group_by(Park_Name) %>%
  summarise(Park_size = mean(Park_Size_),
            species_richness = mean(species_richness)) %>%
  ggplot(., aes(x=species_richness, y=Park_size)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "green", fill = "lightgreen") +
  theme_bw()

### Flip of graph above
final_avonet %>%
  group_by(Park_Name) %>%
  summarise(species_richness = mean(species_richness),
            Park_Size_Ha = mean(Park_Size_)) %>%
  ggplot(., aes(x = Park_Size_Ha, y = species_richness)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "green", fill = "lightgreen", level = 0.95) +
  theme_bw()

### Take log of Park_Size_ and rename axis for figure
final_avonet %>%
  group_by(Park_Name) %>%
  summarise(species_richness = mean(species_richness),
            Park_Size_Ha = mean(Park_Size_)) %>%
  ggplot(aes(x = Park_Size_Ha, y = species_richness)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "#467010", fill = "#e0f19c", level = 0.95) +
  stat_poly_eq(
    formula = y ~ log10(x), 
    aes(label = paste(..rr.label.., ..p.value.label.., sep = "~~~")),
    parse = TRUE,
    size = 3,
    label.x = "left",
    label.y = "top"
  ) +
  scale_x_log10() +  # log-transform the x-axis
  labs(x = "Park Size (ha, log scale)", y = "Species Richness") +
  theme_bw() +
  theme(panel.background = element_blank(), 
        plot.background = element_blank(),
        panel.grid = element_blank())
#ggsave('Figures/Transparent_Histogram_log.png', bg = 'transparent')


##############################################
# Look at Species Richness per park on average
##############################################

## Take the Standard Deviation per Park for species richness (for error bars)
sd_per_park <- final_avonet %>%
  group_by(Park_Addre) %>%
  summarise(species_richness_sd = sd(species_richness, na.rm = TRUE))

## Add back to original
final_avonet <- final_avonet %>%
  st_join(sd_per_park, by = "Park_Addre")

### Summmarize to one row per park (average them)
park_summary <- final_avonet %>%
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
#ggsave('Figures/Summarized_Species_Richness_Park_error_bars.png', bg = "transparent")

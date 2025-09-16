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
final_data_for_analysis <- readRDS("Data/AVONET/final_data_for_analysis.RDS")

######################################
### Preliminary Graphs to explore data
######################################

length(unique(final_data_for_analysis$Park_Addre))

### Graphing Species_richness per season of full-annual cycle
final_data_for_analysis %>%
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

## Test for correlations between richness and checklists
hist(final_data_for_analysis$species_richness)
hist(final_data_for_analysis$number_of_checklists)
hist(sqrt(final_data_for_analysis$number_of_checklists))

cor.test(final_data_for_analysis$species_richness, sqrt(final_data_for_analysis$number_of_checklists))

## Mean, SD, Range of Species Richness## Mean, SD,joined_data_clean Range of Species Richness
final_data_for_analysis$species_richness <- as.numeric(final_data_for_analysis$species_richness)

summary_richness <- final_data_for_analysis %>%
  group_by(Park_Addre) %>%
  summarise(SR=mean(species_richness)) %>%
  summarise(mean=mean(SR),
            sd=sd(SR),
            min=min(SR),
            max=max(SR))

### Plot between richness and Park size but make Log
final_data_for_analysis %>%
  group_by(Park_Addre) %>%
  summarise(Park_size = mean(Park_Size_),
            species_richness = mean(species_richness)) %>%
  ggplot(., aes(x=species_richness, y=Park_size)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "green", fill = "lightgreen") +
  theme_bw()

### Flip of graph above
final_data_for_analysis %>%
  group_by(Park_Addre) %>%
  summarise(species_richness = mean(species_richness),
            Park_Size_Ha = mean(Park_Size_)) %>%
  ggplot(., aes(x = Park_Size_Ha, y = species_richness)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "green", fill = "lightgreen", level = 0.95) +
  theme_bw()

### Take log of Park_Size_ and rename axis for figure
final_data_for_analysis %>%
  group_by(Park_Addre) %>%
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
#ggsave('Figures/Transparent_Histogram_log_Park_Size_Richness.png', bg = 'transparent')


##############################################
# Look at Species Richness per park on average
##############################################

## Take the Standard Deviation per Park for species richness (for error bars)
sd_per_park <- final_data_for_analysis %>%
  group_by(Park_Addre) %>%
  summarise(species_richness_sd = sd(species_richness, na.rm = TRUE))

## Add back to original
final_data_for_analysis <- final_data_for_analysis %>%
  left_join(sd_per_park, by = "Park_Addre")

### Summmarize to one row per park (average them)
park_summary <- final_data_for_analysis %>%
  group_by(Park_Addre) %>%
  summarise(
    mean_richness = mean(species_richness, na.rm = TRUE),
    sd_richness = sd(species_richness, na.rm = TRUE)
  )

#### Plot the summarized data
ggplot(park_summary, aes(x = reorder(Park_Addre, mean_richness), y = mean_richness)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_richness - sd_richness, ymax = mean_richness + sd_richness)) +
  theme_bw() +
  ylab("Mean species richness") +
  xlab("Greenspace") +
  theme(axis.text.x = element_text(size = 5, color = "black")) +
  theme(axis.text.y = element_text(size = 5, color = "black")) +
  theme(axis.title.x = element_text(size = 12)) +
  theme(axis.title.y = element_text(size = 12)) +
  theme(panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank()) +
  theme(panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank()) +
  coord_flip()
#ggsave('Figures/Summarized_Mean_Species_Richness_Park_error_bars.png', bg = "transparent")

### correlation plot for sampling effort

# Load packages
library(tidyverse)
library(ggcorrplot)

## Load files
greenspaces <- readRDS("Data/final_data_for_big_script.RDS")

### remove the three parks with NAs for GHMI and Isolation
greenspaces <- greenspaces %>% 
  drop_na()

##############################
## standardize predictors   ##
##############################
# log-transform first, then z-transform
greenspaces <- greenspaces %>%
  mutate(
    # log transformed variables
    log_area = log10(Shape_Area),
    log_isolation = log10(nearest_dist_m),
    log_effort = log10(number_of_checklists),
    
    # z-transformed variables
    z_area = as.numeric(scale(log_area)),
    z_isolation = as.numeric(scale(log_isolation)),
    z_ghmi = as.numeric(scale(ghmi_mean)),
    z_effort = as.numeric(scale(log_effort))
  )

####################
# Correlation matrix
####################
# Create park-level dataset
corr_df <- greenspaces %>%
  distinct(
    Park_Addre,
    z_effort,
    z_area,
    z_isolation,
    z_ghmi
  )

# Correlation matrix
corr_matrix <- corr_df %>%
  select(z_effort, z_area, z_isolation, z_ghmi) %>%
  cor(use = "complete.obs", method = "pearson")

# attribute names
colnames(corr_matrix) <- c(
  "Sampling effort",
  "Area",
  "Isolation",
  "Urbanization"
)

rownames(corr_matrix) <- c(
  "Sampling effort",
  "Area",
  "Isolation",
  "Urbanization"
)

corrplot_attributes <- ggcorrplot(corr_matrix, type = "lower", lab = TRUE, lab_size = 5, show.diag = FALSE,
  colors = c("#4575B4", "white", "#D73027"), outline.color = "grey80") +
  theme_bw(base_size = 16) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

# Save as png
ggsave("Figures/supplementary/corrplot_attributes.png", 
       corrplot_attributes, bg = "transparent", width = 8, height = 5)

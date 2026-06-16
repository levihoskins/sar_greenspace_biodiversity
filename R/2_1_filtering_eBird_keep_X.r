# filtering eBird data to remove any outliers, random checklists, incomplete, etc.
## split by month function for full-annual cycle research
### keep checklists with X in this data

# Load packages
library(tidyverse)

# load in eBird data
filtered_br <- readRDS("Data/eBird/RDS/filtered_br.rds")
filtered_md <- readRDS("Data/eBird/RDS/filtered_md.rds")
filtered_pb <- readRDS("Data/eBird/RDS/filtered_pb.rds")

# get rid of any checklists that are not complete and anything greater than 120 minutes
# BR
# Convert DURATION.MINUTES to numeric
filtered_br <- filtered_br %>%
  dplyr::mutate(
    DURATION.MINUTES = as.numeric(ifelse(DURATION.MINUTES == "", NA, DURATION.MINUTES))
  )

# remove incomplete checklists
dat_br_x <- filtered_br %>%
  dplyr::filter(!is.na(DURATION.MINUTES) & DURATION.MINUTES > 5 & DURATION.MINUTES < 240) %>%
  dplyr::filter(!is.na(EFFORT.DISTANCE.KM) & EFFORT.DISTANCE.KM < 5)

# MD
# Convert DURATION.MINUTES to numeric
filtered_md <- filtered_md %>%
  dplyr::mutate(
    DURATION.MINUTES = as.numeric(ifelse(DURATION.MINUTES == "", NA, DURATION.MINUTES))
  )

# remove incomplete checklists
dat_md_x <- filtered_md %>%
  dplyr::filter(!is.na(DURATION.MINUTES) & DURATION.MINUTES > 5 & DURATION.MINUTES < 240) %>%
  dplyr::filter(!is.na(EFFORT.DISTANCE.KM) & EFFORT.DISTANCE.KM < 5)

# PB
# Convert DURATION.MINUTES to numeric
filtered_pb <- filtered_pb %>%
  dplyr::mutate(
    DURATION.MINUTES = as.numeric(ifelse(DURATION.MINUTES == "", NA, DURATION.MINUTES))
  )

# remove incomplete checklists
dat_pb_x <- filtered_pb %>%
  dplyr::filter(!is.na(DURATION.MINUTES) & DURATION.MINUTES > 5 & DURATION.MINUTES < 240) %>%
  dplyr::filter(!is.na(EFFORT.DISTANCE.KM) & EFFORT.DISTANCE.KM < 5)

# add in a month column - month function
dat_br_x <- dat_br_x %>%
  mutate(OBSERVATION.DATE = as.Date(OBSERVATION.DATE)) %>%
  mutate(MONTH = month(OBSERVATION.DATE, label = TRUE, abbr = TRUE))

split_by_month_function <- function(month){
  
  temp <- dat_br_x %>%
    dplyr::filter(MONTH==month)
  
  saveRDS(temp, paste0("Data/eBird/RDS/ebird_data_raw_br_x"))
}
lapply(unique(dat_br_x$MONTH), split_by_month_function)

dat_md_x <- dat_md_x %>%
  mutate(OBSERVATION.DATE = as.Date(OBSERVATION.DATE)) %>%
  mutate(MONTH = month(OBSERVATION.DATE, label = TRUE, abbr = TRUE))

split_by_month_function <- function(month){
  
  temp <- dat_md_x %>%
    dplyr::filter(MONTH==month)
  
  saveRDS(temp, paste0("Data/eBird/RDS/ebird_data_raw_md_x"))
}
lapply(unique(dat_md_x$MONTH), split_by_month_function)

dat_pb_x <- dat_pb_x %>%
  mutate(OBSERVATION.DATE = as.Date(OBSERVATION.DATE)) %>%
  mutate(MONTH = month(OBSERVATION.DATE, label = TRUE, abbr = TRUE))

split_by_month_function <- function(month){
  
  temp <- dat_pb_x %>%
    dplyr::filter(MONTH==month)
  
  saveRDS(temp, paste0("Data/eBird/RDS/ebird_data_raw_pb_x"))
}
lapply(unique(dat_pb_x$MONTH), split_by_month_function)

#saveRDS(dat_br_x, "Data/eBird/RDS/dat_br_x")
#saveRDS(dat_md_x, "Data/eBird/RDS/dat_md_x")
#saveRDS(dat_pb_x, "Data/eBird/RDS/dat_pb_x")

# filtering eBird data to remove any outliers, random checklists, incomplete, etc.

# load in packages
library("dplyr")
library("sf")
library("raster")
library("lubridate")

# load in eBird data
filtered_br <- readRDS("Data/eBird/RDS/filtered_br.rds")
filtered_md <- readRDS("Data/eBird/RDS/filtered_md.rds")
filtered_pb <- readRDS("Data/eBird/RDS/filtered_pb.rds")

# get rid of any checklists that are not complete
x_filtered_br <- filtered_br %>%
  dplyr::filter(OBSERVATION.COUNT=="X") %>%
  dplyr::select(SAMPLING.EVENT.IDENTIFIER) %>%
  distinct()

dat_br <- filtered_br %>%
  dplyr::filter(!SAMPLING.EVENT.IDENTIFIER %in% x_filtered_br$SAMPLING.EVENT.IDENTIFIER)

x_filtered_md <- filtered_md %>%
  dplyr::filter(OBSERVATION.COUNT=="X") %>%
  dplyr::select(SAMPLING.EVENT.IDENTIFIER) %>%
  distinct()

dat_md <- filtered_md %>%
  dplyr::filter(!SAMPLING.EVENT.IDENTIFIER %in% x_filtered_md$SAMPLING.EVENT.IDENTIFIER)

x_filtered_pb <- filtered_pb %>%
  dplyr::filter(OBSERVATION.COUNT=="X") %>%
  dplyr::select(SAMPLING.EVENT.IDENTIFIER) %>%
  distinct()

dat_pb <- filtered_pb %>%
  dplyr::filter(!SAMPLING.EVENT.IDENTIFIER %in% x_filtered_pb$SAMPLING.EVENT.IDENTIFIER)

#saveRDS(dat_br, "Data/eBird/RDS/dat_br")
#saveRDS(dat_md, "Data/eBird/RDS/dat_md")
#saveRDS(dat_pb, "Data/eBird/RDS/dat_pb")

# monhtly function
dat_br <- dat_br %>%
  mutate(OBSERVATION.DATE = as.Date(OBSERVATION.DATE)) %>%
  mutate(MONTH = month(OBSERVATION.DATE, label = TRUE, abbr = TRUE))

split_by_month_function <- function(month){
  
  temp <- dat_br %>%
    dplyr::filter(MONTH==month)
  
  saveRDS(temp, paste0("Data/eBird/RDS/ebird_data_raw_br"))
}
lapply(unique(dat_br$MONTH), split_by_month_function)

dat_md <- dat_md %>%
  mutate(OBSERVATION.DATE = as.Date(OBSERVATION.DATE)) %>%
  mutate(MONTH = month(OBSERVATION.DATE, label = TRUE, abbr = TRUE))

split_by_month_function <- function(month){
  
  temp <- dat_md %>%
    dplyr::filter(MONTH==month)
  
  saveRDS(temp, paste0("Data/eBird/RDS/ebird_data_raw_md"))
}
lapply(unique(dat_md$MONTH), split_by_month_function)

dat_pb <- dat_pb %>%
  mutate(OBSERVATION.DATE = as.Date(OBSERVATION.DATE)) %>%
  mutate(MONTH = month(OBSERVATION.DATE, label = TRUE, abbr = TRUE))

split_by_month_function <- function(month){
  
  temp <- dat_pb %>%
    dplyr::filter(MONTH==month)
  
  saveRDS(temp, paste0("Data/eBird/RDS/ebird_data_raw_pb"))
}
lapply(unique(dat_pb$MONTH), split_by_month_function)


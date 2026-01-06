# download eBird data
## make dataframe (RDS)

# Load packages
library(tidyverse)

# load eBird data
# read the txt file and covert to csv
#br <- read.table("Data/eBird/species_BR.txt", sep = "\t", header = TRUE, stringsAsFactors = FALSE, fill = TRUE)
#write.csv(br, "Data/eBird/species_BR.csv", row.names = FALSE)
#saveRDS(br, "Data/eBird/species_BR.rds")
#md <- read.table("Data/eBird/species_MD.txt", sep = "\t", header = TRUE, stringsAsFactors = FALSE, fill = TRUE)
#write.csv(md, "Data/eBird/species_MD.csv", row.names = FALSE)
#saveRDS(md, "Data/eBird/species_MD.rds")
#pb <- read.table("Data/eBird/species_PB.txt", sep = "\t", header = TRUE, stringsAsFactors = FALSE, fill = TRUE)
#write.csv(pb, "Data/eBird/species_PB.csv", row.names = FALSE)
#saveRDS(pb, "Data/eBird/species_PB.rds")

#read in csv
br <-readRDS("Data/eBird/species_BR.rds")
md <-readRDS("Data/eBird/species_MD.rds")
pb <-readRDS("Data/eBird/species_PB.rds")

# convert to numeric matrix
br <- br %>%
  mutate(
    LATITUDE = as.numeric(LATITUDE),
    LONGITUDE = as.numeric(LONGITUDE)
  )
md <- md %>%
  mutate(
    LATITUDE = as.numeric(LATITUDE),
    LONGITUDE = as.numeric(LONGITUDE)
  )
pb <- pb %>%
  mutate(
    LATITUDE = as.numeric(LATITUDE),
    LONGITUDE = as.numeric(LONGITUDE)
  )

# select eBird data for necessary information
filtered_br <- br %>%
  dplyr::select(COMMON.NAME, SCIENTIFIC.NAME, LATITUDE, LONGITUDE,
         COUNTY, LOCALITY, LOCALITY.ID, LOCALITY.TYPE,
         OBSERVATION.DATE, OBSERVATION.COUNT, OBSERVER.ID,
         SAMPLING.EVENT.IDENTIFIER, DURATION.MINUTES, EFFORT.DISTANCE.KM)

filtered_md <- md %>%
  dplyr::select(COMMON.NAME, SCIENTIFIC.NAME, LATITUDE, LONGITUDE,
                COUNTY, LOCALITY, LOCALITY.ID, LOCALITY.TYPE,
                OBSERVATION.DATE, OBSERVATION.COUNT, OBSERVER.ID,
                SAMPLING.EVENT.IDENTIFIER, DURATION.MINUTES, EFFORT.DISTANCE.KM)

filtered_pb <- pb %>%
  dplyr::select(COMMON.NAME, SCIENTIFIC.NAME, LATITUDE, LONGITUDE,
                COUNTY, LOCALITY, LOCALITY.ID, LOCALITY.TYPE,
                OBSERVATION.DATE, OBSERVATION.COUNT, OBSERVER.ID,
                SAMPLING.EVENT.IDENTIFIER, DURATION.MINUTES, EFFORT.DISTANCE.KM)

# make them numeric matrices
filtered_br <- filtered_br %>%
  filter(!is.na(LONGITUDE) & !is.na(LATITUDE))
filtered_br <- filtered_br %>%
  dplyr::mutate(
    LATITUDE = as.numeric(LATITUDE),
    LONGITUDE = as.numeric(LONGITUDE)
  )

filtered_md <- filtered_md %>%
 filter(!is.na(LONGITUDE) & !is.na(LATITUDE))
filtered_md <- filtered_md %>%
 dplyr::mutate(
    LATITUDE = as.numeric(LATITUDE),
    LONGITUDE = as.numeric(LONGITUDE)
  )

filtered_pb <- filtered_pb %>%
  filter(!is.na(LONGITUDE) & !is.na(LATITUDE))
filtered_pb <- filtered_pb %>%
  dplyr::mutate(
    LATITUDE = as.numeric(LATITUDE),
    LONGITUDE = as.numeric(LONGITUDE)
  )

#saveRDS(filtered_br, "Data/eBird/RDS/filtered_br.rds")
#saveRDS(filtered_md, "Data/eBird/RDS/filtered_md.rds")
#saveRDS(filtered_pb, "Data/eBird/RDS/filtered_pb.rds")

# download eBird data

library("tidyverse")
library("dplyr")
library("sf")
library("raster")
library("lubridate")

# load eBird data
# read the txt file and covert to csv
#br <- read.table("Data/eBird/species_BR.txt", sep = "\t", header = TRUE, stringsAsFactors = FALSE, fill = TRUE)
#write.csv(br, "Data/eBird/species_BR.csv", row.names = FALSE)
#md <- read.table("Data/eBird/species_MD.txt", sep = "\t", header = TRUE, stringsAsFactors = FALSE, fill = TRUE)
#write.csv(md, "Data/eBird/species_MD.csv", row.names = FALSE)
#pb <- read.table("Data/eBird/species_PB.txt", sep = "\t", header = TRUE, stringsAsFactors = FALSE, fill = TRUE)
#write.csv(pb, "Data/eBird/species_PB.csv", row.names = FALSE)

#read in csv
br <-read.csv("Data/eBird/species_BR.csv")
md <-read.csv("Data/eBird/species_MD.csv")
pb <-read.csv("Data/eBird/species_PB.csv")

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

# select eBird data for needed information
filtered_br <- br %>%
  dplyr::select(COMMON.NAME, SCIENTIFIC.NAME, LATITUDE, LONGITUDE,
         COUNTY, STATE, LOCALITY, LOCALITY.ID, LOCALITY.TYPE,
         OBSERVATION.DATE, OBSERVATION.COUNT, OBSERVER.ID,
         SAMPLING.EVENT.IDENTIFIER)

filtered_md <- md %>%
  dplyr::select(COMMON.NAME, SCIENTIFIC.NAME, LATITUDE, LONGITUDE,
                COUNTY, STATE, LOCALITY, LOCALITY.ID, LOCALITY.TYPE,
                OBSERVATION.DATE, OBSERVATION.COUNT, OBSERVER.ID,
                SAMPLING.EVENT.IDENTIFIER)

filtered_pb <- pb %>%
  dplyr::select(COMMON.NAME, SCIENTIFIC.NAME, LATITUDE, LONGITUDE,
                COUNTY, STATE, LOCALITY, LOCALITY.ID, LOCALITY.TYPE,
                OBSERVATION.DATE, OBSERVATION.COUNT, OBSERVER.ID,
                SAMPLING.EVENT.IDENTIFIER)

# make them numeric matrices
filtered_br <- filtered_br %>%
  filter(!is.na(LONGITUDE) & !is.na(LATITUDE))
filtered_br <- filtered_br %>%
  mutate(
    LATITUDE = as.numeric(LATITUDE),
    LONGITUDE = as.numeric(LONGITUDE)
  )

filtered_md <- filtered_md %>%
 filter(!is.na(LONGITUDE) & !is.na(LATITUDE))
filtered_md <- filtered_md %>%
 mutate(
    LATITUDE = as.numeric(LATITUDE),
    LONGITUDE = as.numeric(LONGITUDE)
  )

filtered_pb <- filtered_pb %>%
  filter(!is.na(LONGITUDE) & !is.na(LATITUDE))
filtered_pb <- filtered_pb %>%
  mutate(
    LATITUDE = as.numeric(LATITUDE),
    LONGITUDE = as.numeric(LONGITUDE)
  )

# convert eBird data to spatial data
points_sf <- filtered_br %>%
  dplyr::select(LATITUDE, LONGITUDE, SAMPLING.EVENT.IDENTIFIER) %>%
  distinct() %>%
  st_as_sf(coords=c("LONGITUDE", "LATITUDE"), crs=2237) #use 4326 if wish to see general
#coordinates(filtered_br) <- ~LONGITUDE + LATITUDE

points_sf <- filtered_md %>%
  dplyr::select(LATITUDE, LONGITUDE, SAMPLING.EVENT.IDENTIFIER) %>%
  distinct() %>%
  st_as_sf(coords=c("LONGITUDE", "LATITUDE"), crs=2237) #use 4326 if wish to see general
#coordinates(filtered_md) <- ~LONGITUDE + LATITUDE

points_sf <- filtered_pb %>%
  dplyr::select(LATITUDE, LONGITUDE, SAMPLING.EVENT.IDENTIFIER) %>%
  distinct() %>%
  st_as_sf(coords=c("LONGITUDE", "LATITUDE"), crs=2237) #use 4326 if wish to see general
#coordinates(filtered_pb) <- ~LONGITUDE + LATITUDE

#saveRDS(filtered_br, "Data/eBird/RDS/filtered_br.rds")
#saveRDS(filtered_md, "Data/eBird/RDS/filtered_md.rds")
#saveRDS(filtered_pb, "Data/eBird/RDS/filtered_pb.rds")

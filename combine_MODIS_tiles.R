
library(readr)

# combine .csv MODIS-AQUA data
### RUN THIS for EACH DAY ##############################################


# setwd("Z:/_SHARED_FOLDERS/Dust_Event_UAE_2015/MODIS_10km/TERRA/.....")
filenames_tiles <- list.files(pattern = "\\.csv$")

LAT = NULL
LON = NULL
aod = NULL


## Bind all data together 
for (i in 1:length(filenames_tiles)) {
  lon <- read_csv(filenames_tiles[i])[,1]
  lat <- read_csv(filenames_tiles[i])[,2]
  AOD <- read_csv(filenames_tiles[i])[,3]
  LON = rbind(LON, data.frame(lon))
  LAT = rbind(LAT, data.frame(lat))
  aod = rbind(aod, data.frame(AOD))
}

MODIS04_data <- cbind(LON, LAT, aod)
MODIS04_data <- subset(MODIS04_data, !is.na(values) & !lat == -999 & !lon == -999)

write.csv(MODIS04_data, paste0("AOD_MOD04_93_10km_UAE.csv"))


# combine .csv MODIS-TERRA data
# setwd("Z:/_SHARED_FOLDERS/Dust_Event_UAE_2015/MODIS_10km/AQUA/.......")
filenames_tiles <- list.files(pattern = "\\.csv$")

LAT = NULL
LON = NULL
aod = NULL


## Bind all data together 
for (i in 1:length(filenames_tiles)) {
  lon <- read_csv(filenames_tiles[i])[,1]
  lat <- read_csv(filenames_tiles[i])[,2]
  AOD <- read_csv(filenames_tiles[i])[,3]
  LON = rbind(LON, data.frame(lon))
  LAT = rbind(LAT, data.frame(lat))
  aod = rbind(aod, data.frame(AOD))
}

MODIS04_data <- cbind(LON, LAT, aod)
MODIS04_data <- subset(MODIS04_data, !is.na(values) & !lat == -999 & !lon == -999)

write.csv(MODIS04_data, paste0("AOD_MOY04_93_10km_UAE.csv"))


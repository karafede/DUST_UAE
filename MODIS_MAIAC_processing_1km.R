
library(stringr)
library(plyr)
library(dplyr)
library(threadr)
library(gdalUtils)
library(rgdal)
library(raster)
library(RNetCDF)
library(gstat)
library(leaflet)
library(webshot)
library(htmlwidgets)
library(readr)


### data ectraction from HDF files -----------------------------------------------

#############################################
#### LONGITUDE and LATITUDE #################
#############################################

wd_LATLON <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/LatLon"
setwd(wd_LATLON)
getwd()

  # list files ------------------------------------------------------------------
  filenames <- list.files(pattern = "\\.hdf$")
  
  # dir <- "/disk3/fkaragulian/MODIS_AOD/UAE_boundary"
  # ### shapefile for UAE
  # shp_UAE <- readOGR(dsn = dir, layer = "uae_emirates")
  # 
  # # ----- Transform to EPSG 4326 - WGS84 (required)
  # shp_UAE <- spTransform(shp_UAE, CRS("+init=epsg:4326"))
  # # names(shp)
  # 
  # shp_UAE@data$name <- 1:nrow(shp_UAE)
  # # plot(shp_UAE)
  
  
  # # make a function to generate all .csv files with Lon, lat and value ##----
  ######-----------------------------------------------------------------------
  
  
 #  file <- filenames[1]
    
  
  extract_HDF_LatLon <- function (file) {      ## this is the filenames 
 
    nome <- str_sub(file, start = 1, end = -5)
# get list of field names.....please wait untit it open the phyton libraries from the PC (need to have QGis with Gdal utilities installed)
    sds <- get_subdatasets(file)
    
    lat <- sds[1]  # latitude @1km
    filename <- rasterTmpFile()
    extension(filename) <- 'tif'
    gdal_translate(sds[1], dst_dataset = filename) 
    lat <- raster(filename)
    
    # data values for latitude
    latitude <- rasterToPoints(lat)  
    colnames(latitude) <- c("x", "y", "lat")
    latitude <- as.data.frame (latitude)
    
    
    lon <- sds[2] # longitude  @1km
    filename <- rasterTmpFile()
    extension(filename) <- 'tif'
    gdal_translate(sds[2], dst_dataset = filename) 
    
    lon <- raster(filename)
    # data values for longitude
    longitude <- rasterToPoints(lon)  
    colnames(longitude) <- c("x", "y", "lon")
    longitude <- as.data.frame (longitude)
    
    # Join  lat, lon 
    Lat_Lon <- longitude %>% 
      inner_join(latitude, c("x", "y"))

    write.csv(Lat_Lon, file = paste(nome,".csv", sep = ""), row.names=FALSE)
    
  }  
  

  BBB <- lapply(filenames, extract_HDF_LatLon)

  ######################################################################################
  # collate the tiles for LAT & LON together ####-------------------------------------
  
  filenames_tiles <- list.files(pattern = "\\.csv$")
  
  LAT = NULL
  LON = NULL
  X = NULL
  Y = NULL
  
  
  # i <- 1
  
  ## Bind all data together 
  for (i in 1:length(filenames_tiles)) {
     x <- read_csv(filenames_tiles[i])[,1]
     y <- read_csv(filenames_tiles[i])[,2]
     lon <- read_csv(filenames_tiles[i])[,3]
     lat <- read_csv(filenames_tiles[i])[,4]
     X = rbind(X, data.frame(x))
     Y = rbind(Y, data.frame(y))
     LON = rbind(LON, data.frame(lon))
     LAT = rbind(LAT, data.frame(lat))
  }
  
  X_Y_LAT_LON <- cbind(X, Y, LON, LAT)
  
  write_csv(X_Y_LAT_LON, "X_Y_Lat_Lon_ME_MAIAC_1km.csv")
  

  
  
  ####################################################################################
  ####################################################################################
  ####################################################################################
  ### data ectraction from HDF files -----------------------------------------------
  
  ############################################################
  #### AEROSOSL OPTICAL DEPTH (AOD) AQUA #####################
  ###########################################################
  
  ################
  ## MODIS AQUA ##
  ################
  
wd_AOD <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/AQUA"
  setwd(wd_AOD)
getwd()

# list directories
DAYS_AQUA <- str_sub(list.dirs(), start = 3, end = -1)
DAYS_AQUA <- DAYS_AQUA[-1]


#-----START of the LOOP for all files------------------------

# i <- 3

for (i in 1:length(DAYS_AQUA)) {
  date <- DAYS_AQUA[i]
  setwd(paste0(wd_AOD,"/",DAYS_AQUA[i]))
  filenames_AQUA <- list.files(pattern = "\\.hdf$") 
  
#  file <- filenames_AQUA[i]  
  
  extract_HDF_AOD <- function (file) {      ## this is the filenames 
    
    nome <- str_sub(file, start = 1, end = -5)
    # get list of field names.....please wait untit it open the phyton libraries from the PC (need to have QGis with Gdal utilities installed)
    sds <- get_subdatasets(file)
    
    AOD <- sds[2]  # AOD at 550 nm (1km)
    filename <- rasterTmpFile()
    extension(filename) <- 'tif'
    gdal_translate(sds[2], dst_dataset = filename) 
    AOD <- raster(filename)
    
    # data values for AOD
    AOD <- rasterToPoints(AOD)  
    colnames(AOD) <- c("x", "y", "AOD")
    AOD <- as.data.frame(AOD)
  
    write.csv(AOD, file = paste(nome,".csv", sep = ""), row.names=FALSE)
  
}



CCC <- lapply(filenames_AQUA, extract_HDF_AOD)

  
# collate the tiles for AOD-AQUA together ####-------------------------------------

filenames_tiles <- list.files(pattern = "\\.csv$")

AOD = NULL
X = NULL
Y = NULL

# i <- 1

## Bind all data together 
for (i in 1:length(filenames_tiles)) {
  x <- read_csv(filenames_tiles[i])[,1]
  y <- read_csv(filenames_tiles[i])[,2]
  aod <- read.csv(filenames_tiles[i])[,3]
  X = rbind(X, data.frame(x))
  Y = rbind(Y, data.frame(y))
  AOD = rbind(AOD, data.frame(aod))
}


  X_Y_AOD <- cbind(X, Y, AOD)
  write_csv(X_Y_AOD, paste0("X_Y_AOD_ME_MAIAC_1km","_",date,"_AQUA.csv"))

}



###################################################################
#### JOIN Latitude and Longitude with AOD data for MODIS AQUA #####
###################################################################
  
X_Y_LAT_LON <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/LatLon/X_Y_Lat_Lon_ME_MAIAC_1km.csv")
X_Y_LAT_LON$x <- round(X_Y_LAT_LON$x, digits=0)
X_Y_LAT_LON$y <- round(X_Y_LAT_LON$y, digits=0)

wd_AOD <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/AQUA"
setwd(wd_AOD)


# list directories
DAYS_AQUA <- str_sub(list.dirs(), start = 3, end = -1)
DAYS_AQUA <- DAYS_AQUA[-1]

# i <- 1

for (i in 1:length(DAYS_AQUA)) {
  date <- DAYS_AQUA[i]
  setwd(paste0(wd_AOD,"/",DAYS_AQUA[i]))
  X_Y_AOD_AQUA <- read_csv(paste0("X_Y_AOD_ME_MAIAC_1km_", date, "_AQUA.csv")) 
  X_Y_AOD_AQUA$x <- round(X_Y_AOD_AQUA$x, digits=0)
  X_Y_AOD_AQUA$y <- round(X_Y_AOD_AQUA$y, digits=0)
  
  
  ### comvert from km to lat lon-------------
  

# # join lat lon with AOD
 AOD <- X_Y_LAT_LON %>% 
   join(X_Y_AOD_AQUA, by = c("x", "y"))  
 
AOD <- na.omit(AOD)

AOD <- AOD[,3:5]


write_csv(AOD, paste0("AOD_LAT_LON_1km_", date, "_AQUA.csv"))

}
  
  
  
###############################################################################################
###############################################################################################
###############################################################################################
###############################################################################################
###############################################################################################
###############################################################################################
###############################################################################################
###############################################################################################
###############################################################################################


#############################################################
#### AEROSOSL OPTICAL DEPTH (AOD) TERRA #####################
#############################################################

################
## MODIS TERRA ##
################

wd_AOD <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/TERRA"
setwd(wd_AOD)
getwd()

# list directories
DAYS_TERRA <- str_sub(list.dirs(), start = 3, end = -1)
DAYS_TERRA <- DAYS_TERRA[-1]


#-----START of the LOOP for all files------------------------

# i <- 3

for (i in 1:length(DAYS_TERRA)) {
  date <- DAYS_TERRA[i]
  setwd(paste0(wd_AOD,"/",DAYS_TERRA[i]))
  filenames_TERRA <- list.files(pattern = "\\.hdf$") 
  
  #  file <- filenames_AQUA[i]  
  
  extract_HDF_AOD <- function (file) {      ## this is the filenames 
    
    nome <- str_sub(file, start = 1, end = -5)
    # get list of field names.....please wait untit it open the phyton libraries from the PC (need to have QGis with Gdal utilities installed)
    sds <- get_subdatasets(file)
    
    AOD <- sds[2]  # AOD at 550 nm (1km)
    filename <- rasterTmpFile()
    extension(filename) <- 'tif'
    gdal_translate(sds[2], dst_dataset = filename) 
    AOD <- raster(filename)
    
    # data values for AOD
    AOD <- rasterToPoints(AOD)  
    colnames(AOD) <- c("x", "y", "AOD")
    AOD <- as.data.frame(AOD)
    
    write.csv(AOD, file = paste(nome,".csv", sep = ""), row.names=FALSE)
    
  }
  
  
  
  CCC <- lapply(filenames_TERRA, extract_HDF_AOD)
  
  
  # collate the tiles for AOD-AQUA together ####-------------------------------------
  
  filenames_tiles <- list.files(pattern = "\\.csv$")
  
  AOD = NULL
  X = NULL
  Y = NULL
  
  # i <- 1
  
  ## Bind all data together 
  for (i in 1:length(filenames_tiles)) {
    x <- read_csv(filenames_tiles[i])[,1]
    y <- read_csv(filenames_tiles[i])[,2]
    aod <- read.csv(filenames_tiles[i])[,3]
    X = rbind(X, data.frame(x))
    Y = rbind(Y, data.frame(y))
    AOD = rbind(AOD, data.frame(aod))
  }
  
  
  X_Y_AOD <- cbind(X, Y, AOD)
  write_csv(X_Y_AOD, paste0("X_Y_AOD_ME_MAIAC_1km","_",date,"_TERRA.csv"))
  
}



###################################################################
#### JOIN Latitude and Longitude with AOD data for MODIS AQUA #####
###################################################################

X_Y_LAT_LON <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/LatLon/X_Y_Lat_Lon_ME_MAIAC_1km.csv")
X_Y_LAT_LON$x <- round(X_Y_LAT_LON$x, digits=0)
X_Y_LAT_LON$y <- round(X_Y_LAT_LON$y, digits=0)

wd_AOD <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/TERRA"
setwd(wd_AOD)


# list directories
DAYS_TERRA <- str_sub(list.dirs(), start = 3, end = -1)
DAYS_TERRA <- DAYS_TERRA[-1]

# i <- 1

for (i in 1:length(DAYS_TERRA)) {
  date <- DAYS_TERRA[i]
  setwd(paste0(wd_AOD,"/",DAYS_TERRA[i]))
  X_Y_AOD_TERRA <- read_csv(paste0("X_Y_AOD_ME_MAIAC_1km_", date, "_TERRA.csv")) 
  X_Y_AOD_TERRA$x <- round(X_Y_AOD_TERRA$x, digits=0)
  X_Y_AOD_TERRA$y <- round(X_Y_AOD_TERRA$y, digits=0)
  
  
  ### comvert from km to lat lon-------------
  
  
  # # join lat lon with AOD
  AOD <- X_Y_LAT_LON %>% 
    join(X_Y_AOD_TERRA, by = c("x", "y"))  
  
  AOD <- na.omit(AOD)
  
  AOD <- AOD[,3:5]
  
  
  write_csv(AOD, paste0("AOD_LAT_LON_1km_", date, "_TERRA.csv"))
  
}



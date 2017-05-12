
library(readr)
library(sp)
library(raster)
library(gstat)
library(rgdal)
library(RNetCDF)
library(ncdf4)
library(stringr)

library(raster)
library(leaflet)
library(htmlwidgets)

# setwd("D:/Dust_Event_UAE_2015/WRF_trial_runs")
setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs")

# get extent from WRFChem output-----------------------------------------------
# use the raster stack with all the WRF_chem output

# get only one image
WRF_STACK_image <- raster("DUST_WRFChem_02_April_2015.tif", band = 2)
plot(WRF_STACK_image)

# get the extent of the raster
e <- extent(WRF_STACK_image)
# make a spatial polygon from the extent
p <- as(e, "SpatialPolygons")
plot(p)
crs(p) <- "proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# save shp file for the rectangular DOMAIN from WRFChem
setwd("D:/Dust_Event_UAE_2015/WRFChem_domain")
shapefile(p, "domain_d01_WRFChem.shp", overwrite=TRUE)


#########################################################################
#########################################################################

#### importing the UAE shapefile to use as a masking 
dir <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/website_MODIS/UAE_boundary"
### shapefile for UAE
shp_UAE <- readOGR(dsn = dir, layer = "uae_emirates")

# ----- Transform to EPSG 4326 - WGS84 (required)
shp_UAE <- spTransform(shp_UAE, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
# names(shp)

shp_UAE@data$name <- 1:nrow(shp_UAE)
plot(shp_UAE)

########################################################################
########################################################################


#### importing the domain shapefile to use as a masking 
dir <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRFChem_domain"
shp_WRF <- readOGR(dsn = dir, layer = "domain_d01_WRFChem")


shp_WRF@data$name <- 1:nrow(shp_WRF)
plot(shp_WRF)
plot(shp_UAE, add=TRUE, lwd=1)



########################################################################
########################################################################
############## KRIGING #################################################
########################################################################

##########
# AQUA ###
##########



wd_AOD <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/AQUA"
wd_AOD <- "D:/Dust_Event_UAE_2015/MAIAC_1km/AQUA"
setwd(wd_AOD)


# list directories
DAYS_AQUA <- str_sub(list.dirs(), start = 3, end = -1)
DAYS_AQUA <- DAYS_AQUA[-1]

# i <- 1

for (i in 1:length(DAYS_AQUA)) {
  date <- DAYS_AQUA[i]
  setwd(paste0(wd_AOD,"/",DAYS_AQUA[i]))
  AOD_AQUA <- read_csv(paste0("AOD_LAT_LON_1km_", date, ".csv")) 

# setwd("Z:/_SHARED_FOLDERS/Dust_Event_UAE_2015/MODIS_10km/AQUA")
# setwd("D:/Dust_Event_UAE_2015/MODIS_10km/AQUA")
# filenames_AQUA <- list.files(pattern = ".csv")

kriging_points <- function(filenames_AQUA, resl_ras= 0.1, shp_WRF = "Z:/_SHARED_FOLDERS/Dust_Event_UAE_2015/WRFChem_domain"){
# kriging_points <- function(filenames_AQUA, resl_ras= 0.1, shp_WRF = "D:/Dust_Event_UAE_2015/WRFChem_domain"){
  
#  date <- str_sub(filenames_AQUA, start = 11, end = -14)
#  dawit_AQUA <- read_csv(filenames_AQUA)
  
# masking layer or shapefile
  
  if (is.character(shp_WRF)) {
    setwd(shp_WRF)
    dir<- shp_WRF
    shp_WRF <- readOGR(dsn = dir, layer = "domain_d01_WRFChem")
  #  setwd("Z:/_SHARED_FOLDERS/Dust_Event_UAE_2015/MODIS_10km/AQUA")
    # setwd("D:/Dust_Event_UAE_2015/MODIS_10km/AQUA")
  }
  
  limit_x_y <- extent(shp_WRF)
  
  AOD_AQUA$x <- AOD_AQUA$lon
  AOD_AQUA$y <- AOD_AQUA$lat
  
  coordinates(AOD_AQUA) = ~ x + y  ## Set spatial coordinates to create a Spatial object:
  
 # spplot(AOD_AQUA, zcol="aod")
  
  
  ## make a variogram----------------------------------------------------------------
  
  vargram_AOD <- variogram(aod ~ 1, AOD_AQUA) # calculates sample variogram values
  nn <- floor(length(vargram_AOD$gamma)/2)
  var_for_fit<- mean(vargram_AOD[nn:nrow(vargram_AOD),3])
  
  
  # fit the variogram
  vargram_AOD_fit  <- fit.variogram(vargram_AOD, fit.ranges = FALSE, fit.sills = FALSE,
                                    vgm(var_for_fit, "Sph"), fit.kappa = TRUE)
  
  plot(vargram_AOD, vargram_AOD_fit) # plot the sample values, along with the fit model
  
  plot(vargram_AOD_fit)
  
  
  
  # make a regular empty grid
  x.range <- as.numeric(c(floor(limit_x_y[1]-1),ceiling(limit_x_y[2]+1)))  # min/max longitude of the interpolation area
  y.range <- as.numeric(c(floor(limit_x_y[3]-1),ceiling(limit_x_y[4]+1)))  # min/max latitude of the interpolation area
  
  
  ## grid at 10km resolution
  grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = resl_ras),
                     y = seq(from = y.range[1], to = y.range[2], by = resl_ras))  # expand points to grid
  coordinates(grd) <- ~x + y
  gridded(grd) <- TRUE
  
  plot(grd, cex = 1.5, col = "grey")
  points(dawit_AQUA, pch = 1, col = "red", cex = 1)
  
  
  f.1 <- as.formula(Precip_in ~ X + Y)
  # perform kriging
  dat.krg <- gstat::krige(values ~ 1, dawit_AQUA, grd, vargram_AOD_fit, nmax = 50)
  
  
  r <- raster(dat.krg)
  plot(shp_UAE, add=TRUE, lwd=1)
  plot(r)
  projection(r) <- CRS("+proj=longlat +datum=WGS84")
  
  # writeRaster(r, paste0("Z:/_SHARED_FOLDERS/Dust_Event_UAE_2015/MODIS_10km/AQUA/AOD_MOY04_10km_UAE","_",date,".tif"), overwrite = TRUE)
  writeRaster(r, paste0("D:/Dust_Event_UAE_2015/MODIS_10km/AQUA/AOD_MOY04_10km_UAE","_",date,".tif"), overwrite = TRUE)
  
  return(r)
}

# kriging_points(filenames_TERRA)
BBB <- lapply(filenames_AQUA, kriging_points)







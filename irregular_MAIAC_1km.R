
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

 setwd("D:/Dust_Event_UAE_2015/WRF_trial_runs")
# setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs")

# get extent from WRFChem output-----------------------------------------------
# use the raster stack with all the WRF_chem output

# get only one image
WRF_STACK_image <- raster("DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = 2)
plot(WRF_STACK_image)

# get the extent of the raster
e <- extent(WRF_STACK_image)
# make a spatial polygon from the extent
p <- as(e, "SpatialPolygons")
plot(p)
crs(p) <- "proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# save shp file for the rectangular DOMAIN from WRFChem
setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRFChem_domain")
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
dir <- "D:/Dust_Event_UAE_2015/WRFChem_domain"

# larger WRF domain
shp_WRF_L <- readOGR(dsn = dir, layer = "domain_d01_WRFChem")
plot(shp_WRF_L)
# small WRF domain
shp_WRF <- readOGR(dsn = dir, layer = "ADMIN_domain_d01_WRFChem_small")
shp_WRF <- spTransform(shp_WRF, CRS("+init=epsg:4326"))

plot(shp_WRF_L)
plot(shp_UAE, add=TRUE, lwd=1)
plot(shp_WRF, add=TRUE, lwd=1)


#####################################################################################################################
#####################################################################################################################
############## RASTERIZE MODIS data #################################################################################
#####################################################################################################################

##########
# AQUA ###
##########

wd_AOD <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/AQUA"
# wd_AOD <- "D:/Dust_Event_UAE_2015/MAIAC_1km/AQUA"
setwd(wd_AOD)


# list directories
DAYS_AQUA <- str_sub(list.dirs(), start = 3, end = -1)
DAYS_AQUA <- DAYS_AQUA[-1]

# i <- 1


#  resl_ras= 0.01

for (i in 1:length(DAYS_AQUA)) {
  date <- DAYS_AQUA[i]
  setwd(paste0(wd_AOD,"/",DAYS_AQUA[i]))
  
  filenames_AQUA <- list.files(pattern = "AOD_LAT_LON_1km_")
  
  ##### make a function to create a raster from an irregular data frame (lat , lon , AOD)
#   raster_irregular <- function(filenames_AQUA, resl_ras= 0.01, shp_WRF = "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRFChem_domain"){
   raster_irregular <- function(filenames_AQUA, resl_ras= 0.01){
       
    date <- str_sub(filenames_AQUA, start = 17, end = -5)
    AOD_AQUA <- read_csv(filenames_AQUA)  # load the one without NA values
    
# masking layer or shapefile
  
  # if (is.character(shp_WRF)) {
  #   setwd(shp_WRF)
  #   dir<- shp_WRF
  #   shp_WRF <- readOGR(dsn = dir, layer = "domain_d01_WRFChem")
  #   setwd(paste0(wd_AOD,"/",DAYS_AQUA[i]))
  # }

    colnames(AOD_AQUA) <- c('x', 'y', 'z')
    # x.range <- as.numeric(c(floor(min(AOD_AQUA$x)-1),ceiling(max(AOD_AQUA$x)+1)))  # min/max longitude of the interpolation area
    # y.range <- as.numeric(c(floor(min(AOD_AQUA$y)-1),ceiling(max(AOD_AQUA$y)+1)))  # min/max latitude of the interpolation area
    
    x.range <- as.numeric(c(30,76))
    y.range <- as.numeric(c(9,41))
    
    grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = resl_ras),
                        y = seq(from = y.range[1], to = y.range[2], by = resl_ras))  # expand points to grid
    
    
    #  make ne extent for MAIAC data
    # new_extent_MAIAC <- extent(c(32,70,10,36))
    # plot(new_extent_MAIAC)
    # new_extent_MAIAC <- as(new_extent_MAIAC, "SpatialPolygons")
    # plot(new_extent_MAIAC)
    # shapefile(new_extent_MAIAC, 'Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/extent_MAIAC_UAE/extent_MAIAC.shp', overwrite = T)
    
    
    grd_1<- dplyr::filter(grd, grd$x == 45)
    nrow(grd_1)
    grd_2<- dplyr::filter(grd, grd$y == 15)
    nrow(grd_2)
    # r <- raster(xmn=min(AOD_AQUA$x), xmx=max(AOD_AQUA$x), ymn=min(AOD_AQUA$y), 
    #             ymx=max(AOD_AQUA$y), ncol=4601, nrow= 3201)
    
    r <- raster(xmn=min(AOD_AQUA$x), xmx=max(AOD_AQUA$x), ymn=min(AOD_AQUA$y),
                ymx=max(AOD_AQUA$y), ncol=nrow(grd_2), nrow= nrow(grd_1))
    
    raster_AQUA <- rasterize(AOD_AQUA[, 1:2], r, AOD_AQUA[,3], fun=mean)
   
    res(raster_AQUA)
    plot(raster_AQUA)
    projection(raster_AQUA) <- CRS("+proj=longlat +datum=WGS84")
    plot(shp_UAE, add=TRUE, lwd=1)
 #   plot(new_extent_MAIAC, add=TRUE, lwd=1)
    
    
    # raster_AQUA_crop <- crop(raster_AQUA, extent(new_extent_MAIAC))
    # raster_AQUA_crop <- mask(raster_AQUA_crop, new_extent_MAIAC)
    # plot(raster_AQUA_crop)
    
    
    writeRaster(raster_AQUA, paste0(date,".tif"), overwrite = TRUE)   
    return(r)
   }
    
BBB <- lapply(filenames_AQUA, raster_irregular)

}






###############################################################################################################################
###############################################################################################################################
###############################################################################################################################

############
# TERRA ####
############

wd_AOD <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/TERRA"
# wd_AOD <- "D:/Dust_Event_UAE_2015/MAIAC_1km/TERRA"
setwd(wd_AOD)


# list directories
DAYS_TERRA <- str_sub(list.dirs(), start = 3, end = -1)
DAYS_TERRA <- DAYS_TERRA[-1]

# i <- 1


#  resl_ras= 0.01

for (i in 1:length(DAYS_TERRA)) {
  date <- DAYS_TERRA[i]
  setwd(paste0(wd_AOD,"/",DAYS_TERRA[i]))
  
  filenames_TERRA <- list.files(pattern = "AOD_LAT_LON_1km_")
  
  ##### make a function to create a raster from an irregular data frame (lat , lon , AOD)
  raster_irregular <- function(filenames_AQUA, resl_ras= 0.01){
    
    date <- str_sub(filenames_TERRA, start = 17, end = -5)
    AOD_TERRA <- read_csv(filenames_TERRA)  # load the one without NA values
  
    colnames(AOD_TERRA) <- c('x', 'y', 'z')
    # x.range <- as.numeric(c(floor(min(AOD_AQUA$x)-1),ceiling(max(AOD_AQUA$x)+1)))  # min/max longitude of the interpolation area
    # y.range <- as.numeric(c(floor(min(AOD_AQUA$y)-1),ceiling(max(AOD_AQUA$y)+1)))  # min/max latitude of the interpolation area
    
    x.range <- as.numeric(c(30,76))
    y.range <- as.numeric(c(9,41))
    
    grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = resl_ras),
                       y = seq(from = y.range[1], to = y.range[2], by = resl_ras))  # expand points to grid
    
    grd_1<- dplyr::filter(grd, grd$x == 45)
    nrow(grd_1)
    grd_2<- dplyr::filter(grd, grd$y == 15)
    nrow(grd_2)
    # r <- raster(xmn=min(AOD_AQUA$x), xmx=max(AOD_AQUA$x), ymn=min(AOD_AQUA$y), 
    #             ymx=max(AOD_AQUA$y), ncol=4601, nrow= 3201)
    
    r <- raster(xmn=min(AOD_TERRA$x), xmx=max(AOD_TERRA$x), ymn=min(AOD_TERRA$y),
                ymx=max(AOD_TERRA$y), ncol=nrow(grd_2), nrow= nrow(grd_1))
    
    raster_TERRA <- rasterize(AOD_TERRA[, 1:2], r, AOD_TERRA[,3], fun=mean)
    
    res(raster_TERRA)
    plot(raster_TERRA)
    projection(raster_TERRA) <- CRS("+proj=longlat +datum=WGS84")
    plot(shp_UAE, add=TRUE, lwd=1)

    
    writeRaster(raster_TERRA, paste0(date,".tif"), overwrite = TRUE)   
    return(r)
  }
  
  BBB <- lapply(filenames_TERRA, raster_irregular)
  
}



#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################
#########################################################################

 
# ############## AVERAGE MODIS Terra and MODIS Aqua ################################################
# ##############--------------------------------------- ############################################
# 
# # list number of the days ######
# wd_AOD <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/TERRA"
# # wd_AOD <- "D:/Dust_Event_UAE_2015/MAIAC_1km/TERRA"
# setwd(wd_AOD)
# 
# # list directories
# DAYS_TERRA <- str_sub(list.dirs(), start = 3, end = -1)
# DAYS <- DAYS_TERRA[-1]
# 
# # i <- 3
# 
# # load a reference raster to use for the same extension and projection #######
# reference <- raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/AQUA/88/88_AQUA.tif")
# 
# 
# for (i in 1:length(DAYS)) {
#   date <- DAYS[i]
# #  setwd(paste0(wd_AOD,"/",DAYS_TERRA[i]))
#   
#   filenames_AQUA <- list.files(path = paste0("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/AQUA/", date, "/"),
#                                pattern = "_AQUA.tif$")
#   
#   filenames_TERRA <- list.files(path = paste0("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/TERRA/", date, "/"),
#                                pattern = "_TERRA.tif$")
# 
#   
# r_AQUA <- raster(paste0("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/AQUA/", date, "/",filenames_AQUA))
# r_TERRA <- raster(paste0("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/TERRA/", date, "/",filenames_TERRA))
# 
# # reproject each raster with the same extent and resolution of the reference raster above
# r_TERRA = projectRaster(r_TERRA, reference)
# r_AQUA = projectRaster(r_AQUA, reference)
# 
# plot(r_TERRA)
# plot(r_AQUA)
# 
# # make a stack with the two raster from MODIS Terra and MODIS AQUA for each day (date) ####
# stack_MAIAC <- stack(r_TERRA, r_AQUA)
# # calculate the mean of all the rasters (2) in the stack
# mean_MAIAC <- mean(stack_MAIAC, na.rm=TRUE)
# plot(mean_MAIAC)
# dir_out <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/AVG_TERRA_MAIAC"
# writeRaster(mean_MAIAC, paste0(dir_out,"/", date,"_AVERAGE_Terra_Aqua_MAIAC.tif"), overwrite = TRUE)   
# 
# remove(stack_MAIAC, stack_MAIAC)
# 
# }





##################################################################################
##################################################################################
##################################################################################
#### reload data and crop images #################################################

setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/AQUA")
setwd("D:/Dust_Event_UAE_2015/MAIAC_1km/AQUA")

# create new extent to crop the rasters #######################################################
new_extent_MAIAC <- extent(c(40,62,11,32))
# new_extent_MAIAC <- extent(c(34,62,11,38))
plot(new_extent_MAIAC)
new_extent_MAIAC <- as(new_extent_MAIAC, "SpatialPolygons")
plot(new_extent_MAIAC)
shapefile(new_extent_MAIAC, 'Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/extent_MAIAC_UAE/extent_MAIAC.shp', overwrite = T)


##### crop MODIS TERRA & AQUA rasters ##-----------------------------------------


# AQUA ####-------------------------------------------------------------------------
setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/AQUA")
setwd("D:/Dust_Event_UAE_2015/MAIAC_1km/AQUA")
filenames <- list.files(pattern = "_AQUA.tif$")
dir_out <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/AQUA"
dir_out <- "D:/Dust_Event_UAE_2015/MAIAC_1km/AQUA"

# i <- 3 

# filenames <- filenames[i]

# crop rasters with the WRF large extent

for (i in 1:length(filenames)) {
  
  date <- str_sub(filenames[i], start = 1, end = -10)
  r <- raster(filenames[i])
  plot(r)
  # r <- crop(r, extent(new_extent_MAIAC))
  # r <- mask(r, new_extent_MAIAC)
   r <- crop(r, extent(shp_WRF))
   r <- mask(r, shp_WRF)  
  plot(r)
  
  # plot(new_extent_MAIAC,add=TRUE, lwd=1)
  # plot(shp_WRF,add=TRUE, lwd=1)
  # plot(shp_UAE, add=TRUE, lwd=1)
  
  writeRaster(r, paste0(dir_out,"/", date,"_Aqua_MAIAC_crop_small.tif"), overwrite = TRUE) 
}



# TERRA ####-------------------------------------------------------------------------
setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/TERRA")
setwd("D:/Dust_Event_UAE_2015/MAIAC_1km/TERRA")
filenames <- list.files(pattern = "_TERRA.tif$")
dir_out <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/TERRA"
dir_out <- "D:/Dust_Event_UAE_2015/MAIAC_1km/TERRA"

# i <- 3 

# filenames <- filenames[i]

# crop rasters with the WRF large extent

for (i in 1:length(filenames)) {
  
  date <- str_sub(filenames[i], start = 1, end = -11)
  r <- raster(filenames[i])
  plot(r)
  r <- crop(r, extent(shp_WRF))
  r <- mask(r, shp_WRF)  
  plot(r)
  
  # plot(new_extent_MAIAC,add=TRUE, lwd=1)
  # plot(shp_WRF,add=TRUE, lwd=1)
  # plot(shp_UAE, add=TRUE, lwd=1)
  
  writeRaster(r, paste0(dir_out,"/", date,"_Terra_MAIAC_crop_small.tif"), overwrite = TRUE) 
}

##############################################################################################################################
##############################################################################################################################
######################### MAKE Raster Stacks of MODIS TERRA and MODIS AQUA separately ########################################
##############################################################################################################################
##############################################################################################################################


# AQUA ----------------------------------------------------------------

dir_AQUA <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/AQUA/"

# multiply raster by conversion factor from AOD to PM10
  # March_29 <- raster(paste0(dir_AQUA, "88_Aqua_MAIAC_crop.tif"))*294
  # March_30 <- raster(paste0(dir_AQUA, "89_Aqua_MAIAC_crop.tif"))*294
  # March_31 <- raster(paste0(dir_AQUA, "90_Aqua_MAIAC_crop.tif"))*294
  # April_01 <- raster(paste0(dir_AQUA, "91_Aqua_MAIAC_crop.tif"))*294
  # April_02 <- raster(paste0(dir_AQUA, "92_Aqua_MAIAC_crop.tif"))*294
  # April_03 <- raster(paste0(dir_AQUA, "93_Aqua_MAIAC_crop.tif"))*294
  
  March_29 <- raster(paste0(dir_AQUA, "88_Aqua_MAIAC_crop.tif"))
  March_30 <- raster(paste0(dir_AQUA, "89_Aqua_MAIAC_crop.tif"))
  March_31 <- raster(paste0(dir_AQUA, "90_Aqua_MAIAC_crop.tif"))
  April_01 <- raster(paste0(dir_AQUA, "91_Aqua_MAIAC_crop.tif"))
  April_02 <- raster(paste0(dir_AQUA, "92_Aqua_MAIAC_crop.tif"))
  April_03 <- raster(paste0(dir_AQUA, "93_Aqua_MAIAC_crop.tif"))
  
  plot(March_29)
  plot(March_30)
  plot(March_31)
  plot(April_01)
  plot(April_02)
  plot(April_03)
  
  
  ########################################################################################
  ########################################################################################
  # make a stack raster-------------------------------------------------------------------
  ########################################################################################
  ########################################################################################
  
  # AQUA ##-------------------------------------------------------------------------
  setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/AQUA")
  setwd("D:/Dust_Event_UAE_2015/MAIAC_1km/AQUA")
  patt<- "_MAIAC_crop_small.tif$"
  filenames <- list.files(pattern = patt)
  all_rasters <- stack()    # inizialise the raster stack
  
# make a raster stack ############
  # i <- 2
  
  for (i in 1:length(filenames)){  
    if (i==1){
      r <- raster(filenames[i])
      plot(r)
      all_rasters<- stack(all_rasters,r)
      ref<-r
    }else{
      r <- raster(filenames[i])
      r = projectRaster(r, ref)
      plot(r)
      all_rasters<- stack(all_rasters,r)
    }
  
  }
  
  
  # save the raster stack with all the MAIAC data at 1km resolution
  writeRaster(all_rasters, "AQUA_MAIAC_DUST_event_02_April_2015_small.tif" , options= "INTERLEAVE=BAND", overwrite=T)
  
  # find min and max values of the stack raster
  max(all_rasters)
  # min = 0.095
  # max = 4.089
  minValue(all_rasters)[[1]] # Gives first in stack max value
  maxValue(all_rasters)[[2]] # Gives second in stack max value
  
  
  
  
  # TERRA ##-------------------------------------------------------------------------
  setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/TERRA")
  setwd("D:/Dust_Event_UAE_2015/MAIAC_1km/TERRA")
  patt<- "_MAIAC_crop_small.tif$"
  filenames <- list.files(pattern = patt)
  all_rasters <- stack()    # inizialise the raster stack
  
  # make a raster stack ############
  # i <- 2
  
  for (i in 1:length(filenames)){  
    if (i==1){
      r <- raster(filenames[i])
      plot(r)
      all_rasters<- stack(all_rasters,r)
      ref<-r
    }else{
      r <- raster(filenames[i])
      r = projectRaster(r, ref)
      plot(r)
      all_rasters<- stack(all_rasters,r)
    }
    
  }
  
  
  # save the raster stack with all the MAIAC data at 1km resolution
  writeRaster(all_rasters, "TERRA_MAIAC_DUST_event_02_April_2015_small.tif" , options= "INTERLEAVE=BAND", overwrite=T)
  
  # find min and max values of the stack raster
  max(all_rasters)
  # min = 0.095
  # max = 4.089
  minValue(all_rasters)[[1]] # Gives first in stack max value
  maxValue(all_rasters)[[2]] # Gives second in stack max value
  

  
########################################################################################
##### MAKE a STACK of rasters for the days 31 March, April 1 and April 2 2016 ##########
########################################################################################
  
  # setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/AVG_TERRA_MAIAC")
  # 
  # March_31 <- raster("90_AVERAGE_Terra_Aqua_MAIAC_crop.tif")*294
  # April_01 <- raster("91_AVERAGE_Terra_Aqua_MAIAC_crop.tif")*294
  # April_02 <- raster("92_AVERAGE_Terra_Aqua_MAIAC_crop.tif")*294
  # 
  # all_rasters<- stack(March_31,
  #                     April_01,
  #                     April_02)
  # 
  # writeRaster(all_rasters, "stack_31_1_2_Apr_2015/MAIAC_DUST_event_02_April_2015.tif" , options= "INTERLEAVE=BAND", overwrite=T)
    
########################################################################################
########################################################################################

########################################################################################
#### reload data and make a MOVIE ######################################################

library(leaflet)
library(webshot)
library(htmlwidgets)
library(RColorBrewer)
library(raster)
library(classInt)
library(stringr)
  
library(viridis)
library(lattice)

#### import the Arabian Peninsusula domain #############

dir_ME <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRFChem_domain"
dir_ME <- "D:/Dust_Event_UAE_2015/WRFChem_domain"
### shapefile for WRF_domain
shp_ME <- readOGR(dsn = dir_ME, layer = "ADMIN_domain_d01_WRFChem")
  
shp_ME@data$name <- 1:nrow(shp_ME)
plot(shp_ME)
  
  

# gerate a time sequence of 6 days
# start <- as.POSIXct("2015-03-29 13:30")  # MODIS AQUA
 start <- as.POSIXct("2015-03-29 10:30")  # MODIS TERRA
interval <- 60*12 #minutes (1 day interval)
end <- start + as.difftime(5, units="days")  # 6 days
TS <- seq(from=start, by=interval*60*2, to=end)

# setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/AVG_TERRA_MAIAC")
# setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/AQUA")
# setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/TERRA")

# setwd("D:/Dust_Event_UAE_2015/MAIAC_1km/AQUA")
setwd("D:/Dust_Event_UAE_2015/MAIAC_1km/TERRA")

# output_folder_AQUA <- "D:/Dust_Event_UAE_2015/MAIAC_1km/AQUA/plots_small_domain/"
output_folder_TERRA <- "D:/Dust_Event_UAE_2015/MAIAC_1km/TERRA/plots_small_domain/"

# AQUA
# MAIAC_STACK_image <- stack("AQUA_MAIAC_DUST_event_02_April_2015_small.tif")
# TERRA
MAIAC_STACK_image <- stack("TERRA_MAIAC_DUST_event_02_April_2015_small.tif")



####### color pallet

vec_all <- as.vector(MAIAC_STACK_image)

# max_val<- (max(vec_all, na.rm = T))
max_val <- 4
# min_val<- (min(vec_all,  na.rm = T))
min_val <- 0


stat_dat <- summary(as.vector(MAIAC_STACK_image))
IQR <- (as.numeric((stat_dat[5]-stat_dat[2])* 2))# n is the space after IQR

low_IQR<- if(floor(min_val) > floor(as.numeric((stat_dat[2]- IQR)))) floor(min_val) else floor(as.numeric((stat_dat[2]- IQR)))
high_IQR <-if ( max_val > (as.numeric((stat_dat[5]+IQR)))) max_val else (as.numeric((stat_dat[5]+IQR)))

cool = rainbow(50, start=rgb2hsv(col2rgb('green'))[1], end=rgb2hsv(col2rgb('blue'))[1])
cool_2 = rainbow(25, start=rgb2hsv(col2rgb('yellow'))[1], end=rgb2hsv(col2rgb('green'))[1])
warm = rainbow(125, start=rgb2hsv(col2rgb('red'))[1], end=rgb2hsv(col2rgb('yellow'))[1])
cols = c(rev(cool), rev(cool_2), rev(warm))

# greenyellow at the place of yellow?

# i <- 5



########################
### plots of maps ######
########################

# AQUA
# raster_MAIAC <- stack("AQUA_MAIAC_DUST_event_02_April_2015_small.tif")
# TERRA
raster_MAIAC <- stack("TERRA_MAIAC_DUST_event_02_April_2015_small.tif")

for (i in 1:length(raster_MAIAC@layers)) {
  name_time <- TS[i]
 # AQUA_images <- raster("AQUA_MAIAC_DUST_event_02_April_2015_small.tif", band = i)
  TERRA_images <- raster("TERRA_MAIAC_DUST_event_02_April_2015_small.tif", band = i)
#  plot(AQUA_images)
  plot(TERRA_images)
  
 # h <- rasterVis::levelplot(AQUA_images, 
  h <- rasterVis::levelplot(TERRA_images, 
                            margin=FALSE, main= as.character(name_time),
                            xlab = "",
                            ylab = "",
                            ## about colorbar
                            colorkey=list(
                              space='right',                   
                              labels= list(at= floor(as.numeric( seq(low_IQR, high_IQR, length.out=7))),
                                           font=3),
                              axis.line=list(col='black'),
                              width=0.75,
                              title=expression(paste("     AOD") )
                            ),   
                            ## about the axis
                            par.settings=list(
                              strip.border=list(col='transparent'),
                              strip.background=list(col='transparent'),
                              axis.line=list(col='black')
                            ),
                            scales=list(draw=T, alternating= F),            
                            #col.regions = colorRampPalette(c("blue", "white","red"))(1e3),
                            col.regions = cols,
                            at=unique(c(seq(low_IQR, high_IQR, length.out=200)))) +
    latticeExtra::layer(sp.polygons(shp_ME))
  h
  
#  png(paste0(output_folder_AQUA ,str_sub(name_time, start = 1, end = -10), "_",
  png(paste0(output_folder_TERRA ,str_sub(name_time, start = 1, end = -10), "_",
             str_sub(name_time, start = 12, end = -7), "_",
             str_sub(name_time, start = 15, end = -4),
             ".png"), width = 900, height = 900,
      units = "px", pointsize = 50,
      bg = "white", res = 200)
  print(h)
  dev.off()
  
}


# to make a movie.......
# to use with ImageMagik using the commnad line cmd in windows
# cd into the directory where there are the png files

# magick -delay 100 -loop 0 *.png MAIAC_AQUA_DUST_event_02_April_2015.gif

#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################

##### old stuff ######

for (i in 1:6) {
  
  # path <- ".tif$"
  # filenames <- list.files(pattern = path)
  # multiply raster by conversion factor from AOD to PM10
  # raster_MODIS <- raster(filenames[i])
  
  raster_MODIS_AQUA <- raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/AQUA/AQUA_MAIAC_DUST_event_02_April_2015.tif", band = i)
  # raster_MODIS_TERRA <- raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/TERRA/TERRA_MAIAC_DUST_event_02_April_2015.tif", band = i)
  # raster_MODIS <- raster(filenames[i])*294
  
  # plot(raster_MODIS)
  # plot(shp_UAE, add=TRUE, lwd=1)
  
  name_time <- TS[i]
  


# define popup for time scene
"h2 { font-size: 3px;}"
# content <- paste('<h2><strong>', name_time,'', " TERRA", sep = "")
 content <- paste('<h1><strong>', name_time,'', "  AQUA", sep = "")


# for AQUA
# MIN_PAL <- 0.095*294
# MAX_PAL <- 3.924*294
 MIN_PAL <- 0
 MAX_PAL <- 4.1

# for TERRA
 # MIN_PAL <- 0.095*294
 # MAX_PAL <- 4.089*294
 # MIN_PAL <- 0.095
 # MAX_PAL <- 4.089


# pal_MODIS <- colorNumeric(rev(terrain.colors(255)),
#                         getValues(raster_MODIS), na.color = "transparent")

pal_MODIS <- colorNumeric(rev(terrain.colors(255)),
                          c(MIN_PAL, MAX_PAL),na.color = "transparent")

map <- leaflet(shp_ME) %>% 
  setView(51, 26, 5) %>%
  addTiles() %>% 
  addTiles(group = "OSM (default)") %>%
  addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
  
  addPopups(40, 35, content,
            options = popupOptions(closeButton = FALSE)) %>%
  
  addRasterImage(raster_MODIS_AQUA, 
                 colors = pal_MODIS, 
                 opacity = 0.7, group = "MODIS", maxBytes = 8 * 1024 * 1024) %>%
  
  addPolygons(stroke = TRUE, smoothFactor = 1, fillOpacity = 0,
              weight = 2, color = "#000000",
              group = "ME") %>%
  
  addLayersControl(
    baseGroups = c("Toner Lite", "Road map", "Satellite"),
    overlayGroups = "MODIS",
    options = layersControlOptions(collapsed = TRUE)) %>%
  addLegend("bottomright", pal = pal_MODIS, values = c(MIN_PAL, MAX_PAL),
#            title = "<br><strong>PM<sub>10</sub> (<font face=symbol>m</font>g/m<sup>3</sup>) MAIAC: </strong>",
            title = "<br><strong> AOD: </strong>",
            labFormat = labelFormat(prefix = ""),
            opacity = 0.7)
# addLegend("bottomright", pal = pal_MODIS, values = getValues(raster_MODIS),
#           title = "<br><strong>PM<sub>10</sub> (<font face=symbol>m</font>g/m<sup>3</sup>) MAIAC: </strong>",
#           labFormat = labelFormat(prefix = ""),
#           opacity = 0.7)


map

## This is the png creation part
saveWidget(map, 'temp.html', selfcontained = FALSE)
webshot('temp.html', file = paste0(str_sub(name_time, start = 1, end = -10), "_",
                                   str_sub(name_time, start = 12, end = -7), "_",
                                   str_sub(name_time, start = 15, end = -4),
                                   ".png"), vwidth = 1000, vheight = 1000,
        cliprect = 'viewport')

remove(raster_MODIS)

}

# to make a movie.......
# to use with ImageMagik using the commnad line cmd in windows
# cd into the directory where there are the png files

# magick -delay 100 -loop 0 *.png MAIAC_AQUA_DUST_event_02_April_2015.gif


#########################################################################################################
#########################################################################################################

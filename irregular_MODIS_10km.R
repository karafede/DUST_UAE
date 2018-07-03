
library(readr)
library(sp)
library(raster)
library(gstat)
library(rgdal)
library(RNetCDF)
library(ncdf4)
library(stringr)
library(rgeos)

library(raster)
library(leaflet)
library(htmlwidgets)

# get extent from WRFChem output-----------------------------------------------
# use the raster stack with all the WRF_chem output

# get only one image
# WRF_STACK_image <- raster("DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = 2)
# plot(WRF_STACK_image)

# get the extent of the raster
# e <- extent(WRF_STACK_image)
# make a spatial polygon from the extent
# p <- as(e, "SpatialPolygons")
# plot(p)
# crs(p) <- "proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# save shp file for the rectangular DOMAIN from WRFChem
# setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRFChem_domain")
# shapefile(p, "domain_d01_WRFChem.shp", overwrite=TRUE)


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
shp_WRF <- readOGR(dsn = dir, layer = "domain_d01_12km_WRFChem")
plot(shp_WRF)
# small WRF domain
# shp_WRF <- readOGR(dsn = dir, layer = "ADMIN_domain_d01_WRFChem_small")
# shp_WRF <- spTransform(shp_WRF, CRS("+init=epsg:4326"))

plot(shp_WRF)
plot(shp_UAE, add=TRUE, lwd=1)
plot(shp_WRF, add=TRUE, lwd=1)

dir_ME <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRFChem_domain"
dir_ME <- "D:/Dust_Event_UAE_2015/WRFChem_domain"
### shapefile for WRF_domain (ARABIAN PENINSULA)
shp_ME <- readOGR(dsn = dir_ME, layer = "ADMIN_domain_d01_12km_WRFChem")
plot(shp_ME)


# load reference file (MAIAC 1km resolution)
# MODIS MAIAC reference
# ref <- raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/AQUA/92_Aqua_MAIAC_crop.tif")
# load reference for SEVIRI data (2km)
ref <- raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/SEVIRI_20150402_outputs/II_method/Seviri_20150402_METFr_Orig_stack.tif", band = 20)
ref <- crop(ref, extent(38.01822, 59.99447, 13, 34))
plot(ref)
plot(shp_ME, add = TRUE)

# crop shp_ME
shp_ME <- raster::crop(shp_ME, extent(38.01822, 59.99447, 13, 34))
plot(shp_ME)


#####################################################################################################################
#####################################################################################################################
############## RASTERIZE MODIS data #################################################################################
#####################################################################################################################

##########
# AQUA ###
##########

wd_AOD <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MODIS_10km/AQUA"
setwd(wd_AOD)

filenames_AQUA <- list.files(pattern = ".csv$")

# i <- 1
# filenames_AQUA <- filenames_AQUA[1]


  ##### make a function to create a raster from an irregular data frame (lat , lon , AOD)
   raster_irregular <- function(filenames_AQUA, resl_ras= 0.17){
       
    name <- str_sub(filenames_AQUA, start = 1, end = -5)
    date <- str_sub(filenames_AQUA, start = 11, end = -14)
    AOD_AQUA <- read_csv(filenames_AQUA)[-1]  # load the one without NA values
    

    colnames(AOD_AQUA) <- c('x', 'y', 'z')

    x.range <- as.numeric(c(30,76))
    y.range <- as.numeric(c(9,41))
    
    grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = resl_ras),
                        y = seq(from = y.range[1], to = y.range[2], by = resl_ras))  # expand points to grid
    

    grd_1<- dplyr::filter(grd, grd$x == 30)
    nrow(grd_1)
    grd_2<- dplyr::filter(grd, grd$y == 9)
    nrow(grd_2)
    
    r <- raster(xmn=min(AOD_AQUA$x), xmx=max(AOD_AQUA$x), ymn=min(AOD_AQUA$y),
                ymx=max(AOD_AQUA$y), ncol=nrow(grd_2), nrow= nrow(grd_1))
    
    raster_AQUA <- rasterize(AOD_AQUA[, 1:2], r, AOD_AQUA[,3], fun=mean)
   
    res(raster_AQUA)
    plot(raster_AQUA)
    projection(raster_AQUA) <- CRS("+proj=longlat +datum=WGS84")
    plot(shp_UAE, add=TRUE, lwd=1)
    
    writeRaster(raster_AQUA, paste0("AOD_MOY04_10km_",date,".tif"), overwrite = TRUE)   
    return(r)
   }
    
BBB <- lapply(filenames_AQUA, raster_irregular)




###############################################################################################################################
###############################################################################################################################
###############################################################################################################################

############
# TERRA ####
############

wd_AOD <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MODIS_10km/TERRA"
setwd(wd_AOD)

filenames_TERRA <- list.files(pattern = ".csv$")

# i <- 1
# filenames_AQUA <- filenames_AQUA[1]


##### make a function to create a raster from an irregular data frame (lat , lon , AOD)
raster_irregular <- function(filenames_TERRA, resl_ras= 0.17){
  
  name <- str_sub(filenames_TERRA, start = 1, end = -5)
  date <- str_sub(filenames_TERRA, start = 11, end = -14)
  AOD_TERRA <- read_csv(filenames_TERRA)[-1]  # load the one without NA values
  
  
  colnames(AOD_TERRA) <- c('x', 'y', 'z')
  
  x.range <- as.numeric(c(30,76))
  y.range <- as.numeric(c(9,41))
  
  grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = resl_ras),
                     y = seq(from = y.range[1], to = y.range[2], by = resl_ras))  # expand points to grid
  
  
  grd_1<- dplyr::filter(grd, grd$x == 30)
  nrow(grd_1)
  grd_2<- dplyr::filter(grd, grd$y == 9)
  nrow(grd_2)
  
  r <- raster(xmn=min(AOD_TERRA$x), xmx=max(AOD_TERRA$x), ymn=min(AOD_TERRA$y),
              ymx=max(AOD_TERRA$y), ncol=nrow(grd_2), nrow= nrow(grd_1))
  
  raster_TERRA <- rasterize(AOD_TERRA[, 1:2], r, AOD_TERRA[,3], fun=mean)
  
  res(raster_TERRA)
  plot(raster_TERRA)
  projection(raster_TERRA) <- CRS("+proj=longlat +datum=WGS84")
  plot(shp_UAE, add=TRUE, lwd=1)
  
  writeRaster(raster_TERRA, paste0("AOD_MOD04_10km_",date,".tif"), overwrite = TRUE)   
  return(r)
}

BBB <- lapply(filenames_TERRA, raster_irregular)



#########################################################################
#########################################################################

 ##### crop MODIS TERRA & AQUA rasters ##-----------------------------------------


# AQUA ####-------------------------------------------------------------------------
setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MODIS_10km/AQUA")
#### move in all tiff files for MODIS and rename date numbers into datenumber_AQUA....same for TERRA
filenames <- list.files(pattern = ".tif$")
dir_out <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MODIS_10km/AQUA"

# i <- 3 

# filenames <- filenames[i]

# crop rasters with the WRF large extent

for (i in 1:length(filenames)) {
  
  date <- str_sub(filenames[i], start = 16, end = -5)
  r <- raster(filenames[i])
  plot(r)
  r <- crop(r, extent(shp_ME))
#  r <- mask(r, shp_ME)  
  plot(r)
  
  
  writeRaster(r, paste0(dir_out,"/", date,"_Aqua_MODIS_crop.tif"), overwrite = TRUE) 
}



# TERRA ####-------------------------------------------------------------------------
setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MODIS_10km/TERRA")
filenames <- list.files(pattern = ".tif$")
dir_out <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MODIS_10km/TERRA"

# i <- 3 

# filenames <- filenames[i]

# crop rasters with the WRF large extent

for (i in 1:length(filenames)) {
  
  date <- str_sub(filenames[i], start = 16, end = -5)
  r <- raster(filenames[i])
  plot(r)
  r <- crop(r, extent(shp_ME))
  # r <- mask(r, shp_ME)  
  plot(r)
  
  writeRaster(r, paste0(dir_out,"/", date,"_Terra_MODIS_crop.tif"), overwrite = TRUE) 
}

##############################################################################################################################
##############################################################################################################################
######################### MAKE Raster Stacks of MODIS TERRA and MODIS AQUA separately ########################################
##############################################################################################################################
##############################################################################################################################


  ########################################################################################
  ########################################################################################
  # make a stack raster-------------------------------------------------------------------
  ########################################################################################
  ########################################################################################
  
  # AQUA ##-------------------------------------------------------------------------
  setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MODIS_10km/AQUA")
  patt<- "_MODIS_crop.tif$"
  filenames <- list.files(pattern = patt)
  all_rasters <- stack()    # inizialise the raster stack
  all_rasters_MASKS <- stack() 

# make a raster stack ############
  # i <- 2
  
  for (i in 1:length(filenames)){  
    # if (i==1){
    #   r <- raster(filenames[i])
    #   plot(r)
    #   all_rasters<- stack(all_rasters,r)
    #   ref <- r
    # }else{
      r <- raster(filenames[i])
      r = projectRaster(r, ref)
      plot(r)
      all_rasters<- stack(all_rasters,r)
      
      # create a DUST MASK (0 & 1) according to AOD threshold values
      r_MASK <- r
      values(r_MASK)[values(r_MASK) < 0.75] = 0
      values(r_MASK)[values(r_MASK) > 0.75] = 1
      plot(r_MASK)
      all_rasters_MASKS <- stack(all_rasters_MASKS, r_MASK)
    }
  
 # }
  
  
  # save the raster stack with all the MAIAC data at 1km resolution
  writeRaster(all_rasters, "AQUA_MODIS_DUST_event_02_April_2015_1km.tif" , options= "INTERLEAVE=BAND", overwrite=T)
  writeRaster(all_rasters_MASKS, "MASK_DUST_AQUA_MODIS_event_02_April_2015.tif" , options= "INTERLEAVE=BAND", overwrite=T)
  
  # find min and max values of the stack raster
  max(all_rasters)
  # min = 0.095
  # max = 4.089
  minValue(all_rasters)[[1]] # Gives first in stack max value
  maxValue(all_rasters)[[2]] # Gives second in stack max value
  
  
  
  
  # TERRA ##-------------------------------------------------------------------------
  setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MODIS_10km/TERRA")
  patt<- "_MODIS_crop.tif$"
  filenames <- list.files(pattern = patt)
  all_rasters <- stack()    # inizialise the raster stack
  all_rasters_MASKS <- stack() 
  
  # make a raster stack ############
  # i <- 6
  
  for (i in 1:length(filenames)){  
    # if (i==1){
    #   r <- raster(filenames[i])
    #   plot(r)
    #   all_rasters<- stack(all_rasters,r)
    #   ref <- r
    # }else{
      r <- raster(filenames[i])
      r = projectRaster(r, ref)
      plot(r)
      all_rasters <- stack(all_rasters,r)
      
      # create a DUST MASK (0 & 1) according to AOD threshold values
      r_MASK <- r
      values(r_MASK)[values(r_MASK) < 0.75] = 0
      values(r_MASK)[values(r_MASK) > 0.75] = 1
      plot(r_MASK)
      all_rasters_MASKS <- stack(all_rasters_MASKS, r_MASK)
    }
    
#  }
  
  
  # save the raster stack with all the MAIAC data at 1km resolution
  writeRaster(all_rasters, "TERRA_MODIS_DUST_event_02_April_2015_1km.tif" , options= "INTERLEAVE=BAND", overwrite=T)
  writeRaster(all_rasters_MASKS, "MASK_DUST_TERRA_MODIS_event_02_April_2015.tif" , options= "INTERLEAVE=BAND", overwrite=T)
  
  # find min and max values of the stack raster
  max(all_rasters)
  # min = 0.095
  # max = 4.089
  minValue(all_rasters)[[1]] # Gives first in stack max value
  maxValue(all_rasters)[[2]] # Gives second in stack max value
  

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

# gerate a time sequence of 10 days
start <- as.POSIXct("2015-03-28 13:30")  # MODIS AQUA
# start <- as.POSIXct("2015-03-28 10:30")  # MODIS TERRA
interval <- 60*12 #minutes (1 day interval)
end <- start + as.difftime(9, units="days")  # 6 days
TS <- seq(from=start, by=interval*60*2, to=end)


setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MODIS_10km/AQUA")
# setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MODIS_10km/TERRA")

output_folder_AQUA <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MODIS_10km/AQUA/plots_1km/"
# output_folder_TERRA <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MODIS_10km/TERRA/plots_1km/"

# AQUA
MODIS_STACK_image <- stack("AQUA_MODIS_DUST_event_02_April_2015_1km.tif")
# TERRA
# MODIS_STACK_image <- stack("TERRA_MODIS_DUST_event_02_April_2015_1km.tif")



####### color pallet

vec_all <- as.vector(MODIS_STACK_image)

# max_val<- (max(vec_all, na.rm = T))
max_val <- 4
# min_val<- (min(vec_all,  na.rm = T))
min_val <- 0


stat_dat <- summary(as.vector(MODIS_STACK_image))
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
raster_MODIS <- stack("AQUA_MODIS_DUST_event_02_April_2015_1km.tif")
# TERRA
# raster_MODIS <- stack("TERRA_MODIS_DUST_event_02_April_2015_1km.tif")

for (i in 1:length(raster_MODIS@layers)) {
  name_time <- TS[i]
  AQUA_images <- raster("AQUA_MODIS_DUST_event_02_April_2015_1km.tif", band = i)
 # TERRA_images <- raster("TERRA_MAIAC_DUST_event_02_April_2015_12km.tif", band = i)
  plot(AQUA_images)
#  plot(TERRA_images)
  
  h <- rasterVis::levelplot(AQUA_images, 
 # h <- rasterVis::levelplot(TERRA_images, 
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
  
  png(paste0(output_folder_AQUA ,str_sub(name_time, start = 1, end = -10), "_",
#  png(paste0(output_folder_TERRA ,str_sub(name_time, start = 1, end = -10), "_",
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

# gerate a time sequence of 10 days
# start <- as.POSIXct("2015-03-28 13:30")  # MODIS AQUA
start <- as.POSIXct("2015-03-28 10:30")  # MODIS TERRA
interval <- 60*12 #minutes (1 day interval)
end <- start + as.difftime(9, units="days")  # 6 days
TS <- seq(from=start, by=interval*60*2, to=end)


# setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MODIS_10km/AQUA")
setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MODIS_10km/TERRA")

# output_folder_AQUA <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MODIS_10km/AQUA/plots_1km/"
output_folder_TERRA <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MODIS_10km/TERRA/plots_1km/"

# AQUA
# MODIS_STACK_image <- stack("AQUA_MODIS_DUST_event_02_April_2015_1km.tif")
# TERRA
MODIS_STACK_image <- stack("TERRA_MODIS_DUST_event_02_April_2015_1km.tif")

####### color pallet

vec_all <- as.vector(MODIS_STACK_image)

# max_val<- (max(vec_all, na.rm = T))
max_val <- 4
# min_val<- (min(vec_all,  na.rm = T))
min_val <- 0


stat_dat <- summary(as.vector(MODIS_STACK_image))
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

# TERRA
raster_MODIS <- stack("TERRA_MODIS_DUST_event_02_April_2015_1km.tif")

for (i in 1:length(raster_MODIS@layers)) {
  name_time <- TS[i]
 TERRA_images <- raster("TERRA_MODIS_DUST_event_02_April_2015_1km.tif", band = i)
   plot(TERRA_images)
  
  h <- rasterVis::levelplot(TERRA_images, 
                            # h <- rasterVis::levelplot(TERRA_images, 
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

#############################################################################################
#############################################################################################
#############################################################################################

#############################################################################################
#### pictures for MASKS from AOD AQUA #######################################################

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

# gerate a time sequence of 10 days
start <- as.POSIXct("2015-03-28 13:30")  # MODIS AQUA
# start <- as.POSIXct("2015-03-28 10:30")  # MODIS TERRA
interval <- 60*12 #minutes (1 day interval)
end <- start + as.difftime(9, units="days")  # 6 days
TS <- seq(from=start, by=interval*60*2, to=end)


setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MODIS_10km/AQUA")
# setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MODIS_10km/TERRA")

output_folder_AQUA <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MODIS_10km/AQUA/plots_1km/"
# output_folder_TERRA <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MODIS_10km/TERRA/plots_1km/"

# AQUA
MASKS_STACK_image <- stack("MASK_DUST_AQUA_MODIS_event_02_April_2015.tif")

####### color pallet

# max_val<- (max(vec_all, na.rm = T))
max_val <- 1
# min_val<- (min(vec_all,  na.rm = T))
min_val <- 0


low_IQR<- 0
high_IQR <- 1

cols <-  colorRampPalette(c("white", "red"))

# i <- 5

########################
### plots of maps ######
########################

# AQUA
raster_MODIS <- stack("MASK_DUST_AQUA_MODIS_event_02_April_2015.tif")


for (i in 1:length(raster_MODIS@layers)) {
  name_time <- TS[i]
  AQUA_MASKS <- raster("MASK_DUST_AQUA_MODIS_event_02_April_2015.tif", band = i)
  plot(AQUA_MASKS)
  
  h <- rasterVis::levelplot(AQUA_MASKS, 
                            # h <- rasterVis::levelplot(TERRA_images, 
                            margin=FALSE, main= as.character(name_time),
                            xlab = "",
                            ylab = "",
                            ## about colorbar
                            colorkey = FALSE,   
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
  
  png(paste0(output_folder_AQUA ,"MASK_MODIS_", str_sub(name_time, start = 1, end = -10), "_",
             str_sub(name_time, start = 12, end = -7), "_",
             str_sub(name_time, start = 15, end = -4),
             ".png"), width = 900, height = 900,
      units = "px", pointsize = 50,
      bg = "white", res = 200)
  print(h)
  dev.off()
  
}

########################################################################################
#### pictures for MASKS from AOD TERRA #################################################

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

# gerate a time sequence of 10 days
start <- as.POSIXct("2015-03-28 10:30")  # MODIS TERRA
interval <- 60*12 #minutes (1 day interval)
end <- start + as.difftime(9, units="days")  # 6 days
TS <- seq(from=start, by=interval*60*2, to=end)

setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MODIS_10km/TERRA")

output_folder_TERRA <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MODIS_10km/TERRA/plots_1km/"

# TERRA
MASKS_STACK_image <- stack("MASK_DUST_TERRA_MODIS_event_02_April_2015.tif")

####### color pallet

# max_val<- (max(vec_all, na.rm = T))
max_val <- 1
# min_val<- (min(vec_all,  na.rm = T))
min_val <- 0


low_IQR<- 0
high_IQR <- 1

cols <-  colorRampPalette(c("white", "red"))

# i <- 5

########################
### plots of maps ######
########################

# TERRA
raster_MODIS <- stack("MASK_DUST_TERRA_MODIS_event_02_April_2015.tif")


for (i in 1:length(raster_MODIS@layers)) {
  name_time <- TS[i]
  TERRA_MASKS <- raster("MASK_DUST_TERRA_MODIS_event_02_April_2015.tif", band = i)
  plot(TERRA_MASKS)
  
  h <- rasterVis::levelplot(TERRA_MASKS, 
                            # h <- rasterVis::levelplot(TERRA_images, 
                            margin=FALSE, main= as.character(name_time),
                            xlab = "",
                            ylab = "",
                            ## about colorbar
                            colorkey = FALSE,   
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
  
  png(paste0(output_folder_TERRA ,"MASK_MODIS_", str_sub(name_time, start = 1, end = -10), "_",
             str_sub(name_time, start = 12, end = -7), "_",
             str_sub(name_time, start = 15, end = -4),
             ".png"), width = 900, height = 900,
      units = "px", pointsize = 50,
      bg = "white", res = 200)
  print(h)
  dev.off()
  
}



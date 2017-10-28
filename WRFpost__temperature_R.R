
library(RNetCDF)
library(rgdal)
library(ncdf4)
library(raster)
library(stringr)
library(leaflet)


# list .nc files


###############################################
##### ORIGINAL LAND COVER #####################
###############################################

setwd("C:/Users/aaldababseh/WRF_PostProcessing_DATA/Test2_May_LC/Original")

patt<- ".nc"
filenames <- list.files(pattern = patt)

# gerate a time sequence for the WRF-Chem run at intervals of 1 hour (should be 145 images), 6 days
start <- as.POSIXct("2060-05-01 00:00")
interval <- 60 #minutes
end <- start + as.difftime(31, units="days")
TS <- seq(from=start, by=interval*60, to=end)
TS <- TS[1:744]
name <- str_sub(filenames, start = 1, end = -4)


# inizialise an empty raster to stack ALL HOURS together in an unique raster 
# filenames <- filenames[2]

  import_nc_WRF <- function(filenames){
  all_rasters <- stack()    # stack ALL 73 HOURS together in an unique raster
    
  j <- 4
  
  for(j in 1:length(filenames)) {
    
  WRF_file <- open.nc(filenames[j])
  WRF_file <- read.nc(WRF_file)
  name_vari <- names(WRF_file)
  name_vari

#### only one variable (temperature 2m) == var = 14
  
     var_value<-(WRF_file[14])
     names(var_value) <- "xxyyzz"
     var_value <- (var_value$xxyyzz)
     LON <-WRF_file$lon
     LAT <-WRF_file$lat
     
     xmn = min(LON)
     xmx = max(LON)
     ymn = min(LAT)
     ymx = max(LAT)

          # i = 5

    MMM <-  t(var_value[ , ])    # map is upside down 
    MMM <- MMM[nrow(MMM):1, ]
    r <- raster(MMM, xmn, xmx, ymn,  ymx, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    plot(r)
    all_rasters <- stack(all_rasters,r)
  }

  return(all_rasters)
 }

 
BBB <- import_nc_WRF(filenames)

writeRaster(BBB, "T_2060_May_Original_Hourly.tif" , options= "INTERLEAVE=BAND", overwrite=T)



##########################################
##### NEW LAND COVER #####################
##########################################

setwd("C:/Users/aaldababseh/WRF_PostProcessing_DATA/Test2_May_LC/LC")

patt<- ".nc"
filenames <- list.files(pattern = patt)

# gerate a time sequence for the WRF-Chem run at intervals of 1 hour (should be 145 images), 6 days
start <- as.POSIXct("2060-05-01 00:00")
interval <- 60 #minutes
end <- start + as.difftime(31, units="days")
TS <- seq(from=start, by=interval*60, to=end)
TS <- TS[1:744]
name <- str_sub(filenames, start = 1, end = -4)


# inizialise an empty raster to stack ALL HOURS together in an unique raster 
# filenames <- filenames[2]

import_nc_WRF <- function(filenames){
  all_rasters <- stack()    # stack ALL 73 HOURS together in an unique raster
  
  for(j in 1:length(filenames)) {
    
    WRF_file <- open.nc(filenames[j])
    WRF_file <- read.nc(WRF_file)
    name_vari <- names(WRF_file)
    
    
    #### only one variable (temperature 2m) == var = 14
    
    var_value<-(WRF_file[14])
    names(var_value) <- "xxyyzz"
    var_value <- (var_value$xxyyzz)
    LON <-WRF_file$lon
    LAT <-WRF_file$lat
    
    xmn = min(LON)
    xmx = max(LON)
    ymn = min(LAT)
    ymx = max(LAT)
    
    # i = 5
    
    MMM <-  t(var_value[ , ])    # map is upside down 
    MMM <- MMM[nrow(MMM):1, ]
    r <- raster(MMM, xmn, xmx, ymn,  ymx, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    plot(r)
    all_rasters <- stack(all_rasters,r)
  }
  
  return(all_rasters)
}


BBB <- import_nc_WRF(filenames)

writeRaster(BBB, "T_2060_May_LC_Hourly.tif" , options= "INTERLEAVE=BAND", overwrite=T)




#########################################################################################
########################################################################################


library(RNetCDF)
library(rgdal)
library(ncdf4)
library(raster)
library(stringr)
library(leaflet)


# subtract new land cover minus original

T_original <- stack("C:/Users/aaldababseh/WRF_PostProcessing_DATA/Test2_May_LC/Original/T_2060_May_Original_Hourly.tif")
Mean_T_original <- mean(T_original)
MAX_T_original <- max(T_original)
MIN_T_original <- min(T_original)
plot(Mean_T_original)
plot(MAX_T_original)
plot(MIN_T_original)

T_LC <- stack("C:/Users/aaldababseh/WRF_PostProcessing_DATA/Test2_May_LC/LC/T_2060_May_LC_Hourly.tif")
Mean_T_LC <- mean(T_LC)
MAX_T_LC <- max(T_LC)
MIN_T_LC <- min(T_LC)
plot(Mean_T_LC)
plot(MAX_T_LC)
plot(MIN_T_LC)


Diff_LCO2_mean <- Mean_T_LC - Mean_T_original
plot(Diff_LCO2_mean)
writeRaster(Diff_LCO2_mean, "C:/Users/aaldababseh/WRF_PostProcessing_DATA/Test2_May_LC/Difference2_mean_temp_2060_May.tif" , options= "INTERLEAVE=BAND", overwrite=T)



Diff_LCO2_min <- MIN_T_LC - MIN_T_original
plot(Diff_LCO2_min)
writeRaster(Diff_LCO2_min, "C:/Users/aaldababseh/WRF_PostProcessing_DATA/Test2_May_LC/Difference2_min_temp_2060_May.tif" , options= "INTERLEAVE=BAND", overwrite=T)



Diff_LCO2_max <- MAX_T_LC - MAX_T_original
plot(Diff_LCO2_max)
writeRaster(Diff_LCO2_max, "C:/Users/aaldababseh/WRF_PostProcessing_DATA/Test2_May_LC/Difference2_max_temp_2060_May.tif" , options= "INTERLEAVE=BAND", overwrite=T)



#########################################################################################
########################################################################################

#######################################################################################
#########################################################################################
#### plot maps ##########################################################################

library(RColorBrewer)
library(raster)
library(classInt)
library(stringr)
library(ggplot2)

library(viridis)
library(lattice)

# load raster stack ---------------------------------

diff_map <- raster("C:/Users/aaldababseh/WRF_PostProcessing_DATA/Test2_May_LC/Difference2_mean_temp_2060_May.tif")
# diff_map <- raster("C:/Users/aaldababseh/WRF_PostProcessing_DATA/Test2_May_LC/Difference2_min_temp_2060_May.tif")
# diff_map <- raster("C:/Users/aaldababseh/WRF_PostProcessing_DATA/Test2_May_LC/Difference2_max_temp_2060_May.tif")
output_folder <- "C:/Users/aaldababseh/WRF_PostProcessing_DATA/Test2_May_LC/"

####### color pallet

vec_all <- as.vector(diff_map)

min_val<- (min(vec_all,  na.rm = T))
max_val<- (max(vec_all, na.rm = T))


stat_dat <- summary(as.vector(diff_map))
IQR <- (as.numeric((stat_dat[5]-stat_dat[2])* 2))# n is the space after IQR

low_IQR<- if(floor(min_val) > floor(as.numeric((stat_dat[2]- IQR)))) floor(min_val) else floor(as.numeric((stat_dat[2]- IQR)))
high_IQR <-if ( max_val > (as.numeric((stat_dat[5]+IQR)))) max_val else (as.numeric((stat_dat[5]+IQR)))

# cool = rainbow(25, start=rgb2hsv(col2rgb('green'))[1], end=rgb2hsv(col2rgb('royalblue2'))[1])
# cool_2 = rainbow(25, start=rgb2hsv(col2rgb('yellow'))[1], end=rgb2hsv(col2rgb('green'))[1])
# warm = rainbow(150, start=rgb2hsv(col2rgb('red'))[1], end=rgb2hsv(col2rgb('yellow'))[1])
# cols = c(rev(cool), rev(cool_2), rev(warm))


cool = rainbow(180, start=rgb2hsv(col2rgb('yellow'))[1], end=rgb2hsv(col2rgb('white'))[1])
warm = rainbow(20, start=rgb2hsv(col2rgb('red'))[1], end=rgb2hsv(col2rgb('yellow'))[1])
cols = c(rev(cool), rev(warm))






########################
### plots of maps ######
########################

Temp_images <- raster("C:/Users/aaldababseh/WRF_PostProcessing_DATA/Test2_May_LC/Difference2_mean_temp_2060_May.tif")
# Temp_images <- raster("C:/Users/aaldababseh/WRF_PostProcessing_DATA/Test2_May_LC/Difference2_min_temp_2060_May.tif")
# Temp_images <- raster("C:/Users/aaldababseh/WRF_PostProcessing_DATA/Test2_May_LC/Difference2_max_temp_2060_May.tif")

plot(Temp_images)

h <- rasterVis::levelplot(Temp_images, 
                          margin=FALSE, main= "T_mean difference in land cover",
                      #    margin=FALSE, main= "T_min difference in land cover",
                       #   margin=FALSE, main= "T_max difference in land cover",
                          xlab = "",
                          ylab = "",
                          ## about colorbar
                          colorkey=list(
                            space='bottom',                   
                            labels= list(at= floor(as.numeric( seq(low_IQR, high_IQR, length.out=7))),
                                         font=3),
                            axis.line=list(col='black'),
                            width=0.75
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
                          at=unique(c(seq(low_IQR, high_IQR, length.out=200))),
                          names.attr=rep(names(Temp_images))) 
#  latticeExtra::layer(sp.polygons(shp_ME))
h

png(paste0(output_folder,"T_mean_diff_land_cover.png"), width = 900, height = 900,
    units = "px", pointsize = 50,
    bg = "white", res = 200)
print(h)
dev.off()


# png(paste0(output_folder,"T_min_diff_land_cover.png"), width = 900, height = 900,
#     units = "px", pointsize = 50,
#     bg = "white", res = 200)
# print(h)
# dev.off()



# png(paste0(output_folder,"T_max_diff_land_cover.png"), width = 900, height = 900,
#     units = "px", pointsize = 50,
#     bg = "white", res = 200)
# print(h)
# dev.off()






#########################################################################################
########################################################################################
#########################################################################################
########################################################################################
#########################################################################################
########################################################################################
#########################################################################################
########################################################################################
 #########################################################################################
 #### plot maps ##########################################################################

 library(RColorBrewer)
 library(raster)
 library(classInt)
 library(stringr)
 library(ggplot2)
 
 library(viridis)
 library(lattice)
 
 #### import the Arabian Peninsusula domain #############
 
 # dir_ME <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRFChem_domain"
 # dir_ME <- "D:/Dust_Event_UAE_2015/WRFChem_domain"
 ### shapefile for WRF_domain
 # shp_ME <- readOGR(dsn = dir_ME, layer = "ADMIN_domain_d01_12km_WRFChem")
 # shp_ME <- spTransform(shp_ME, CRS("+init=epsg:4326"))
 
 # plot(shp_ME)
 
 # set directory where we want to save the images
 setwd("C:/Users/aaldababseh/WRF_PostProcessing_DATA/LC_8.5/2060/2060_05_01/images_png")
 # setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/images_png")
 
 
 # gerate a time sequence for a given day every 60 minuntes (should be 73 images)
start <- as.POSIXct("2060-05-01 00:00")
interval <- 60 #minutes
end <- start + as.difftime(4, units="days")
TS <- seq(from=start, by=interval*60, to=end)
TS <- TS[1:97]
name <- str_sub(filenames, start = 1, end = -4)

 # i = 4
 
 # load raster stack ---------------------------------
 
 WRF_STACK_image <- stack("C:/Users/aaldababseh/WRF_PostProcessing_DATA/LC_8.5/2060/2060_05_01/temperature_2060_May_LC.tif")
 output_folder <- "C:/Users/aaldababseh/WRF_PostProcessing_DATA/LC_8.5/2060/2060_05_01/images_png/"
 
 ####### color pallet
 
 # recalibrate the model output (--> AOD*6.25)
 # WRF_STACK_image <- WRF_STACK_image*6.25
 
 vec_all <- as.vector(WRF_STACK_image)
 
 min_val<- (min(vec_all,  na.rm = T))
# min_val <- 0
 max_val<- (max(vec_all, na.rm = T))
# max_val <- 2200
 
 
 
 stat_dat <- summary(as.vector(WRF_STACK_image))
 IQR <- (as.numeric((stat_dat[5]-stat_dat[2])* 2))# n is the space after IQR
 
 low_IQR<- if(floor(min_val) > floor(as.numeric((stat_dat[2]- IQR)))) floor(min_val) else floor(as.numeric((stat_dat[2]- IQR)))
 high_IQR <-if ( max_val > (as.numeric((stat_dat[5]+IQR)))) max_val else (as.numeric((stat_dat[5]+IQR)))
 
 # cool = rainbow(25, start=rgb2hsv(col2rgb('cyan'))[1], end=rgb2hsv(col2rgb('blue'))[1])
 cool = rainbow(50, start=rgb2hsv(col2rgb('green'))[1], end=rgb2hsv(col2rgb('royalblue2'))[1])
 cool_2 = rainbow(25, start=rgb2hsv(col2rgb('yellow'))[1], end=rgb2hsv(col2rgb('green'))[1])
 warm = rainbow(125, start=rgb2hsv(col2rgb('red'))[1], end=rgb2hsv(col2rgb('yellow'))[1])
 cols = c(rev(cool), rev(cool_2), rev(warm))
 
 # greenyellow at the place of yellow?
 
 # i <- 57
 
 
 ########################
 ### plots of maps ######
 ########################
 
 Temp_images <- stack("C:/Users/aaldababseh/WRF_PostProcessing_DATA/LC_8.5/2060/2060_05_01/temperature_2060_May_LC.tif")
 
 for (i in 1:length(Temp_images@layers)) {
   TITLE <- paste(TS[i], " (UTC)")
   name_time <- TS[i]
   Temp_images <- raster("C:/Users/aaldababseh/WRF_PostProcessing_DATA/LC_8.5/2060/2060_05_01/temperature_2060_May_LC.tif", band = i)
   plot(Temp_images)
   
   h <- rasterVis::levelplot(Temp_images, 
                             margin=FALSE, main= as.character(TITLE),
                             xlab = "",
                             ylab = "",
                             ## about colorbar
                             colorkey=list(
                               space='bottom',                   
                               labels= list(at= floor(as.numeric( seq(low_IQR, high_IQR, length.out=7))),
                                            font=3),
                               axis.line=list(col='black'),
                               width=0.75
                               # title=expression(paste("            mass") )  
                               # title=expression(paste("           ",PM[10], " (µg/",m^3, ")"))
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
                             at=unique(c(seq(low_IQR, high_IQR, length.out=200))),
                             names.attr=rep(names(Temp_images))) 
   #  latticeExtra::layer(sp.polygons(shp_ME))
   h
   
   png(paste0(output_folder ,str_sub(name_time, start = 1, end = -10), "_",
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
 # magick -delay 50 -loop 0 *.png WRF_Chem_DUST_event_02_April_2015.gif
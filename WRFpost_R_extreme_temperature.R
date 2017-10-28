
library(RNetCDF)
library(rgdal)
library(ncdf4)
library(raster)
library(stringr)
library(leaflet)


# list .nc files for extreme files

##########################################
##### NEW LAND COVER #####################
##########################################


setwd("C:/Users/aaldababseh/WRF_PostProcessing_DATA/Test_May_LC/2060_extrm")

patt<- ".nc"
filenames <- list.files(pattern = patt)
# load WRFChem output from Mike (only dust concentration simulation for the 2nd April 2015 dust storm)
# filenames <- "wrfpost_xtrm_d02_2060-05-01_00_00_00.nc"  


start <- as.Date("2060-05-01")
interval <- 1 #days
end <- start + as.difftime(30, units="days")
TS <- seq(from=start, by= 1, to=end) # minutes (24h * 5)
TS

  import_nc_WRF <- function(filenames){
    
  # inizialise an empty raster to stack ALL HOURS together in an unique raster 
  all_rasters <- stack()  
    
  for(j in 1:length(filenames)) {
  WRF_file <- open.nc(filenames[j])
  WRF_file <- read.nc(WRF_file)
  name_vari <- names(WRF_file)
  

#### only one variable (temperature 2m max) == var = 11
  
     var_value<-(WRF_file[11])   
     names(var_value) <- "xxyyzz"
     var_value <- (var_value$xxyyzz)
     LON <-WRF_file$lon
     LAT <-WRF_file$lat
     
     xmn = min(LON)
     xmx = max(LON)
     ymn = min(LAT)
     ymx = max(LAT)

          # j = 3

  for (i in 1:dim(var_value)[3]){    
    MMM <-  t(var_value[ , ,i])    # map is upside down 
    MMM <- MMM[nrow(MMM):1, ]
    r <- raster(MMM, xmn, xmx, ymn,  ymx, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    plot(r)
    all_rasters <- stack(all_rasters,r)
  }
}
  
  return(all_rasters)
  }

# create a raster stack 
BBB <- import_nc_WRF(filenames)

plot(BBB)
A <- BBB[[2:5]]
B <- BBB[[7:8]]
C <- BBB[[10:14]]
D <- BBB[[16:17]]
O <- BBB[[19:20]]
E <- BBB[[22:25]]
K <- BBB[[28:30]]
plot(K)

AVG <- mean(A, B, C, D, O, E, K)
plot(AVG)

writeRaster(AVG, "temperature_MAX_2060_May_LC.tif" , options= "INTERLEAVE=BAND", overwrite=T)


###############################################
##### ORIGINAL LAND COVER #####################
###############################################

setwd("C:/Users/aaldababseh/WRF_PostProcessing_DATA/Test_May_LC/2060_OriginalLC")

patt<- ".nc"
filenames <- list.files(pattern = patt)
# load WRFChem output from Mike (only dust concentration simulation for the 2nd April 2015 dust storm)
# filenames <- "wrfpost_xtrm_d02_2060-05-01_00_00_00.nc"  


start <- as.Date("2060-05-01")
interval <- 1 #days
end <- start + as.difftime(30, units="days")
TS <- seq(from=start, by= 1, to=end) # minutes (24h * 5)
TS

import_nc_WRF <- function(filenames){
  
  # inizialise an empty raster to stack ALL HOURS together in an unique raster 
  all_rasters <- stack()  
  
  for(j in 1:length(filenames)) {
    WRF_file <- open.nc(filenames[j])
    WRF_file <- read.nc(WRF_file)
    name_vari <- names(WRF_file)
    
    
    #### only one variable (temperature 2m max) == var = 11
    
    var_value<-(WRF_file[11])   
    names(var_value) <- "xxyyzz"
    var_value <- (var_value$xxyyzz)
    LON <-WRF_file$lon
    LAT <-WRF_file$lat
    
    xmn = min(LON)
    xmx = max(LON)
    ymn = min(LAT)
    ymx = max(LAT)
    
    # i = 3
    
    for (i in 1:dim(var_value)[3]){    
      MMM <-  t(var_value[ , ,i])    # map is upside down 
      MMM <- MMM[nrow(MMM):1, ]
      r <- raster(MMM, xmn, xmx, ymn,  ymx, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
      plot(r)
      all_rasters <- stack(all_rasters,r)
    }
  }
  
  return(all_rasters)
}

# create a raster stack 
BBB <- import_nc_WRF(filenames)

plot(BBB)

A <- BBB[[2]]
B <- BBB[[4:9]]
C <- BBB[[11]]
D <- BBB[[13]]
O <- BBB[[15:16]]
E <- BBB[[20:22]]
I <- BBB[[24]]
K <- BBB[[26:27]]
L <- BBB[[29:30]]
plot(E)

AVG <- mean(A, B, C, D, O, E, I, K, L)
plot(AVG)

writeRaster(AVG, "temperature_MAX_2060_May_Original.tif" , options= "INTERLEAVE=BAND", overwrite=T)

# subtract new land cover minus original

TMax_original <- raster("C:/Users/aaldababseh/WRF_PostProcessing_DATA/Test_May_LC/2060_OriginalLC/temperature_MAX_2060_May_Original.tif")
plot(TMax_original)

TMax_LC <- raster("C:/Users/aaldababseh/WRF_PostProcessing_DATA/Test_May_LC/2060_extrm/temperature_MAX_2060_May_LC.tif")
plot(TMax_LC)

Diff_LCO <- TMax_LC - TMax_original
plot(Diff_LCO)
writeRaster(Diff_LCO, "C:/Users/aaldababseh/WRF_PostProcessing_DATA/Test_May_LC/Difference_temp_MAX_2060_May.tif" , options= "INTERLEAVE=BAND", overwrite=T)


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
 
 #### import the Arabian Peninsusula domain #############
 
 
 # load raster stack ---------------------------------
 
diff_map <- raster("C:/Users/aaldababseh/WRF_PostProcessing_DATA/Test_May_LC/Difference_temp_MAX_2060_May.tif")
output_folder <- "C:/Users/aaldababseh/WRF_PostProcessing_DATA/Test_May_LC/"
 
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
 

 cool = rainbow(175, start=rgb2hsv(col2rgb('yellow'))[1], end=rgb2hsv(col2rgb('blue'))[1])
 warm = rainbow(25, start=rgb2hsv(col2rgb('red'))[1], end=rgb2hsv(col2rgb('yellow'))[1])
 cols = c(rev(cool), rev(warm))
 
 
 
 

 
 ########################
 ### plots of maps ######
 ########################
 
 Temp_images <- raster("C:/Users/aaldababseh/WRF_PostProcessing_DATA/Test_May_LC/Difference_temp_MAX_2060_May.tif")
 
  Temp_images <- raster("C:/Users/aaldababseh/WRF_PostProcessing_DATA/Test_May_LC/Difference_temp_MAX_2060_May.tif")
   plot(Temp_images)
   
   h <- rasterVis::levelplot(Temp_images, 
                             margin=FALSE, main= "Tmax difference in land cover",
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
   
   png(paste0(output_folder,"Tmax_diff_land_cover.png"), width = 900, height = 900,
       units = "px", pointsize = 50,
       bg = "white", res = 200)
   print(h)
   dev.off()
   
   


 
 
 
 
 # to make a movie.......
 # to use with ImageMagik using the commnad line cmd in windows
 # cd into the directory where there are the png files
 # magick -delay 50 -loop 0 *.png WRF_Chem_DUST_event_02_April_2015.gif
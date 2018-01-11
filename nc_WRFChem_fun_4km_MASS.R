
library(RNetCDF)
library(rgdal)
library(ncdf4)
library(raster)
library(stringr)


# list .nc files
setwd("D:/Dust_Event_UAE_2015/WRF_trial_runs")
# 4km
# setwd("D:/Dust_Event_UAE_2015/WRF_trial_runs/DUST_AOD_FK/extinction55/4km")
setwd("D:/Dust_Event_UAE_2015/WRF_trial_runs/DUST_AOD_FK/DUST/4km")
# setwd("D:/Dust_Event_UAE_2015/WRF_trial_runs/DUST_AOD_FK/extinction55/4km/archived")


patt<- ".nc"
filenames <- list.files(pattern = patt)
filenames <- filenames
# filenames <- filenames[1:4]

#############################################
## function to import multiple .nc files ####
#############################################

# filenames <- filenames[1]


# gerate a time sequence for the WRF-Chem run at intervals of 1 hour (should be 145 images), 6 days
start <- as.POSIXct("2015-03-31 00:00")
interval <- 60 #minutes
end <- start + as.difftime(6, units="days")
TS <- seq(from=start, by=interval*60, to=end)
TS <- TS[1:96]



 import_nc_WRF <- function(filenames){

   
  ######
  #### looping the variables of the nc files

 # jj = 1
  
  #### only one variable (dust_e) == var = 18 (total mass concentration dust)
  all_rasters <- stack()
  qq<- 1
  
   for(jj in 1:length(filenames)) {
     
     
     WRF_file <- open.nc(filenames[jj])
     WRF_file <- read.nc(WRF_file)
     name_vari <- names(WRF_file)
     name <- str_sub(filenames[jj], start = 1, end = -25)
     
     var_value <- (WRF_file[18])    #  only one variable (dust_e) == var = 18
     names(var_value)<- "xxyyzz"
     var_value <- (var_value$xxyyzz)
     LON <- WRF_file$lon
     LAT <- WRF_file$lat
     
     xmn= min(LON)
     xmx=max(LON)
     ymn=min(LAT)
     ymx=max(LAT)
     
     # j = 5
     
    
   
  for (j in 1:dim(var_value)[4]){      # time dimension (always 24)
    MMM <-  t(var_value[ , ,1,j])   # map is upside down  (only consider surface level)
    MMM <- MMM[nrow(MMM):1, ]
    r <- raster(MMM, xmn, xmx, ymn,  ymx, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    plot(r)
    name_time <- TS[qq]
    names(r) <- paste("AOD_WRF_Chem_", name_time, sep = "")
    all_rasters <- stack(all_rasters,r)
    qq<- qq+1
  }
}
  
  write.csv(names(all_rasters), paste(name,  ".csv", sep=""))
  return(all_rasters)
}

 
 #BBB <- lapply(filenames, import_nc_WRF) 
 BBB <- import_nc_WRF(filenames)
 
 writeRaster(BBB, "DUST_mass_4km_WRFChem_DUST1_Em3.tif" , options= "INTERLEAVE=BAND", overwrite=T)


#########################################################################################
#### plot maps ##########################################################################

library(leaflet)
library(webshot)
library(htmlwidgets)
library(RColorBrewer)
library(raster)
library(classInt)
library(stringr)
library(ggplot2)

library(viridis)
library(lattice)

#### import the Arabian Peninsusula domain #############

# dir_ME <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRFChem_domain"
dir_ME <- "D:/Dust_Event_UAE_2015/WRFChem_domain"
### shapefile for WRF_domain
shp_ME <- readOGR(dsn = dir_ME, layer = "ADMIN_domain_d01_4km_WRFChem")
shp_ME <- spTransform(shp_ME, CRS("+init=epsg:4326"))

plot(shp_ME)

# set directory where we want to save the images
setwd("D:/Dust_Event_UAE_2015/WRF_trial_runs/images_png")
# setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/images_png")


# gerate a time sequence for a given day every 60 minuntes (should be 73 images)
start <- as.POSIXct("2015-03-31 00:00")
interval <- 60 #minutes
end <- start + as.difftime(6, units="days")
TS <- seq(from=start, by=interval*60, to=end)
TS <- TS[1:96]

# i = 4

# load raster stack ---------------------------------

WRF_STACK_image <- stack("D:/Dust_Event_UAE_2015/WRF_trial_runs/DUST_AOD_FK/DUST/4km/DUST_mass_4km_WRFChem_DUST1_Em3.tif")
output_folder <- "D:/Dust_Event_UAE_2015/WRF_trial_runs/images_png/mass_4km_Em3/"

####### color pallet

# recalibrate the model output (--> AOD*6.25)
# WRF_STACK_image <- WRF_STACK_image*6.25

vec_all <- as.vector(WRF_STACK_image)

min_val<- (min(vec_all,  na.rm = T))
min_val <- 0
max_val<- (max(vec_all, na.rm = T))
max_val <- 2200



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

MASS_images <- stack("D:/Dust_Event_UAE_2015/WRF_trial_runs/DUST_AOD_FK/DUST/4km/DUST_mass_4km_WRFChem_DUST1_Em3.tif")

for (i in 1:length(MASS_images@layers)) {
  TITLE <- paste(TS[i], " (UTC)")
  name_time <- TS[i]
  MASS_images <- raster("D:/Dust_Event_UAE_2015/WRF_trial_runs/DUST_AOD_FK/DUST/4km/DUST_mass_4km_WRFChem_DUST1_Em3.tif", band = i)
    # plot(AOD_images)
  
  h <- rasterVis::levelplot(MASS_images, 
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
                            names.attr=rep(names(MASS_images))) +
    latticeExtra::layer(sp.polygons(shp_ME))
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




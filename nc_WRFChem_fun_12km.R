
library(RNetCDF)
library(rgdal)
library(ncdf4)
library(raster)
library(stringr)


# list .nc files
setwd("D:/Dust_Event_UAE_2015/WRF_trial_runs")
# 12km
setwd("D:/Dust_Event_UAE_2015/WRF_trial_runs/DUST_AOD_FK/extinction55/12km")
# setwd("D:/Dust_Event_UAE_2015/WRF_trial_runs/DUST_ADO_FK/DUST/12km")

# 4km
# setwd("D:/Dust_Event_UAE_2015/WRF_trial_runs/DUST_AOD_FK/extinction55/4km")
# setwd("D:/Dust_Event_UAE_2015/WRF_trial_runs/DUST_AOD_FK/DUST/4km")

patt<- ".nc"
filenames <- list.files(pattern = patt)
filenames <- filenames

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
  
  #### only one variable (extcof55_fk) == var = 21
  all_rasters <- stack()
  qq<- 1
  
   for(jj in 1:length(filenames)) {
     
     
     WRF_file <- open.nc(filenames[jj])
     WRF_file <- read.nc(WRF_file)
     name_vari <- names(WRF_file)
     name <- str_sub(filenames[jj], start = 1, end = -25)
     
     
     
     var_value <- (WRF_file[21])    #  only one variable (extcof55_fk) == var = 21
     z_e <- WRF_file[20]   
     names(var_value)<- "xxyyzz"
     names(z_e) <- "xxyyzz"
     var_value<- (var_value$xxyyzz)
     z_e <- (z_e$xxyyzz)
     LON <- WRF_file$lon
     LAT <- WRF_file$lat
     
     xmn= min(LON)
     xmx=max(LON)
     ymn=min(LAT)
     ymx=max(LAT)
     
     #  i <- 5
     
     # get altitude levels (from z_e) and compute steps
     
     AOD <- 0
     
     for (i in 1:(dim(z_e)[3]-1)){    # eta (height, 44 levels)
       dz <- (z_e[ , ,i+1, ] - z_e[ , , i, ])*0.001   # height levels (km)
       MMM <-  var_value[ , ,i, ]
       AOD <-  AOD + MMM*dz
     }
     
     
     # j = 5
     
    
   
  for (j in 1:dim(var_value)[4]){      # time dimension (always 24)
    MMM <-  t(AOD[ , ,j])   # map is upside down 
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
 

 
 # make a large stack raster with the 19*3=57 layeer for DUST_1
 
 # ras_stack<- stack()
 # 
 # 
 # for (jj in 1:48){
 # plot(BBB[[jj]],kk)
 #     ras <- raster(BBB[[jj]], kk)
 #     ras_stack<- stack(ras_stack,ras)
 #   }
 # 
 # 
 # 
 # AAA <- ras_stack[[70]]
 # plot(AAA) 
 
 
writeRaster(BBB, "AOD_12km_WRFChem_DUST300.tif" , options= "INTERLEAVE=BAND", overwrite=T)
writeRaster(BBB, "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_AOD_FK/extinction55/12km/AOD_12km_WRFChem_DUST300.tif" , options= "INTERLEAVE=BAND", overwrite=T)



#########################################################################################
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

# save images as webshot from leaflet
# reload rasters by band or layers (145 scenes)

# gerate a time sequence for a given day every 60 minuntes (should be 73 images)
start <- as.POSIXct("2015-03-31 00:00")
interval <- 60 #minutes
end <- start + as.difftime(6, units="days")
TS <- seq(from=start, by=interval*60, to=end)
TS <- TS[1:96]

# i = 4

# load raster stack ---------------------------------

# WRF_STACK_image <- stack("D:/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif")
# WRF_STACK_image <- stack("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif")

# WRF_STACK_image <- stack("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/AOD_WRFChem_02April2015_stack_5_DAYS.tif")
WRF_STACK_image <- stack("D:/Dust_Event_UAE_2015/WRF_trial_runs/DUST_AOD_FK/extinction55/4km/AOD_4km_WRFChem_DUST300.tif")


output_folder <- "D:/Dust_Event_UAE_2015/WRF_trial_runs/images_png/AOD_dust_3_4km/"

####### color pallet

vec_all <- as.vector(WRF_STACK_image)

max_val<- (max(vec_all, na.rm = T))
min_val<- (min(vec_all,  na.rm = T))


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

AOD_images <- stack("D:/Dust_Event_UAE_2015/WRF_trial_runs/DUST_AOD_FK/extinction55/4km/AOD_4km_WRFChem_DUST300.tif")

for (i in 1:length(AOD_images@layers)) {
  TITLE <- paste(TS[i], " (UTC)")
  name_time <- TS[i]
  AOD_images <- raster("D:/Dust_Event_UAE_2015/WRF_trial_runs/DUST_AOD_FK/extinction55/4km/AOD_4km_WRFChem_DUST300.tif", band = i)
  # plot(AOD_images)
  
  h <- rasterVis::levelplot(AOD_images, 
                            margin=FALSE, main= as.character(TITLE),
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
                            at=unique(c(seq(low_IQR, high_IQR, length.out=200))),
                            names.attr=rep(names(AOD_images))) +
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



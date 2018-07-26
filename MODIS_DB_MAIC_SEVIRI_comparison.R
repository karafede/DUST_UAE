
library(readr)
library(dplyr)
library(lubridate)
library(rgdal)
# install.packages("NISTunits", dependencies = TRUE)
library(NISTunits)
library(raster)
library(leaflet)
library(htmlwidgets)
library(webshot)
library(gstat)

# setwd("D:/Dust_Event_UAE_2015")
setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015")


# load WRFChem output----------------------------------------------------
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

#############################################################
#############################################################

# AQUA ###
# read stack raster MAIAC AQUA & MASKS
dir <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/AQUA/"
stack_MAIAC_AQUA <- stack(paste0(dir,"AQUA_MAIAC_DUST_event_02_April_2015_1km.tif"))
stack_MAIAC_AQUA_MASKS <- stack(paste0(dir,"MASK_DUST_AQUA_MAIAC_event_02_April_2015.tif"))
# subset layers as for MAIAC (1,2,3 APRIL)
stack_MAIAC_AQUA <- stack(stack_MAIAC_AQUA@layers[4:6])
stack_MAIAC_AQUA_MASKS <- stack(stack_MAIAC_AQUA_MASKS@layers[4:6])
plot(stack_MAIAC_AQUA)


# read stack raster MODIS DB AQUA
dir <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MODIS_10km/AQUA/"
stack_MODIS_AQUA <- stack(paste0(dir,"AQUA_MODIS_DUST_event_02_April_2015_1km.tif"))
stack_MODIS_AQUA_MASKS <- stack(paste0(dir,"MASK_DUST_AQUA_MODIS_event_02_April_2015.tif"))
# subset layers as for MAIAC (1,2,3 APRIL)
stack_MODIS_AQUA <- stack(stack_MODIS_AQUA@layers[5:7])
stack_MODIS_AQUA_MASKS <- stack(stack_MODIS_AQUA_MASKS@layers[5:7])


# TERRA ## 
# read stack raster MAIAC TERRA & MASKS
dir <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/TERRA/"
stack_MAIAC_TERRA <- stack(paste0(dir,"TERRA_MAIAC_DUST_event_02_April_2015_1km.tif"))
stack_MAIAC_TERRA_MASKS <- stack(paste0(dir,"MASK_DUST_TERRA_MAIAC_event_02_April_2015.tif"))
# subset layers as for MAIAC (1,2,3 APRIL)
stack_MAIAC_TERRA <- stack(stack_MAIAC_TERRA@layers[4:6])
stack_MAIAC_TERRA_MASKS <- stack(stack_MAIAC_TERRA_MASKS@layers[4:6])
plot(stack_MAIAC_TERRA)

# TERRA ###
# read stack raster MODIS DB TERRA
dir <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MODIS_10km/TERRA/"
stack_MODIS_TERRA <- stack(paste0(dir,"TERRA_MODIS_DUST_event_02_April_2015_1km.tif"))
stack_MODIS_TERRA_MASKS <- stack(paste0(dir,"MASK_DUST_TERRA_MODIS_event_02_April_2015.tif"))
# subset layers as for MAIAC (1,2,3 APRIL)
stack_MODIS_TERRA <- stack(stack_MODIS_TERRA@layers[5:7])
stack_MODIS_TERRA_MASKS <- stack(stack_MODIS_TERRA_MASKS@layers[5:7])

# i <- 5


# make a combine rasters of 3 + 3 = 6 layers
TERRA_stack <- stack(stack_MAIAC_TERRA,
               stack_MODIS_TERRA)

AQUA_stack <- stack(stack_MAIAC_AQUA,
              stack_MODIS_AQUA)


#############################################################
#############################################################

# Load selected SEVIRI data (MASKS)
dir <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/SEVIRI_20150402_outputs/II_method/"
stack_SEVIRI_01_April <- stack(paste0(dir,"Seviri_20150401_METFr_Orig_stack.tif"))
stack_SEVIRI_01_April <- stack_SEVIRI_01_April[[19]]
plot(stack_SEVIRI_01_April)
stack_SEVIRI_02_April <- stack(paste0(dir,"Seviri_20150402_METFr_Orig_stack.tif"))
stack_SEVIRI_02_April <- stack_SEVIRI_02_April[[20]]
plot(stack_SEVIRI_02_April)
stack_SEVIRI_03_April <- stack(paste0(dir,"Seviri_20150403_METFr_Orig_stack.tif"))
stack_SEVIRI_03_April <- stack_SEVIRI_03_April[[18]]
plot(stack_SEVIRI_03_April)

SEVIRI_stack <- stack(stack_SEVIRI_01_April,
                      stack_SEVIRI_02_April,
                      stack_SEVIRI_03_April)

SEVIRI_stack = projectRaster(SEVIRI_stack, stack_MAIAC_TERRA)

####################################################################
### DIFFERENCE and CORRELATION between raster MODIS-MAIAC AQUA #####
####################################################################

all_rasters_TERRA <- stack()    # inizialise the raster stack
all_rasters_TERRA_SEVIRI <- stack()    # inizialise the raster stack
all_rasters_AQUA <- stack()
all_rasters_AQUA_SEVIRI <- stack()    # inizialise the raster stack

# generate a time sequence of 3 days
start <- as.POSIXct("2015-04-01 10:30")  # MODIS TERRA
interval <- 60*12 #minutes (1 day interval)
end <- start + as.difftime(2, units="days")  # 6 days
TS <- seq(from=start, by=interval*60*2, to=end)

# i <- 2

for (i in 1:length(stack_MAIAC_AQUA@layers)) {
  name_time <- TS[i]
# difference
diff_MAIAC_MODIS_TERRA <- stack_MAIAC_TERRA[[i]] - stack_MODIS_TERRA[[i]]    # 10:30 am
diff_MAIAC_SEVIRI_TERRA <- stack_MAIAC_TERRA_MASKS[[i]] - SEVIRI_stack[[i]]    # 10:30 am
# plot(diff_MAIAC_MODIS_TERRA)
plot(diff_MAIAC_SEVIRI_TERRA)
all_rasters_TERRA <- stack(all_rasters_TERRA, diff_MAIAC_MODIS_TERRA)
all_rasters_TERRA_SEVIRI <- stack(all_rasters_TERRA_SEVIRI, diff_MAIAC_SEVIRI_TERRA)
dir <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/TERRA/"
writeRaster(all_rasters_TERRA, paste0(dir,"diff_TERRA_MAIAC_MODIS_DUST_event_02_April_2015_1km.tif") , options= "INTERLEAVE=BAND", overwrite=T)
writeRaster(all_rasters_TERRA_SEVIRI, paste0(dir,"diff_TERRA_MAIAC_SEVIRI_DUST_event_02_April_2015_1km.tif") , options= "INTERLEAVE=BAND", overwrite=T)


print(i)

}

# generate a time sequence of 3 days
start <- as.POSIXct("2015-04-01 13:30")  # MODIS AQUA
interval <- 60*12 #minutes (1 day interval)
end <- start + as.difftime(2, units="days")  # 6 days
TS <- seq(from=start, by=interval*60*2, to=end)

for (i in 1:length(stack_MAIAC_AQUA@layers)) {
name_time <- TS[i]
diff_MAIAC_MODIS_AQUA <- stack_MAIAC_AQUA[[i]] - stack_MODIS_AQUA[[i]]    # 13:30 am
diff_MAIAC_SEVIRI_AQUA <- stack_MAIAC_AQUA_MASKS[[i]] - SEVIRI_stack[[i]]    # 13:30 am
plot(diff_MAIAC_MODIS_AQUA)
# plot(diff_MAIAC_SEVIRI_AQUA)
all_rasters_AQUA <- stack(all_rasters_AQUA, diff_MAIAC_MODIS_AQUA)
all_rasters_AQUA_SEVIRI <- stack(all_rasters_AQUA_SEVIRI, diff_MAIAC_SEVIRI_AQUA)
dir <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/AQUA/"
writeRaster(all_rasters_AQUA, paste0(dir,"diff_AQUA_MAIAC_MODIS_DUST_event_02_April_2015_1km.tif") , options= "INTERLEAVE=BAND", overwrite=T)
# writeRaster(all_rasters_AQUA_SEVIRI, paste0(dir,"diff_AQUA_MAIAC_SEVIRI_DUST_event_02_April_2015_1km.tif") , options= "INTERLEAVE=BAND", overwrite=T)

print(i)

}


# Pearson correlation (TOO HEAVY TO RUN!!!!)
# TERRA 
# pearson_TERRA <- calc(TERRA_stack, fun=function(x) cor(stack_MAIAC_TERRA[1:length(stack_MAIAC_AQUA@layers)], 
#                                                        stack_MODIS_TERRA[1:length(stack_MAIAC_AQUA@layers)], 
#                                                        method='pearson', use= "pairwise.complete.obs"))
# dir <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/TERRA/"
# writeRaster(pearson_TERRA, paste0(dir,"corr_TERRA_MAIAC_MODIS_DUST_event_02_April_2015_1km.tif") , options= "INTERLEAVE=BAND", overwrite=T)
# 
# # AQUA
# pearson_AQUA <- calc(AQUA_stack, fun=function(x) cor(stack_MAIAC_AQUA[1:length(stack_MAIAC_AQUA@layers)], 
#                                                        stack_MODIS_AQUA[1:length(stack_MAIAC_AQUA@layers)], 
#                                                        method='pearson', use= "pairwise.complete.obs"))
# dir <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/AQUA/"
# writeRaster(pearson_AQUA, paste0(dir,"corr_AQUA_MAIAC_MODIS_DUST_event_02_April_2015_1km.tif") , options= "INTERLEAVE=BAND", overwrite=T)


########################################################################################
#### reload data and create images #####################################################

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

# gerate a time sequence of 6 days
start <- as.POSIXct("2015-04-01 13:30")  # MODIS AQUA
# start <- as.POSIXct("2015-03-29 10:30")  # MODIS TERRA
interval <- 60*12 #minutes (1 day interval)
end <- start + as.difftime(2, units="days")  # 6 days
TS <- seq(from=start, by=interval*60*2, to=end)

setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/AQUA")
# setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/TERRA")


output_folder_AQUA <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/AQUA/plots_1km/"
# output_folder_TERRA <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/TERRA/plots_1km"

# AQUA
MAIAC_STACK_image <- stack("diff_AQUA_MAIAC_MODIS_DUST_event_02_April_2015_1km.tif")


####### color pallet

vec_all <- as.vector(MAIAC_STACK_image)

# max_val<- (max(vec_all, na.rm = T))
max_val <- 1
# min_val<- (min(vec_all,  na.rm = T))
min_val <- -1


stat_dat <- summary(as.vector(MAIAC_STACK_image))
IQR <- (as.numeric((stat_dat[5]-stat_dat[2])* 2))# n is the space after IQR

low_IQR<- if(floor(min_val) > floor(as.numeric((stat_dat[2]- IQR)))) floor(min_val) else floor(as.numeric((stat_dat[2]- IQR)))
high_IQR <-if ( max_val > (as.numeric((stat_dat[5]+IQR)))) max_val else (as.numeric((stat_dat[5]+IQR)))
high_IQR <- 1

# i <- 2

low_IQR <- -2
high_IQR <- 2

########################
### plots of maps ######
########################

# AQUA
raster_MAIAC <- stack("diff_AQUA_MAIAC_MODIS_DUST_event_02_April_2015_1km.tif")

for (i in 1:length(raster_MAIAC@layers)) {
  name_time <- TS[i]
  AQUA_images <- raster("diff_AQUA_MAIAC_MODIS_DUST_event_02_April_2015_1km.tif", band = i)
  plot(AQUA_images)
  
  h <- rasterVis::levelplot(AQUA_images, 
                            # h <- rasterVis::levelplot(TERRA_images, 
                            margin=FALSE, main= as.character(name_time),
                            xlab = "",
                            ylab = "",
                            ## about colorbar
                            colorkey=list(
                              space='right', #"bottom"                   
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
                            col.regions = colorRampPalette(c("blue", "white","red")),
                            # col.regions = cols,
                            at=unique(c(seq(low_IQR, high_IQR, length.out=200)))) +
    latticeExtra::layer(sp.polygons(shp_ME))
  h
  
  png(paste0(output_folder_AQUA ,"diff_MAIAC_MODIS_",str_sub(name_time, start = 1, end = -10), "_",
             #  png(paste0(output_folder_TERRA ,str_sub(name_time, start = 1, end = -10), "_",
             str_sub(name_time, start = 12, end = -7), "_",
             str_sub(name_time, start = 15, end = -4),
             ".png"), width = 900, height = 900,
      units = "px", pointsize = 50,
      bg = "white", res = 200)
  print(h)
  dev.off()
  
}



########################################################################################
#### reload data and create images #####################################################

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

# gerate a time sequence of 6 days
start <- as.POSIXct("2015-04-01 10:30")  # MODIS TERRA
interval <- 60*12 #minutes (1 day interval)
end <- start + as.difftime(2, units="days")  # 6 days
TS <- seq(from=start, by=interval*60*2, to=end)

# setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/AQUA")
setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/TERRA")


# output_folder_AQUA <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/AQUA/plots_1km/"
output_folder_TERRA <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/TERRA/plots_1km/"

# AQUA
# MAIAC_STACK_image <- stack("diff_AQUA_MAIAC_MODIS_DUST_event_02_April_2015_1km.tif")
# TERRA
MAIAC_STACK_image <- stack("diff_TERRA_MAIAC_MODIS_DUST_event_02_April_2015_1km.tif")



####### color pallet

vec_all <- as.vector(MAIAC_STACK_image)

# max_val<- (max(vec_all, na.rm = T))
max_val <- 1
# min_val<- (min(vec_all,  na.rm = T))
min_val <- -1


stat_dat <- summary(as.vector(MAIAC_STACK_image))
IQR <- (as.numeric((stat_dat[5]-stat_dat[2])* 2))# n is the space after IQR

low_IQR<- if(floor(min_val) > floor(as.numeric((stat_dat[2]- IQR)))) floor(min_val) else floor(as.numeric((stat_dat[2]- IQR)))
# high_IQR <-if ( max_val > (as.numeric((stat_dat[5]+IQR)))) max_val else (as.numeric((stat_dat[5]+IQR)))
high_IQR <- 1
# i <- 5

low_IQR <- -2
high_IQR <- 2

########################
### plots of maps ######
########################

# TERRA
raster_MAIAC <- stack("diff_TERRA_MAIAC_MODIS_DUST_event_02_April_2015_1km.tif")

for (i in 1:length(raster_MAIAC@layers)) {
  name_time <- TS[i]
  TERRA_images <- raster("diff_TERRA_MAIAC_MODIS_DUST_event_02_April_2015_1km.tif", band = i)
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
                            col.regions = colorRampPalette(c("blue", "white","red")),
                            # col.regions = cols,
                            at=unique(c(seq(low_IQR, high_IQR, length.out=200)))) +
    latticeExtra::layer(sp.polygons(shp_ME))
  h
  
  png(paste0(output_folder_TERRA ,"diff_MAIAC_MODIS_",str_sub(name_time, start = 1, end = -10), "_",
             #  png(paste0(output_folder_TERRA ,str_sub(name_time, start = 1, end = -10), "_",
             str_sub(name_time, start = 12, end = -7), "_",
             str_sub(name_time, start = 15, end = -4),
             ".png"), width = 900, height = 900,
      units = "px", pointsize = 50,
      bg = "white", res = 200)
  print(h)
  dev.off()
  
}

#################################################################################################
##### plot differences between MAIAC and SEVIRI #################################################

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

# gerate a time sequence of 3 days
start <- as.POSIXct("2015-04-01 10:30")  # MODIS TERRA
interval <- 60*12 #minutes (1 day interval)
end <- start + as.difftime(2, units="days")  # 6 days
TS <- seq(from=start, by=interval*60*2, to=end)

setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/TERRA")

output_folder_TERRA <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/TERRA/plots_1km/"

# TERRA
MAIAC_STACK_image <- stack("diff_TERRA_MAIAC_SEVIRI_DUST_event_02_April_2015_1km.tif")



####### color pallet

vec_all <- as.vector(MAIAC_STACK_image)

# max_val<- (max(vec_all, na.rm = T))
max_val <- 1
# min_val<- (min(vec_all,  na.rm = T))
min_val <- -1


stat_dat <- summary(as.vector(MAIAC_STACK_image))
IQR <- (as.numeric((stat_dat[5]-stat_dat[2])* 2))# n is the space after IQR

low_IQR<- if(floor(min_val) > floor(as.numeric((stat_dat[2]- IQR)))) floor(min_val) else floor(as.numeric((stat_dat[2]- IQR)))
high_IQR <-if ( max_val > (as.numeric((stat_dat[5]+IQR)))) max_val else (as.numeric((stat_dat[5]+IQR)))
high_IQR <- 1

# i <- 2
low_IQR <- -1
high_IQR <- 1

########################
### plots of maps ######
########################

# TERRA
raster_MASK <- stack("diff_TERRA_MAIAC_SEVIRI_DUST_event_02_April_2015_1km.tif")

for (i in 1:length(raster_MASK@layers)) {
  name_time <- TS[i]
  MASK_images <- raster("diff_TERRA_MAIAC_SEVIRI_DUST_event_02_April_2015_1km.tif", band = i)
  plot(MASK_images)
  
  h <- rasterVis::levelplot(MASK_images, 
                            # h <- rasterVis::levelplot(TERRA_images, 
                            margin=FALSE, main= as.character(name_time),
                            xlab = "",
                            ylab = "",
                            ## about colorbar
                            colorkey=list(
                              space='bottom',                   
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
                            # col.regions = colorRampPalette(c("white", "yellow","red")),
                            col.regions = colorRampPalette(c("blue", "white","red")),
                            # col.regions = cols,
                            at=unique(c(seq(low_IQR, high_IQR, length.out=200)))) +
    latticeExtra::layer(sp.polygons(shp_ME))
  h
  
  png(paste0(output_folder_TERRA ,"diff_MAIAC_SEVIRI_",str_sub(name_time, start = 1, end = -10), "_",
             #  png(paste0(output_folder_TERRA ,str_sub(name_time, start = 1, end = -10), "_",
             str_sub(name_time, start = 12, end = -7), "_",
             str_sub(name_time, start = 15, end = -4),
             ".png"), width = 900, height = 900,
      units = "px", pointsize = 50,
      bg = "white", res = 200)
  print(h)
  dev.off()
  
}







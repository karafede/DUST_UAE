
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

############################################################################
# read all WRF  bands in a stack raster ####################################


WRF_AOD_09_00_April_1 <- raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_AOD_FK/extinction55/12km/AOD_12km_WRFChem_DUST1_Em3.tif",
                                band = 25)*6.25
plot(WRF_AOD_09_00_April_1)

WRF_AOD_09_00_April_2 <- raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_AOD_FK/extinction55/12km/AOD_12km_WRFChem_DUST1_Em3.tif",
                                band = 51)*6.25
plot(WRF_AOD_09_00_April_2)

WRF_AOD_09_00_April_3 <- raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_AOD_FK/extinction55/12km/AOD_12km_WRFChem_DUST1_Em3.tif",
                                band = 80)*6.25
plot(WRF_AOD_09_00_April_3)

# make a stack

WRF_AOD_stack <- stack(WRF_AOD_09_00_April_1,
                       WRF_AOD_09_00_April_2,
                       WRF_AOD_09_00_April_3)

plot(WRF_AOD_stack)

# WRF_STACK_image <- stack("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif")
# n <- length(WRF_STACK_image@layers)-1
# 
# 
# # generate a time sequence for the WRF-Chem run at intervals of 1 hour (should be 144 images)
# start <- as.POSIXct("2015-03-29 00:00:00")
# interval <- 60 #minutes
# end <- start + as.difftime(6, units="days")
# TS <- seq(from=start, by=interval*60, to=end)   # same time series as the AQ data
# TS <- TS[1:144]


# average WRF rasters from 11am to 1pm (I will get 3 rasters at the same time of the MODIS/MAIAC data)
# MODIS TERRA: from 10 to 11 am
# MODIS AQUA: from 13 to 14


# WRF_2015_03_29_TERRA <- (raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = 11) +
#                      raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = 12))/2
# 
# plot(WRF_2015_03_29_TERRA)
# 
# 
# WRF_2015_03_30_TERRA <- (raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = 35) +
#                      raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = 36))/2
# 
# plot(WRF_2015_03_30_TERRA)
# 
# 
# 
# WRF_2015_03_31_TERRA <- (raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = 59) +
#                    raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = 60))/2
# 
# plot(WRF_2015_03_31_TERRA)
# 
# 
# 
# WRF_2015_04_01_TERRA <- (raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = 83) +
#                      raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = 84))/2
# 
# plot(WRF_2015_04_01_TERRA)
# 
# 
# 
# WRF_2015_04_02_TERRA <- (raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = 107) +
#                      raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = 108))/2
# 
# plot(WRF_2015_04_02_TERRA)
# 
# 
# WRF_2015_04_03_TERRA <- (raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = 131) +
#                      raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = 132))/2
# 
# plot(WRF_2015_04_03_TERRA)
# 
# 
# # MODIS AQUA: from 13 to 14 ##############################################################################################################
# 
# WRF_2015_03_29_AQUA <- (raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = 14) +
#                            raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = 15))/2
# 
# plot(WRF_2015_03_29_AQUA)
# 
# 
# 
# WRF_2015_03_30_AQUA <- (raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = 38) +
#                            raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = 39))/2
# 
# plot(WRF_2015_03_30_AQUA)
# 
# 
# 
# WRF_2015_03_31_AQUA <- (raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = 62) +
#                            raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = 63))/2
# 
# plot(WRF_2015_03_31_AQUA)
# 
# 
# 
# 
# WRF_2015_04_01_AQUA <- (raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = 86) +
#                            raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = 87))/2
# 
# plot(WRF_2015_04_01_AQUA)
# 
# 
# 
# 
# WRF_2015_04_02_AQUA <- (raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = 110) +
#                            raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = 111))/2
# 
# plot(WRF_2015_04_02_AQUA)
# 
# 
# 
# WRF_2015_04_03_AQUA <- (raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = 134) +
#                            raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = 135))/2
# 
# plot(WRF_2015_04_03_AQUA)


# make a stack
# all_rasters_WRF <- stack(WRF_2015_03_29_TERRA,
#                          WRF_2015_03_30_TERRA,
#                          WRF_2015_03_31_TERRA,
#                          WRF_2015_04_01_TERRA,
#                          WRF_2015_04_02_TERRA,
#                          WRF_2015_04_03_TERRA,
#                          WRF_2015_03_29_AQUA,
#                          WRF_2015_03_30_AQUA,
#                          WRF_2015_03_31_AQUA,
#                          WRF_2015_04_01_AQUA,
#                          WRF_2015_04_02_AQUA,
#                          WRF_2015_04_03_AQUA)
# 
# # stack with 1, 2, 3 April 2015
# stack_rasters_WRF <- stack(WRF_2015_04_01_AQUA,
#                          WRF_2015_04_02_AQUA,
#                          WRF_2015_04_03_AQUA)



######################################################################################################
######################################################################################################

# load MAIAC data 

# TERRA ## 
# read stack raster MAIAC TERRA
dir <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/TERRA/"
stack_MAIAC_TERRA <- stack(paste0(dir,"TERRA_MAIAC_DUST_event_02_April_2015_1km.tif"))
# subset layers as for MAIAC (1,2,3 APRIL)
stack_MAIAC_TERRA <- stack(stack_MAIAC_TERRA@layers[4:6])
plot(stack_MAIAC_TERRA)


# change resolution of WRF_chem rasters
WRF_AOD_stack <- projectRaster(WRF_AOD_stack, stack_MAIAC_TERRA)
WRF_AOD_stack <- stack(WRF_AOD_stack)
plot(WRF_AOD_stack)

####################################################################
### DIFFERENCE and CORRELATION between raster MODIS-MAIAC AQUA #####
####################################################################

all_rasters_TERRA <- stack()    # inizialise the raster stack

# generate a time sequence of 3 days
start <- as.POSIXct("2015-04-01 10:30")  # MODIS TERRA
interval <- 60*12 #minutes (1 day interval)
end <- start + as.difftime(2, units="days")  # 6 days
TS <- seq(from=start, by=interval*60*2, to=end)

# i <- 2

for (i in 1:length(stack_MAIAC_TERRA@layers)) {
  name_time <- TS[i]
  # difference
  diff_MAIAC_WRF <- stack_MAIAC_TERRA[[i]] - WRF_AOD_stack[[i]]    # 10:30 am
  plot(diff_MAIAC_WRF)
  all_rasters_TERRA <- stack(all_rasters_TERRA, diff_MAIAC_WRF)
  dir <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/TERRA/"
  writeRaster(all_rasters_TERRA, paste0(dir,"diff_TERRA_MAIAC_WRF_DUST_event_02_April_2015_1km.tif") , options= "INTERLEAVE=BAND", overwrite=T)
  
  print(i)
  
}


######################################################################################
### plot WRF images ##################################################################
######################################################################################

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

# gerate a time sequence of 2 days
start <- as.POSIXct("2015-04-01 10:30")  # MODIS TERRA
interval <- 60*12 #minutes (1 day interval)
end <- start + as.difftime(2, units="days")  # 6 days
TS <- seq(from=start, by=interval*60*2, to=end)

output_folder_TERRA <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/TERRA/plots_1km/"

####### color pallet

vec_all <- as.vector(WRF_AOD_stack)

max_val<- (max(vec_all, na.rm = T))
min_val<- (min(vec_all,  na.rm = T))

stat_dat <- summary(as.vector(WRF_AOD_stack))
IQR <- (as.numeric((stat_dat[5]-stat_dat[2])* 2))# n is the space after IQR

low_IQR<- if(floor(min_val) > floor(as.numeric((stat_dat[2]- IQR)))) floor(min_val) else floor(as.numeric((stat_dat[2]- IQR)))
high_IQR <-if ( max_val > (as.numeric((stat_dat[5]+IQR)))) max_val else (as.numeric((stat_dat[5]+IQR)))


# cool = rainbow(25, start=rgb2hsv(col2rgb('cyan'))[1], end=rgb2hsv(col2rgb('blue'))[1])
cool = rainbow(50, start=rgb2hsv(col2rgb('green'))[1], end=rgb2hsv(col2rgb('blue'))[1])
cool_2 = rainbow(25, start=rgb2hsv(col2rgb('yellow'))[1], end=rgb2hsv(col2rgb('green'))[1])
warm = rainbow(125, start=rgb2hsv(col2rgb('red'))[1], end=rgb2hsv(col2rgb('yellow'))[1])
cols = c(rev(cool), rev(cool_2), rev(warm))

# i <- 2


for (i in 1:length(WRF_AOD_stack@layers)) {
  TITLE <- paste(TS[i], " (UTC)")
  name_time <- TS[i]
  AOD_images <- WRF_AOD_stack[[i]]
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
  
  png(paste0(output_folder_TERRA ,"WRF_Chem_",str_sub(name_time, start = 1, end = -10), "_",
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
### difference MAIAC-WRF chem ##########################################################

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

# gerate a time sequence of 2 days
start <- as.POSIXct("2015-04-01 10:30")  # MODIS TERRA
interval <- 60*12 #minutes (1 day interval)
end <- start + as.difftime(2, units="days")  # 6 days
TS <- seq(from=start, by=interval*60*2, to=end)

setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/TERRA")

output_folder_TERRA <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/TERRA/plots_1km/"

# TERRA
MAIAC_STACK_image <- stack("diff_TERRA_MAIAC_WRF_DUST_event_02_April_2015_1km.tif")


####### color pallet

low_IQR <- -3
high_IQR <- 3

########################
### plots of maps ######
########################

# TERRA time
raster_MAIAC <- stack("diff_TERRA_MAIAC_WRF_DUST_event_02_April_2015_1km.tif")

for (i in 1:length(raster_MAIAC@layers)) {
  name_time <- TS[i]
  TERRA_WRF_images <- raster("diff_TERRA_MAIAC_WRF_DUST_event_02_April_2015_1km.tif", band = i)
  plot(TERRA_WRF_images)
  
  h <- rasterVis::levelplot(TERRA_WRF_images, 
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
  
  png(paste0(output_folder_TERRA ,"diff_MAIAC_WRF_",str_sub(name_time, start = 1, end = -10), "_",
             #  png(paste0(output_folder_TERRA ,str_sub(name_time, start = 1, end = -10), "_",
             str_sub(name_time, start = 12, end = -7), "_",
             str_sub(name_time, start = 15, end = -4),
             ".png"), width = 900, height = 900,
      units = "px", pointsize = 50,
      bg = "white", res = 200)
  print(h)
  dev.off()
  
}


###################################################################################
###################################################################################
#### OLD STUFF ####################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################


# load MODIS-MAIAC data  (6 rasters for the same time range of WRF-CHEM) March 29, to April 3 2015

# from MAIAC-MODIS (1km)
# setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/AVG_TERRA_MAIAC/stack_raster")
setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/AQUA")
all_rasters_MODIS_AQUA <- stack("AQUA_MAIAC_DUST_event_02_April_2015.tif")

setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/TERRA")
all_rasters_MODIS_TERRA <- stack("TERRA_MAIAC_DUST_event_02_April_2015.tif")

# make MODIS TERRA at the same resolution of MODIS AQUA
all_rasters_MODIS_TERRA <- projectRaster(all_rasters_MODIS_TERRA, all_rasters_MODIS_AQUA[[1]])


all_rasters_MODIS <- stack(all_rasters_MODIS_TERRA, 
                           all_rasters_MODIS_AQUA)

n <- length(all_rasters_MODIS@layers)  # 12 layers
n <- length(all_rasters_WRF@layers)  # 12 layers

res(all_rasters_MODIS)
res(all_rasters_WRF)

# make rasters WRF as same resolution and extent of raster MODIS
all_rasters_WRF = projectRaster(all_rasters_WRF, all_rasters_MODIS)
res(all_rasters_WRF)
extent(all_rasters_WRF)
extent(all_rasters_MODIS_TERRA)

# make a spatial correlation between all/or/each raster

### correlation 
# make a combine rasters of 6 + 6 = 12 layers
z <- stack(all_rasters_WRF,
           all_rasters_MODIS)
length(z@layers) 

res(z)


setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/")

# spatial correlations of all the layers (first 12 layes are from WRF, the second 12 layers are from MODIS)
# Pearson correlation between WRF-Chem and MODIS
r <- calc(z, fun=function(x) cor(x[1:12], x[13:24], method='pearson', use= "pairwise.complete.obs"))
plot(r)
# r <- calc(z, fun=function(x) cor(x[1:3], x[4:6], use= "pairwise.complete.obs"))

plot(r)
# overlay shape of UAE border
plot(shp_UAE, add=TRUE, lwd=1)

# make difference between WRF-Chem and WRF on 2 April 2015
diff_WRF_MODIS_TERRA <- all_rasters_WRF[[5]] - all_rasters_MODIS[[5]]    # 10:30 am
diff_WRF_MODIS_AQUA <- all_rasters_WRF[[11]] - all_rasters_MODIS[[11]]   # 13:30 pm


plot(diff_WRF_MODIS_TERRA)
plot(diff_WRF_MODIS_AQUA)


jpeg('Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/hist_MAIC_WRF.jpg',   
     quality = 100, bg = "white", res = 200, width = 5, height = 5, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

hist((r), breaks = 10)

par(oldpar)
dev.off()




jpeg('Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs//box_MAIC_WRF.jpg',   
     quality = 100, bg = "white", res = 200, width = 5, height = 5, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


boxplot(r)

par(oldpar)
dev.off()


getwd()
writeRaster(r, "Correlation_WRFCHem_MAIAC_02April2015_stack_6_DAYS_LARGE.tif" , options= "INTERLEAVE=BAND", overwrite=T)


#### make correlation using points from raster

# WRF <- values(all_rasters_WRF)
# MODIS<- values(all_rasters_MODIS)
# 
# data_cor<- data.frame()
# 
# for (i in 1:nrow(WRF)){
#   z <-cor(WRF[i,],MODIS[i,],   use= "pairwise.complete.obs")
#   data_cor<- rbind(data_cor,z)
#   
# }
# 
# 
# # get latitude and longitude from one sample raster--------------------
# sample <- rasterToPoints(all_rasters_MODIS[[1]])
# colnames(sample)[3] <- "value"
# sample<- cbind(sample[,-3],data_cor)
# rr<- rasterFromXYZ(sample)
# plot(rr)
# hist((rr),breaks= 100)
# plot(rr-r) ### this difference shoudl be 0
# 
# # overlay shape of UAE border
# plot(shp_UAE, add=TRUE, lwd=1)

###############################################################
# map spatial correlation map ##################################



setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs")
r <- raster("Correlation_WRFCHem_MAIAC_02April2015_stack_6_DAYS_LARGE.tif")
plot(r)

# r <- raster("Correlation_WRFCHem_MAIAC_02April2015_stack_6_DAYS_LARGE.tif")
# 
# # reduce the size of the raster
# extent(r)
# raster("MAIAC_DUST_event_02_April_2015.tif")
# extent(r)
# 
# # Load d01 WRF domain
# dir <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRFChem_domain"
# ### shapefile for UAE
# shp_d01 <- readOGR(dsn = dir, layer = "domain_d01_WRFChem")
# projection(shp_d01) <- CRS("+proj=longlat +datum=WGS84")
# 
# # crop 
# r <- crop(r, extent(shp_d01))
# r <- mask(r, shp_d01)
# 
# extent(r)



#### import the Arabian Peninsusula domain #############

dir_ME <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRFChem_domain"
### shapefile for WRF_domain
shp_ME <- readOGR(dsn = dir_ME, layer = "ADMIN_domain_d01_WRFChem")

shp_ME@data$name <- 1:nrow(shp_ME)
plot(shp_ME)

########################################################

MIN_PAL <- -1
MAX_PAL <- +1


pal_corr <- colorNumeric(c("#0000ff", "#ffffff", "#ff0000"),
                    c(MIN_PAL, MAX_PAL), na.color = "#ffff00")


min <- 0
max <- 1700

pal <- colorNumeric(c("#9999FF", "#FFFF00", "#FF0000", "#ff8000"),
                     c(min, max),na.color = "transparent")


map <- leaflet(shp_ME) %>%
#  setView(46, 26, 6) %>%
  addTiles(group = "OSM (default)") %>%
  addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
  
  addRasterImage(r, colors = pal_corr, opacity = 0.4,
                 group = "correlation", maxBytes = 32 * 1024 * 1024) %>%
  
  addRasterImage(diff_WRF_MODIS_TERRA, colors = pal, opacity = 0.7,
                 group = "diff_WRF_MODIS_TERRA", maxBytes = 32 * 1024 * 1024) %>%
  
  addRasterImage(diff_WRF_MODIS_AQUA, colors = pal, opacity = 0.7,
                 group = "diff_WRF_MODIS_AQUA", maxBytes = 32 * 1024 * 1024) %>%
  
  addPolygons(stroke = TRUE, smoothFactor = 1, fillOpacity = 0,
              weight = 2.5, color = "#000000",
              group = "ME") %>%
 
  # addLegend("bottomright", pal = pal_corr, values = c(MIN_PAL, MAX_PAL),
  #           title = "<br><strong>r MAIAC-WRFChem: </strong>",
  #           labFormat = labelFormat(prefix = ""),
  #           opacity = 0.4) %>%
  
  addLegend("bottomright", pal = pal, values = c(min, max),
            title = "<br><strong><font face=symbol>D</font>PM<sub>10</sub> (<font face=symbol>m</font>g/m<sup>3</sup>)</strong>",
            labFormat = labelFormat(prefix = ""),
            opacity = 0.7) %>%
  
  addLayersControl(
    baseGroups = c("Toner Lite", "Road map", "Satellite"),
    overlayGroups = c("correlation", "diff_WRF_MODIS_TERRA", "diff_WRF_MODIS_AQUA"),
    options = layersControlOptions(collapsed = TRUE)) %>%
  hideGroup(c("correlation", "diff_WRF_MODIS_AQUA"))  #"diff_WRF_MODIS_TERRA"

map


# saveWidget(map, paste0("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/correlation_WRF_MODIS_MAIAC.html"), selfcontained = FALSE)
# webshot("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/correlation_WRF_MODIS_MAIAC.html",
#         file = "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/correlation_WRF_MODIS_MAIAC.jpg", vwidth = 900, vheight = 900,
#         cliprect = 'viewport')


saveWidget(map, paste0("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/diff_WRF_TERRA_MAIAC.html"), selfcontained = FALSE)
webshot("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/diff_WRF_TERRA_MAIAC.html",
        file = "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/diff_WRF_TERRA_MAIAC.jpg", vwidth = 900, vheight = 900,
        cliprect = 'viewport')


saveWidget(map, paste0("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/diff_WRF_AQUA_MAIAC.html"), selfcontained = FALSE)
webshot("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/diff_WRF_AQUA_MAIAC.html",
        file = "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/diff_WRF_AQUA_MAIAC.jpg", vwidth = 900, vheight = 900,
        cliprect = 'viewport')

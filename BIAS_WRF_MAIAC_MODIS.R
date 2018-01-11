
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

# dir_ME <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRFChem_domain"
dir_ME <- "D:/Dust_Event_UAE_2015/WRFChem_domain"
### shapefile for WRF_domain
shp_ME <- readOGR(dsn = dir_ME, layer = "ADMIN_domain_d01_12km_WRFChem")
shp_ME <- spTransform(shp_ME, CRS("+init=epsg:4326"))

plot(shp_ME)


##############################################
# load MODIS MAIAC satellite data ############
##############################################

setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/AQUA")


output_folder_AQUA <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/plots_12km/"

# AQUA
MAIAC_STACK_image <- stack("AQUA_MAIAC_DUST_event_02_April_2015_12km.tif")

##############################################
# load WRF-Chem data #########################
##############################################

# 96 images #####
WRF_STACK_image <- stack("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_AOD_FK/extinction55/12km/AOD_12km_WRFChem_DUST1_Em3.tif")


###########################
# select WRF-Chem images ##
###########################

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


###########################
# select MAIAC images #####
###########################

MAIAC_AOD_09_00_April_1 <- raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/AQUA/AQUA_MAIAC_DUST_event_02_April_2015_12km.tif",
                                band = 4)
plot(MAIAC_AOD_09_00_April_1)

MAIAC_AOD_09_00_April_2 <- raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/AQUA/AQUA_MAIAC_DUST_event_02_April_2015_12km.tif",
                                  band = 5)
plot(MAIAC_AOD_09_00_April_2)

MAIAC_AOD_09_00_April_3 <- raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/AQUA/AQUA_MAIAC_DUST_event_02_April_2015_12km.tif",
                                  band = 6)
plot(MAIAC_AOD_09_00_April_3)

MAIAC_MODIS_AOD_stack <- stack(MAIAC_AOD_09_00_April_1,
                         MAIAC_AOD_09_00_April_2,
                         MAIAC_AOD_09_00_April_3)



#####################################################
# make the difference between 3 selected images #####
#####################################################

res(WRF_AOD_stack)
res(MAIAC_MODIS_AOD_stack)

# make rasters WRF as same resolution and extent of raster MODIS
MAIAC_MODIS_AOD_stack = projectRaster(MAIAC_MODIS_AOD_stack, WRF_AOD_stack)
res(MAIAC_MODIS_AOD_stack)
extent(MAIAC_MODIS_AOD_stack)
extent(WRF_AOD_stack)

# make a spatial correlation between all/or/each raster

### correlation 
# make a combine rasters of 3 + 3 = 6 layers
z <- stack(WRF_AOD_stack,
           MAIAC_MODIS_AOD_stack)
length(z@layers) 

res(z)


# spatial correlations of all the layers (first 3 layes are from WRF, the second 3 layers are from MAIAC)
# Pearson correlation between WRF-Chem and MAIAC
r <- calc(z, fun=function(x) cor(x[1:3], x[4:6], method='pearson', use= "pairwise.complete.obs"))
plot(r)
hist((r), breaks = 10)
# overlay shape of UAE border
plot(shp_ME, add=TRUE, lwd=1)
# save correlation raster
writeRaster(r, "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/AQUA/Correlation_WRFCHem_MAIAC_02April2015_stack_3_DAYS.tif" , options= "INTERLEAVE=BAND", overwrite=T)

# make difference between WRF-Chem and WRF on 2 April 2015
diff_MAIAC_WRF_1_April <- MAIAC_MODIS_AOD_stack[[1]] - WRF_AOD_stack[[1]]    
diff_MAIAC_WRF_2_April <- MAIAC_MODIS_AOD_stack[[2]] - WRF_AOD_stack[[2]]   
diff_MAIAC_WRF_3_April <- MAIAC_MODIS_AOD_stack[[3]] - WRF_AOD_stack[[3]]


plot(diff_MAIAC_WRF_1_April)
plot(diff_MAIAC_WRF_2_April)
plot(diff_MAIAC_WRF_3_April)

# stack differences maps

diff_WRF_MAIAC <- stack(diff_MAIAC_WRF_1_April,
                        diff_MAIAC_WRF_2_April,
                        diff_MAIAC_WRF_3_April)

writeRaster(diff_WRF_MAIAC, "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/AQUA/Diff_MAIAC_WRF.tif" , 
            options= "INTERLEAVE=BAND", overwrite=T)

##############################################
##############################################
##### make plots #############################
##############################################

# for the correlation
vec_all <- as.vector(r)

max_val <- 1
min_val <- -1


stat_dat <- summary(as.vector(r))
IQR <- (as.numeric((stat_dat[5]-stat_dat[2])))# n is the space after IQR

low_IQR<- if(floor(min_val) > floor(as.numeric((stat_dat[2]- IQR)))) floor(min_val) else floor(as.numeric((stat_dat[2]- IQR)))
high_IQR <- 1

cool = rainbow(50, start=rgb2hsv(col2rgb('green'))[1], end=rgb2hsv(col2rgb('blue'))[1])
cool_2 = rainbow(25, start=rgb2hsv(col2rgb('yellow'))[1], end=rgb2hsv(col2rgb('green'))[1])
warm = rainbow(125, start=rgb2hsv(col2rgb('red'))[1], end=rgb2hsv(col2rgb('yellow'))[1])
cols = c(rev(cool), rev(cool_2), rev(warm))


  h <- rasterVis::levelplot(r,
                            margin=FALSE, main="",
                            xlab = "",
                            ylab = "",
                            ## about colorbar
                            colorkey=list(
                              space='right',                   
                              labels= list(at= floor(as.numeric( seq(low_IQR, high_IQR, length.out=7))),
                                           font=4),
                              axis.line=list(col='black'),
                              width=1,
                              title=expression(paste("      R") )
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

  output_folder_AQUA <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/AQUA/plots_12km/"
  
  png(paste0(output_folder_AQUA ,"correlation_MAIAC_WRF.png"), width = 900, height = 900,
      units = "px", pointsize = 50,
      bg = "white", res = 200)
  print(h)
  dev.off()
  
  
##############################################  
# difference Maps ############################
  
  
  
  # for the correlation
  vec_all <- as.vector(diff_WRF_MAIAC)

  max_val<- (max(vec_all, na.rm = T))
  # max_val <- 4
  min_val<- (min(vec_all,  na.rm = T))
  # min_val <- 0
  
  
  stat_dat <- summary(as.vector(diff_WRF_MAIAC))
  IQR <- (as.numeric((stat_dat[5]-stat_dat[2])))# n is the space after IQR
  
  low_IQR<- if(floor(min_val) > floor(as.numeric((stat_dat[2]- IQR)))) floor(min_val) else floor(as.numeric((stat_dat[2]- IQR)))
  high_IQR <-if (max_val > (as.numeric((stat_dat[5]+IQR)))) max_val else (as.numeric((stat_dat[5]+IQR)))
  
  cool = rainbow(50, start=rgb2hsv(col2rgb('green'))[1], end=rgb2hsv(col2rgb('blue'))[1])
  cool_2 = rainbow(25, start=rgb2hsv(col2rgb('yellow'))[1], end=rgb2hsv(col2rgb('green'))[1])
  warm = rainbow(125, start=rgb2hsv(col2rgb('red'))[1], end=rgb2hsv(col2rgb('yellow'))[1])
  cols = c(rev(cool), rev(cool_2), rev(warm))
  
  
  # gerate a time sequence of 3 days
  start <- as.POSIXct("2015-04-01 09:00") 
  interval <- 60*12 #minutes (1 day interval)
  end <- start + as.difftime(2, units="days")  # 6 days
  TS <- seq(from=start, by=interval*60*2, to=end)
  
  
  i <- 2
  
  for (i in 1:length(diff_WRF_MAIAC@layers)) {
    name_time <- TS[i]
    diff_images <- raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/AQUA/Diff_MAIAC_WRF.tif", band = i)
    plot(diff_images)
    
    h <- rasterVis::levelplot(diff_images, 
                              margin=FALSE, main= as.character(name_time),
                              xlab = "",
                              ylab = "",
                              ## about colorbar
                              colorkey=list(
                                space='right',                   
                                labels= list(at= floor(as.numeric( seq(low_IQR, high_IQR, length.out=7))),
                                             font=3),
                                axis.line=list(col='black'),
                                width=1,
                                title=expression(paste("    diff.AOD") )
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
    
    output_folder_AQUA <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/AQUA/plots_12km/"
    
    png(paste0(output_folder_AQUA ,str_sub(name_time, start = 1, end = -10), "_",
               str_sub(name_time, start = 12, end = -7), "_",
               str_sub(name_time, start = 15, end = -4),
               "_MAIAC_WRF_Chem_diff.png"), width = 900, height = 900,
        units = "px", pointsize = 50,
        bg = "white", res = 200)
    print(h)
    dev.off()
    
  }


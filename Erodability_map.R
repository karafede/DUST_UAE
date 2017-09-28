
library(RNetCDF)
library(rgdal)
library(ncdf4)
library(raster)
library(stringr)


# .nc file
setwd("D:/Dust_Event_UAE_2015/WRF_trial_runs/DUST_AOD_FK/erodability")


patt<- ".nc"
filenames <- list.files(pattern = patt)


#############################################
####import .nc files ########################
#############################################

WRF_file <- open.nc(filenames)
WRF_file <- read.nc(WRF_file)
name_vari<- names(WRF_file)

var_value <- (WRF_file[129])    #  only one variable (EROD) == var = 129; Erodability map
names(var_value)<- "xxyyzz"
var_value <- (var_value$xxyyzz)
LON <- WRF_file$XLONG
LAT <- WRF_file$XLAT
     
     xmn= min(LON)
     xmx=max(LON)
     ymn=min(LAT)
     ymx=max(LAT)
     
     # j = 5
     
    MMM <-  t(var_value[ , ,1,3])   # map is upside down  (only consider surface level)
    MMM <- MMM[nrow(MMM):1, ]
    r <- raster(MMM, xmn, xmx, ymn,  ymx, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    plot(r)
    
    crs(r)

 writeRaster(r, "ERODABILITY_Ginoux.tif" , options= "INTERLEAVE=BAND", overwrite=T)


#########################################################################################
### plot maps ##########################################################################

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
shp_ME <- readOGR(dsn = dir_ME, layer = "ADMIN_domain_d01_12km_WRFChem")
crs(shp_ME)
extent(shp_ME)

shp_ME <- spTransform(shp_ME, CRS("+init=epsg:4326"))
plot(shp_ME)

# set directory where we want to save the images
setwd("D:/Dust_Event_UAE_2015/WRF_trial_runs/DUST_AOD_FK/erodability")


# i = 4

# load raster stack ---------------------------------

EROD_map <- raster("D:/Dust_Event_UAE_2015/WRF_trial_runs/DUST_AOD_FK/erodability/ERODABILITY_Ginoux.tif")
res(EROD_map)
extent(EROD_map)
# reproject shp file
# shp_ME <- spTransform(shp_ME, crs(EROD_map))
# load a regular raster
r <- stack("D:/Dust_Event_UAE_2015/WRF_trial_runs/DUST_AOD_FK/extinction55/12km/AOD_12km_WRFChem_DUST1_Em3.tif")
res(r)
crs(r)

EROD_map <- projectRaster(EROD_map, r)
crs(EROD_map)

output_folder <- "D:/Dust_Event_UAE_2015/WRF_trial_runs/DUST_AOD_FK/erodability/"

####### color pallet

# recalibrate the model output (--> AOD*6.25)
# WRF_STACK_image <- WRF_STACK_image*6.25

vec_all <- as.vector(EROD_map)

min_val<- (min(vec_all,  na.rm = T))
min_val <- 0
max_val<- (max(vec_all, na.rm = T))
max_val <- 1



stat_dat <- summary(as.vector(EROD_map))
IQR <- (as.numeric((stat_dat[5]-stat_dat[2])* 2))# n is the space after IQR

low_IQR<- if(floor(min_val) > floor(as.numeric((stat_dat[2]- IQR)))) floor(min_val) else floor(as.numeric((stat_dat[2]- IQR)))
high_IQR <-if ( max_val > (as.numeric((stat_dat[5]+IQR)))) max_val else (as.numeric((stat_dat[5]+IQR)))

# cool = rainbow(25, start=rgb2hsv(col2rgb('cyan'))[1], end=rgb2hsv(col2rgb('blue'))[1])
cool = rainbow(50, start=rgb2hsv(col2rgb('green'))[1], end=rgb2hsv(col2rgb('royalblue2'))[1])
cool_2 = rainbow(25, start=rgb2hsv(col2rgb('yellow'))[1], end=rgb2hsv(col2rgb('green'))[1])
warm = rainbow(125, start=rgb2hsv(col2rgb('red'))[1], end=rgb2hsv(col2rgb('yellow'))[1])
cols = c(rev(cool), rev(cool_2), rev(warm))

# cool = rainbow(50, start=rgb2hsv(col2rgb('cyan'))[1], end=rgb2hsv(col2rgb('royalblue2'))[1])
# cool_2 = rainbow(25, start=rgb2hsv(col2rgb('yellow'))[1], end=rgb2hsv(col2rgb('cyan'))[1])
# warm = rainbow(125, start=rgb2hsv(col2rgb('red'))[1], end=rgb2hsv(col2rgb('yellow'))[1])
# cols = c(rev(cool), rev(cool_2), rev(warm))



###################################
### plots of Erodibility map ######
###################################

EROD_map <- raster("D:/Dust_Event_UAE_2015/WRF_trial_runs/DUST_AOD_FK/erodability/ERODABILITY_Ginoux.tif")
plot(EROD_map)
EROD_map <- projectRaster(EROD_map, r)
plot(EROD_map)

  h <- rasterVis::levelplot(EROD_map, 
                            margin=FALSE, main= "Erodibility Map",
                            xlab = "",
                            ylab = "",
                            ## about colorbar
                            colorkey=list(
                              space='bottom',                   
                              labels= list(at= floor(as.numeric( seq(low_IQR, high_IQR, length.out=7))),
                                           font=3),
                              axis.line=list(col='black'),
                              width=2
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
                            names.attr=rep(names(EROD_map)))  +
     latticeExtra::layer(sp.polygons(shp_ME))
  h
  
  png(paste0(output_folder, "Erodibiltiy_UAE.png"), width = 900, height = 900,
      units = "px", pointsize = 50,
      bg = "white", res = 200)
  print(h)
  dev.off()
  
  

  




# to make a movie.......
# to use with ImageMagik using the commnad line cmd in windows
# cd into the directory where there are the png files
# magick -delay 50 -loop 0 *.png WRF_Chem_DUST_event_02_April_2015.gif




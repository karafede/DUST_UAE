
library(RNetCDF)
library(rgdal)
library(ncdf4)
library(raster)
library(stringr)
library(leaflet)


# list .nc files

setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs")

patt<- ".nc"
filenames <- list.files(pattern = patt)
# load WRFChem output from Mike (only dust concentration simulation for the 2nd April 2015 dust storm)
filenames <- "dust_d01_ug_lev1.nc"  # small domain
filenames <- "wrfchem_d01_pm10_20150329_20150404_lev1.nc"  # large domain

# gerate a time sequence for the WRF-Chem run at intervals of 1 hour (should be 145 images), 6 days
start <- as.POSIXct("2015-03-29 00:00")
interval <- 60 #minutes
end <- start + as.difftime(6, units="days")
TS <- seq(from=start, by=interval*60, to=end)
TS <- TS[1:144]
name <- str_sub(filenames, start = 1, end = -4)


# inizialise an empty raster to stack ALL HOURS together in an unique raster 

 import_nc_WRF <- function(filenames){

  WRF_file <- open.nc(filenames)
  WRF_file <- read.nc(WRF_file)
  name_vari <- names(WRF_file)
  
  
#### only one variable (dust_e) == var = 5
  
  # for(j in 156:156) {
     var_value<-(WRF_file[5])
     names(var_value) <- "xxyyzz"
     var_value <- (var_value$xxyyzz)
     LON <-WRF_file$lon
     LAT <-WRF_file$lat
     
     xmn = min(LON)
     xmx = max(LON)
     ymn = min(LAT)
     ymx = max(LAT)
     
  all_rasters <- stack()    # stack ALL 73 HOURS together in an unique raster
     
     # i = 5
  
  # SEVIRI_STACK_image <- raster("D:/Dust_Event_UAE_2015/SEVIRI_20150402_outputs/I_method/Seviri_20150402_I_Method_stack.tif", band = 3)
  # plot(SEVIRI_STACK_image)
  # r <- raster((var_value[ , , i]), xmn, xmx, ymn,  ymx, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  # r <- projectRaster(r, SEVIRI_STACK_image)
  # plot(r)
  
  for (i in 1:dim(var_value)[3]){      # time dimension (always 145)
    MMM <-  t(var_value[ , , i])    # map is upside down 
    MMM <- MMM[nrow(MMM):1, ]
    # r <- raster((var_value[ , , i]), xmn, xmx, ymn,  ymx, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    r <- raster(MMM, xmn, xmx, ymn,  ymx, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    plot(r)
    name_time <- TS[i]
    names(r)<- paste("WRF_Chem_", name_time, sep = "")
    all_rasters<- stack(all_rasters,r)
  }
 
   
#   }
  
  write.csv(names(all_rasters), paste(name,  ".csv", sep=""))
  return(all_rasters)
}

 
 
 BBB <- lapply(filenames, import_nc_WRF) 
 
 
 # make a large stack raster with the 19*3=57 layeer for DUST_1
 ras_stack <- stack()
 
# kk <- 50
 
 # for (jj in 1:19 ){         # number of .nc files
 for (kk in 1:145){          # number of hours (time stamp) 6 days, 145 hours
   # plot(BBB[[jj]],kk)
   plot(BBB[[1]],kk)
   # ras <- raster(BBB[[jj]], kk)
   ras <- raster(BBB[[1]], kk)
   ras_stack<- stack(ras_stack,ras)
 }
 #}
 
 
 
 # AAA <- ras_stack[[70]]
 # plot(AAA) 
 
 
writeRaster(ras_stack, "DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif" , options= "INTERLEAVE=BAND", overwrite=T)

#### from MIKE WESTON.....add a stamp to geotiff
# Great! Worked perfectly.
# To plot time stamp I did
# par(col.axis="white",col.lab="white",tck=0) # makes the axes white in next step
# plot(my_stack,axes=TRUE,main=heading,legend=FALSE) # Use main to put timestamp. Axes are not visible.


#########################################################################################

library(leaflet)
library(webshot)
library(htmlwidgets)
library(RColorBrewer)
library(raster)
library(classInt)
library(stringr)
library(ggplot2)

# set directory where we want to save the images
setwd("D:/Dust_Event_UAE_2015/WRF_trial_runs/images_png")
setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/images_png")

# save images as webshot from leaflet
# reload rasters by band or layers (145 scenes)

# gerate a time sequence for a given day every 60 minuntes (should be 73 images)
start <- as.POSIXct("2015-03-29 00:00")
interval <- 60 #minutes
end <- start + as.difftime(6, units="days")
TS <- seq(from=start, by=interval*60, to=end)
TS <- TS[1:144]

# i = 4

# load raster stack ---------------------------------

WRF_STACK_image <- stack("D:/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif")
WRF_STACK_image <- stack("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif")


# WRF_STACK_image[WRF_STACK_image < 0] <- 0

vec_all <- as.vector(WRF_STACK_image)

max_val<- ceiling(max(vec_all, na.rm = T))
min_val<- floor(min(vec_all,  na.rm = T))

# for the Irradiance-----------

stat_dat <- summary(vec_all)
IQR <- floor(as.numeric((stat_dat[5]-stat_dat[2])* 4.5))# n is the space after IQR

low_IQR <-floor(as.numeric((stat_dat[2]- IQR)))
high_IQR <-floor(as.numeric((stat_dat[5]+IQR)))


vec_all_1 <- vec_all[ vec_all >= low_IQR & vec_all <= high_IQR & !is.na(vec_all) ]

xxx<- pretty( vec_all_1, n=10)

{
  if (max_val <= max(xxx)){
    xxx<- unique(c( xxx))
  }else{
    xxx<- unique(c( xxx, max_val))
  }
  
  if (min_val >= min(xxx)){
    xxx<- unique(c( xxx))
  }else{
    xxx<- unique(c(min_val, xxx))
  }
}




cool = rainbow(50, start=rgb2hsv(col2rgb('cyan'))[1], end=rgb2hsv(col2rgb('blue'))[1])
warm = rainbow(50, start=rgb2hsv(col2rgb('red'))[1], end=rgb2hsv(col2rgb('yellow'))[1])
cols = c(rev(cool), rev(warm))
mypalette <- colorRampPalette(cols)(255)

pal = colorBin(mypalette, bin = xxx, domain = min_val:max_val, na.color = "transparent")

# i <- 5

for (i in 1:length(WRF_STACK_image@layers)) {
  # load the stacked raster with all the 73 images
  WRF_STACK_image <- raster("D:/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = i)
#  WRF_STACK_image <- raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = i)
  plot(WRF_STACK_image)
  
  name_time <- TS[i]

  
  # pal_WRF <- colorNumeric(c("#b7b7ff", "#ffd699", "#FFFF00", "#ffbf00", "#ffc700", "#FF0000", "#994c00"),  #"#9999FF"
  #                         c(min_val, max_val), na.color = "transparent")
  
  # define popup for time scene
  "h1 { font-size: 3px;}"
  content <- paste('<h1><strong>', name_time,'', sep = "")
  
  map <- leaflet() %>% 
    addTiles() %>% 
    addTiles(group = "OSM (default)") %>%
    addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
    addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
    addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
    
    addPopups(37, 35, content,
              options = popupOptions(closeButton = FALSE)) %>%
    
    addRasterImage(WRF_STACK_image, 
                   colors = pal, 
                   opacity = 0.5, group = "WRF_CHEM") %>%
    addLayersControl(
      baseGroups = c("Toner Lite", "Road map", "Satellite"),
      overlayGroups = "WRF_CHEM",
      options = layersControlOptions(collapsed = TRUE)) %>%
    addLegend("bottomright", pal = pal, values = c(min_val, max_val),
              title = "<br><strong>PM<sub>10</sub> (<font face=symbol>m</font>g/m<sup>3</sup>) DUST </strong>",
              labFormat = labelFormat(prefix = ""),
              opacity = 1)
  
  ## This is the png creation part
  saveWidget(map, 'temp.html', selfcontained = FALSE)
  webshot('temp.html', file = paste0(str_sub(name_time, start = 1, end = -10), "_",
                                     str_sub(name_time, start = 12, end = -7), "_",
                                     str_sub(name_time, start = 15, end = -4),
                                     ".png"), vwidth = 900, vheight = 900,
          cliprect = 'viewport')
  
}

# to make a movie.......
# to use with ImageMagik using the commnad line cmd in windows
# cd into the directory where there are the png files
# magick -delay 50 -loop 0 *.png WRF_Chem_DUST_event_02_April_2015.gif

###################################################################################################################
#### crop all WRF images with the same extetn of the MAIAC domain #################################################
###################################################################################################################

# just crop the entire raster stack....

# WRF_STACK_image <- stack("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif")
# res(WRF_STACK_image)
# extent(WRF_STACK_image)
# 
# 
# 
# #### importing the UAE shapefile to use as a masking 
# dir <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/website_MODIS/UAE_boundary"
# ### shapefile for UAE
# shp_UAE <- readOGR(dsn = dir, layer = "uae_emirates")
# 
# # ----- Transform to EPSG 4326 - WGS84 (required)
# shp_UAE <- spTransform(shp_UAE, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
# # names(shp)
# crs <- projection(shp_UAE) ### get projections from shp file
# 
# # load MAIAC extent-------------------------------------------
# 
# dir <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/extent_MAIAC_UAE"
# ### shapefile for UAE
# shp_MAIAC <- readOGR(dsn = dir, layer = "extent_MAIAC")
# 
# 
# projection(shp_MAIAC) <- CRS("+proj=longlat +datum=WGS84")
# # ----- Transform to EPSG 4326 - WGS84 (required)
# shp_MAIAC <- spTransform(shp_MAIAC, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
# plot(shp_MAIAC)
# 
# 
# # crop the entire stack with the MAIAC extent
# WRF_STACK_image <- crop(WRF_STACK_image, extent(shp_MAIAC))
# WRF_STACK_image <- mask(WRF_STACK_image, shp_MAIAC)
# 
# res(WRF_STACK_image)
# extent(WRF_STACK_image)
# 
# writeRaster(WRF_STACK_image, "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_MAIAC_ext.tif",
#             options= "INTERLEAVE=BAND", overwrite=T)

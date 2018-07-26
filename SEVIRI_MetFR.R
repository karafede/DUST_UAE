
library(RNetCDF)
library(rgdal)
library(ncdf4)
library(raster)
library(stringr)

memory.limit(size = 9000)

# list .nc files
setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/SEVIRI_20150402_outputs/II_method")

# MODIS MAIAC reference
ref <- raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/AQUA/92_Aqua_MAIAC_crop.tif")
# plot(ref)


patt <- ".nc"
filenames <- list.files(pattern = patt)
# change filename 
# filenames <- filenames[1] # 2015-03-29
# filenames <- filenames[2] # 2015-03-30
# filenames <- filenames[3] # 2015-03-31
filenames <- filenames[4] # 2015-04-01
# filenames <- filenames[5] # 2015-04-02
# filenames <- filenames[6] # 2015-04-03
# filenames <- filenames[7] # 2015-04-04

# gerate a time sequence for a given day every 15 minuntes

# start <- as.POSIXct("2015-03-29")
# start <- as.POSIXct("2015-03-30")
# start <- as.POSIXct("2015-03-31")
start <- as.POSIXct("2015-04-01")
# start <- as.POSIXct("2015-04-02")
# start <- as.POSIXct("2015-04-03")
# start <- as.POSIXct("2015-04-04")

interval <- 15 #minutes
end <- start + as.difftime(1, units="days")
TS <- seq(from=start, by=interval*60, to=end-1)
 TS <- TS[1:84]
# TS <- TS[26:31] # TERRA time
name <- str_sub(filenames, start = 1, end = -4)


# inizialise an empty raster to stack ALL HOURS together in an unique raster 

 import_nc_seviri <- function(filenames){

## !we are importing HDF5 files....different structure!!! ----------------------
  seviri_file <- nc_open(filenames)
  name_vari <- names(seviri_file$var)
  name <- str_sub(filenames, start = 1, end = -4)
  
  
###### read the DUST FLAG variable (only)

     var_value <- ncvar_get(seviri_file)
     LON <- ncvar_get(seviri_file,"lon")
     LAT <- ncvar_get(seviri_file,"lat")
     
     xmn= min(LON)
     xmx=max(LON)
     ymn=min(LAT)
     ymx=max(LAT)
     
  all_rasters <- stack()    # stack ALL HOURS together in an unique raster
     
     # i = 56
   
  for (i in 1:dim(var_value)[3]){      # time dimension (always 96 scenes over one day)
#    for (i in 26:31){ 
    r <- raster((var_value[ , , i]), xmn, xmx, ymn,  ymx, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    plot(r)
    # reproject with the reference (MODIS MAIAC 1km)
    # r = projectRaster(r, ref)
    name_time <- TS[i]
    names(r)<- paste("SEVIRI_", name_time, sep = "")
    all_rasters<- stack(all_rasters,r)
  }
   write.csv(names(all_rasters), paste(name,  ".csv", sep=""))
  return(all_rasters)
}

 
 BBB <- lapply(filenames, import_nc_seviri) 
 
 # make a large stack raster with all the masks
 ras_stack<- stack()

# kk <- 50
 
 # for (jj in 1:19 ){         # number of .nc files
  for (kk in 1:84){          # number of scenes (time stamp)
 #    for (kk in 26:31){          # number of scenes (time stamp)
 # plot(BBB[[jj]],kk)
     plot(BBB[[1]],kk)
 # ras <- raster(BBB[[jj]], kk)
     ras <- raster(BBB[[1]], kk)
     ras_stack<- stack(ras_stack,ras)
   }
 #}
 

writeRaster(ras_stack, paste(name, "stack.tif", sep = "_") , options= "INTERLEAVE=BAND", overwrite=T)


#########################################################################################
#########################################################################################

## Maps ###

library(leaflet)
library(webshot)
library(htmlwidgets)
library(RColorBrewer)
library(raster)
library(classInt)
library(stringr)
library(rgdal)

library(viridis)
library(lattice)

#### import the Arabian Peninsusula domain #############

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
# dir <- "D:/Dust_Event_UAE_2015/WRFChem_domain"

# larger WRF domain
shp_WRF <- readOGR(dsn = dir, layer = "domain_d01_12km_WRFChem")
plot(shp_WRF)

plot(shp_WRF)
plot(shp_UAE, add=TRUE, lwd=1)
plot(shp_WRF, add=TRUE, lwd=1)

dir_ME <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRFChem_domain"
### shapefile for WRF_domain (ARABIAN PENINSULA)
shp_ME <- readOGR(dsn = dir_ME, layer = "ADMIN_domain_d01_12km_WRFChem")
plot(shp_ME)

# load reference for SEVIRI data (2km)
ref <- raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/SEVIRI_20150402_outputs/II_method/Seviri_20150402_METFr_Orig_stack.tif", band = 20)
ref <- crop(ref, extent(38.01822, 59.99447, 13, 34))
plot(ref)
plot(shp_ME, add = TRUE)

# crop shp_ME
shp_ME <- raster::crop(shp_ME, extent(38.01822, 59.99447, 13, 34))
plot(shp_ME)


# gerate a time sequence of 6 SEVIRI scenes
start <- as.POSIXct("2015-04-03")
interval <- 15 #minutes
end <- start + as.difftime(1, units="days")
TS <- seq(from=start, by=interval*60, to=end-1)
TS <- TS[1:84]
# TS <- TS[26:31] # TERRA time

setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/SEVIRI_20150402_outputs/II_method")

output_folder_METFRANCE <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/SEVIRI_20150402_outputs/II_method/plots_1km/"

# SEVIRI MASK @ TERRA time
# MASKS_STACK_image <- stack("Seviri_20150329_METFr_Orig_stack.tif")
# MASKS_STACK_image <- stack("Seviri_20150330_METFr_Orig_stack.tif")
# MASKS_STACK_image <- stack("Seviri_20150331_METFr_Orig_stack.tif")
# MASKS_STACK_image <- stack("Seviri_20150401_METFr_Orig_stack.tif")
#  MASKS_STACK_image <- stack("Seviri_20150402_METFr_Orig_stack.tif")
MASKS_STACK_image <- stack("Seviri_20150403_METFr_Orig_stack.tif")
# MASKS_STACK_image <- stack("Seviri_20150404_METFr_Orig_stack.tif")

####### color pallet

# max_val<- (max(vec_all, na.rm = T))
max_val <- 1
# min_val<- (min(vec_all,  na.rm = T))
min_val <- 0


low_IQR<- 0
high_IQR <- 1

cols <-  colorRampPalette(c("white", "red"))

# i <- 24

########################
### plots of maps ######
########################

# raster_MASKS <- stack("Seviri_20150329_METFr_Orig_stack.tif")
# raster_MASKS <- stack("Seviri_20150330_METFr_Orig_stack.tif")
# raster_MASKS <- stack("Seviri_20150331_METFr_Orig_stack.tif")
# raster_MASKS <- stack("Seviri_20150401_METFr_Orig_stack.tif")
# raster_MASKS <- stack("Seviri_20150402_METFr_Orig_stack.tif")
raster_MASKS <- stack("Seviri_20150403_METFr_Orig_stack.tif")
# raster_MASKS <- stack("Seviri_20150404_METFr_Orig_stack.tif")
 

for (i in 1:length(raster_MASKS@layers)) {
  name_time <- TS[i]
  # SEVIRI_MASKS <- raster("Seviri_20150329_METFr_Orig_stack.tif", band = i)
  # SEVIRI_MASKS <- raster("Seviri_20150330_METFr_Orig_stack.tif", band = i)
  # SEVIRI_MASKS <- raster("Seviri_20150331_METFr_Orig_stack.tif", band = i)
  # SEVIRI_MASKS <- raster("Seviri_20150401_METFr_Orig_stack.tif", band = i)
  # SEVIRI_MASKS <- raster("Seviri_20150402_METFr_Orig_stack.tif", band = i)
   SEVIRI_MASKS <- raster("Seviri_20150403_METFr_Orig_stack.tif", band = i)
  # SEVIRI_MASKS <- raster("Seviri_20150404_METFr_Orig_stack.tif", band = i)
  
  SEVIRI_MASKS <- crop(SEVIRI_MASKS, extent(shp_ME))
  # SEVIRI_MASKS <- mask(SEVIRI_MASKS, shp_WRF) # RECTANGULAR
  plot(SEVIRI_MASKS)
  
  h <- rasterVis::levelplot(SEVIRI_MASKS, 
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
  
  png(paste0(output_folder_METFRANCE ,"MASK_SEVIRI_TERRA_", str_sub(name_time, start = 1, end = -10), "_",
             str_sub(name_time, start = 12, end = -7), "_",
             str_sub(name_time, start = 15, end = -4),
             ".png"), width = 900, height = 900,
      units = "px", pointsize = 50,
      bg = "white", res = 200)
  print(h)
  dev.off()
  
}




#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
#### leafleft map #######################################################################
#########################################################################################
#########################################################################################

library(leaflet)
library(webshot)
library(htmlwidgets)
library(RColorBrewer)
library(raster)
library(classInt)
library(stringr)

# set directory where we want to save the images
setwd("D:/Dust_Event_UAE_2015/SEVIRI_20150402_outputs/II_method/images_png")
setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/SEVIRI_20150402_outputs/II_method/images_png")

# save images as webshot from leaflet
# reload rasters by band or layers (96 scenes)

# gerate a time sequence for a given day every 15 minuntes

# start <- as.POSIXct("2015-03-29")
# start <- as.POSIXct("2015-03-30")
# start <- as.POSIXct("2015-03-31")
# start <- as.POSIXct("2015-04-01")
# start <- as.POSIXct("2015-04-02")
# start <- as.POSIXct("2015-04-03")
start <- as.POSIXct("2015-04-04")

interval <- 15 #minutes
end <- start + as.difftime(1, units="days")
TS <- seq(from=start, by=interval*60, to=end-1)

# i = 3

for (i in 1:84) {

    # load the stacked raster with all the 96 images

# SEVIRI_STACK_image <- raster("D:/Dust_Event_UAE_2015/SEVIRI_20150402_outputs/II_method/Seviri_20150401_METFr_Orig_stack.tif", band = i)
# SEVIRI_STACK_image <- raster("D:/Dust_Event_UAE_2015/SEVIRI_20150402_outputs/II_method/Seviri_20150402_METFr_Orig_stack.tif", band = i)
#  SEVIRI_STACK_image <- raster("D:/Dust_Event_UAE_2015/SEVIRI_20150402_outputs/II_method/Seviri_20150403_METFr_Orig_stack.tif", band = i)
  SEVIRI_STACK_image <- raster("D:/Dust_Event_UAE_2015/SEVIRI_20150402_outputs/II_method/Seviri_20150404_METFr_Orig_stack.tif", band = i)
  
  # SEVIRI_STACK_image <- raster("D:/Dust_Event_UAE_2015/SEVIRI_20150402_outputs/II_method/Seviri_20150401_METFr_Orig_stack.tif", band = i)
  # SEVIRI_STACK_image <- raster("D:/Dust_Event_UAE_2015/SEVIRI_20150402_outputs/II_method/Seviri_20150402_METFr_Orig_stack.tif", band = i)
  #  SEVIRI_STACK_image <- raster("D:/Dust_Event_UAE_2015/SEVIRI_20150402_outputs/II_method/Seviri_20150403_METFr_Orig_stack.tif", band = i)
  SEVIRI_STACK_image <- raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/SEVIRI_20150402_outputs/II_method/Seviri_20150404_METFr_Orig_stack.tif", band = i)
  

plot(SEVIRI_STACK_image)

name_time <- TS[i]

min_seviri <- 0
max_seviri <- 1

pal_SEVIRI <- colorNumeric(c("#ffffff", "#ff0000"),
                        c(min_seviri, max_seviri), na.color = "transparent")

# define popup for time scene
"h2 { font-size: 3px;}"
content <- paste('<h2><strong>', name_time,'', sep = "")

map <- leaflet() %>% 
  addTiles() %>% 
  addTiles(group = "OSM (default)") %>%
  addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
  
  addPopups(32, 38, content,
            options = popupOptions(closeButton = FALSE)) %>%
  
  addRasterImage(SEVIRI_STACK_image, 
                 colors = pal_SEVIRI, 
                 opacity = 0.5, group = "SEVIRI") %>%
    addLayersControl(
    baseGroups = c("Road map", "Toner Lite","Satellite"),
    overlayGroups = "SEVIRI",
    options = layersControlOptions(collapsed = TRUE))


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

# magick -delay 100 -loop 0 *.png SEVIRI_DUST_event_02_April_2015_MetFr.gif
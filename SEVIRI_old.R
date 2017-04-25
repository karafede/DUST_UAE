
library(RNetCDF)
library(rgdal)
library(ncdf4)
library(raster)
library(stringr)


# list .nc files
#  setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/DUST SEVIRI/seviri_data_20150402/output_20150402_new")
setwd("D:/Dust_Event_UAE_2015/SEVIRI_20150402_outputs/I_method")

patt <- ".nc"
filenames <- list.files(pattern = patt)
filenames <- filenames[2]

# gerate a time sequence for a given day every 15 minuntes
start <- as.POSIXct("2015-04-02")
interval <- 15 #minutes
end <- start + as.difftime(1, units="days")
TS <- seq(from=start, by=interval*60, to=end-1)
name <- str_sub(filenames, start = 1, end = -4)


# inizialise an empty raster to stack ALL HOURS together in an unique raster 

 import_nc_seviri <- function(filenames){

  seviri_file <- open.nc(filenames)
  seviri_file <- read.nc(seviri_file)
  name_vari <- names(seviri_file)
  name <- str_sub(filenames, start = 1, end = -4)
  
  
###### read the DUST FLAG variable (only)

     var_value<-(seviri_file[1])
     names(var_value)<- "xxyyzz"
     var_value<- (var_value$xxyyzz)
     LON <- seviri_file$lon
     LAT <- seviri_file$lat
     
     xmn= min(LON)
     xmx=max(LON)
     ymn=min(LAT)
     ymx=max(LAT)
     
  all_rasters <- stack()    # stack ALL HOURS together in an unique raster
     
     # i = 56
   
  for (i in 1:dim(var_value)[3]){      # time dimension (always 96 scenes over one day)
    r <- raster((var_value[ , , i]), xmn, xmx, ymn,  ymx, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    plot(r)
    name_time <- TS[i]
    names(r)<- paste("SEVIRI_", name_time, sep = "")
    all_rasters<- stack(all_rasters,r)
   # names(all_rasters)<- c(name,name,name)
  }
   write.csv(names(all_rasters), paste(name,  ".csv", sep=""))
  return(all_rasters)
}

 
 BBB <- lapply(filenames, import_nc_seviri) 
 
 # make a large stack raster with the 19*3=57 layeer for DUST_1
 ras_stack<- stack()

 # for (jj in 1:19 ){         # number of .nc files
   for (kk in 1:96){          # number of scenes (time stamp)
 # plot(BBB[[jj]],kk)
     plot(BBB[[1]],kk)
 # ras <- raster(BBB[[jj]], kk)
     ras <- raster(BBB[[1]], kk)
     ras_stack<- stack(ras_stack,ras)
   }
 #}
 
 
 
 AAA <- ras_stack[[30]]
 plot(AAA) 
 
 
writeRaster(ras_stack, paste(name, "stack.tif", sep = "_") , options= "INTERLEAVE=BAND", overwrite=T)


#########################################################################################

library(leaflet)
library(webshot)
library(htmlwidgets)
library(RColorBrewer)
library(raster)
library(classInt)
library(stringr)

# set directory where we want to save the images
setwd("D:/Dust_Event_UAE_2015/SEVIRI_20150402_outputs/images_png")

# save images as webshot from leaflet
# reload rasters by band or layers (96 scenes)

# gerate a time sequence for a given day every 15 minuntes
start <- as.POSIXct("2015-04-02")
interval <- 15 #minutes
end <- start + as.difftime(1, units="days")
TS <- seq(from=start, by=interval*60, to=end-1)


for (i in 1:96) {
SEVIRI_STACK_image <- raster("D:/Dust_Event_UAE_2015/SEVIRI_20150402_outputs/Seviri_20150402_stack.tif", band = i)
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

# magick -delay 100 -loop 0 *.png SEVIRI_DUST_event_April_2015.gif
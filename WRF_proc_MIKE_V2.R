
library(RNetCDF)
library(rgdal)
library(ncdf4)
library(raster)
library(stringr)
library(leaflet)


# list .nc files
setwd("D:/Dust_Event_UAE_2015/WRF_trial_runs")
setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs")

patt<- ".nc"
filenames <- list.files(pattern = patt)
# load WRFChem output from Mike (only dust concentration simulation for the 2nd April 2015 dust storm)
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
  
  for (i in 1:dim(var_value)[3]){      # time dimension (always 145)
    MMM <-  t(var_value[ , , i])       # map is upside down 
    MMM <- MMM[nrow(MMM):1, ]
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
 
 
 # make a large stack raster 
 # initialize the empty raster
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

WRF_STACK_image <- stack("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif")


# WRF_STACK_image[WRF_STACK_image < 0] <- 0

vec_all <- as.vector(WRF_STACK_image)

max_val<- ceiling(max(vec_all, na.rm = T))
min_val<- floor(min(vec_all,  na.rm = T))

# this is to setup the colorbar-----------

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


# start plotting all the images

for (i in 1:length(WRF_STACK_image@layers)) {
  # load the stacked raster with all the 145 images
  WRF_STACK_image <- raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = i)
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
              title = "<br><strong>PM<sub>10</sub> (<font face=symbol>m</font>g/m<sup>3</sup>) DUST (WRF-Chem): </strong>",
              labFormat = labelFormat(prefix = ""),
              opacity = 0.5)
  
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


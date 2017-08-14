
library(RNetCDF)
library(rgdal)
library(ncdf4)
library(raster)
library(stringr)
library(leaflet)


# list .nc files

# setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs")
# setwd("D:/Dust_Event_UAE_2015/WRF_trial_runs")
setwd("Z:/_SHARED_FOLDERS/Air Quality/WRFChem/dust_opt3_chem_opt300_aod")

patt<- ".nc"
filenames <- list.files(pattern = patt)

filenames <- "aod_dust_opt3_chem_opt300.nc"  

# gerate a time sequence for the WRF-Chem run at intervals of 1 hour (should be 145 images), 6 days
start <- as.POSIXct("2015-03-30 12:00", tz = "GMT")
interval <- 60 #minutes
end <- start + as.difftime(6, units="days")
TS <- seq(from=start, by=interval*60, to=end)
TS <- TS[1:109]
name <- str_sub(filenames, start = 1, end = -11)


# inizialise an empty raster to stack ALL HOURS together in an unique raster 

 import_nc_WRF <- function(filenames){

  WRF_file <- open.nc(filenames)
  WRF_file <- read.nc(WRF_file)
  name_vari <- names(WRF_file)
  
  
#### only one variable 
  
     var_value<-WRF_file[4]        # AOD == var = 4
     names(var_value) <- "xxyyzz"
     var_value <- (var_value$xxyyzz)
     LON <-WRF_file$lon
     LAT <-WRF_file$lat

     xmn = min(LON)
     xmx = max(LON)
     ymn = min(LAT)
     ymx = max(LAT)

    
  all_rasters <- stack()    # stack ALL 115 HOURS together in an unique raster
     
# i = 5
  
  for (i in 1:dim(var_value)[3]){      # time dimension (always 109)
    MMM <-  t(var_value[ , , i])    # map is upside down 
    MMM <- MMM[nrow(MMM):1, ]
    r <- raster(MMM, xmn, xmx, ymn,  ymx, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    plot(r)
    name_time <- TS[i]
    names(r)<- paste("WRF_Chem_", name_time, sep = "")
    all_rasters<- stack(all_rasters,r)
  }
  
  
  write.csv(names(all_rasters), paste(name,  ".csv", sep=""))
  return(all_rasters)
 }
 
 
 BBB <- lapply(filenames, import_nc_WRF) 
 
 
 # make a large stack raster with the 19*3=57 layeer for DUST_1
 ras_stack <- stack()
 
# kk <- 50
 
 for (kk in 1:109){          # number of hours (time stamp), 115 hours
   plot(BBB[[1]],kk)
   # ras <- raster(BBB[[jj]], kk)
   ras <- raster(BBB[[1]], kk)
   ras_stack<- stack(ras_stack,ras)
 }
 #}
 
 
 
 # AAA <- ras_stack[[70]]
 # plot(AAA) 
 
 
writeRaster(ras_stack, "AOD_WRFChem_02April2015_aod_dust_opt3_chem_opt300.tif" , options= "INTERLEAVE=BAND", overwrite=T)


#########################################################################################

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
# shp_ME <- readOGR(dsn = dir_ME, layer = "ADMIN_domain_d01_WRFChem_small")
shp_ME <- readOGR(dsn = dir_ME, layer = "ADMIN_domain_MIKE.shp")
shp_ME <- spTransform(shp_ME, CRS("+init=epsg:4326"))

plot(shp_ME)




# set directory where we want to save the images
# setwd("D:/Dust_Event_UAE_2015/WRF_trial_runs/images_png")
setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/images_png/AOD_dust_opt3_chem_opt300_MIKE")

# save images as webshot from leaflet
# reload rasters by band or layers (145 scenes)

# gerate a time sequence for a given day every 60 minuntes (should be 73 images)
start <- as.POSIXct("2015-03-30 12:00")
interval <- 60 #minutes
end <- start + as.difftime(6, units="days")
TS <- seq(from=start, by=interval*60, to=end)
TS <- TS[1:109]

# i = 4

# load raster stack ---------------------------------

# WRF_STACK_image <- stack("D:/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif")
# WRF_STACK_image <- stack("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif")

# WRF_STACK_image <- stack("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/AOD_WRFChem_02April2015_stack_5_DAYS.tif")
# WRF_STACK_image <- stack("D:/Dust_Event_UAE_2015/WRF_trial_runs/AOD_WRFChem_02April2015_stack_5_DAYS.tif")
WRF_STACK_image <- stack("Z:/_SHARED_FOLDERS/Air Quality/WRFChem/dust_opt3_chem_opt300_aod/AOD_WRFChem_02April2015_aod_dust_opt3_chem_opt300.tif")

output_folder <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/images_png/AOD_dust_opt3_chem_opt300_MIKE/"

####### color pallet

vec_all <- as.vector(WRF_STACK_image)

min_val<- (min(vec_all,  na.rm = T))
max_val<- (max(vec_all, na.rm = T))
max_val <- 4



stat_dat <- summary(as.vector(WRF_STACK_image))
IQR <- (as.numeric((stat_dat[5]-stat_dat[2])* 2))# n is the space after IQR

low_IQR<- if(floor(min_val) > floor(as.numeric((stat_dat[2]- IQR)))) floor(min_val) else floor(as.numeric((stat_dat[2]- IQR)))
high_IQR <-if (max_val > (as.numeric((stat_dat[5]+IQR)))) max_val else (as.numeric((stat_dat[5]+IQR)))


# cool = rainbow(25, start=rgb2hsv(col2rgb('cyan'))[1], end=rgb2hsv(col2rgb('blue'))[1])
cool = rainbow(50, start=rgb2hsv(col2rgb('green'))[1], end=rgb2hsv(col2rgb('royalblue2'))[1])
cool_2 = rainbow(25, start=rgb2hsv(col2rgb('yellow'))[1], end=rgb2hsv(col2rgb('green'))[1])
warm = rainbow(125, start=rgb2hsv(col2rgb('red'))[1], end=rgb2hsv(col2rgb('yellow'))[1])
cols = c(rev(cool), rev(cool_2), rev(warm))

# greenyellow at the place of yellow?

# i <- 3


########################
### plots of maps ######
########################

AOD_images <- stack("Z:/_SHARED_FOLDERS/Air Quality/WRFChem/dust_opt3_chem_opt300_aod/AOD_WRFChem_02April2015_aod_dust_opt3_chem_opt300.tif")

for (i in 1:length(AOD_images@layers)) {
TITLE <- paste(TS[i], " (UTC)")
name_time <- TS[i]
AOD_images <- raster("Z:/_SHARED_FOLDERS/Air Quality/WRFChem/dust_opt3_chem_opt300_aod/AOD_WRFChem_02April2015_aod_dust_opt3_chem_opt300.tif", band = i)
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


###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################
###################################################################################


# stat_dat <- summary(vec_all)
# IQR <- (as.numeric((stat_dat[5]-stat_dat[2])* 1.5))# n is the space after IQR
# 
# low_IQR <-if (min_val <  (as.numeric((stat_dat[2]- IQR)))) (as.numeric((stat_dat[2]- IQR))) else min_val
# high_IQR <-if ( max_val > (as.numeric((stat_dat[5]+IQR)))) max_val else (as.numeric((stat_dat[5]+IQR)))

# low_IQR <-min_val
# high_IQR <-max_val



# vec_all_1 <- vec_all[ vec_all >= low_IQR & vec_all <= high_IQR & !is.na(vec_all) ]
# 
# xxx<- pretty( vec_all, n=10)
# 
# {
#   if (max_val <= max(xxx)){
#     xxx<- unique(c( xxx))
#   }else{
#     xxx<- unique(c( xxx, max_val))
#   }
# 
#   if (min_val >= min(xxx)){
#     xxx<- unique(c( xxx))
#   }else{
#     xxx<- unique(c(min_val, xxx))
#   }
# }
# 
# 
# 
# 
# cool = rainbow(50, start=rgb2hsv(col2rgb('cyan'))[1], end=rgb2hsv(col2rgb('blue'))[1])
# warm = rainbow(50, start=rgb2hsv(col2rgb('red'))[1], end=rgb2hsv(col2rgb('yellow'))[1])
# cols = c(rev(cool), rev(warm))
# mypalette <- colorRampPalette(cols)(255)


# pal <- colorBin(c("blue", "green", "red"), seq(0,0.2, by = 0.01), 10, pretty=F)
# pal <- colorBin(c("blue", "green", "red"), c(0, 0.02, 0.05, 0.5), 10, pretty=F)

# pal <- colorNumeric(rev(terrain.colors(255)),
#                           c(min_val, max_val),na.color = "transparent")

# pal <- colorNumeric(c("blue", "green", "red"),
#                     domain = c(min_val, max_val), na.color = "transparent")
# 
# pal <- colorNumeric(rev(terrain.colors(255)),
#                      domain = c(min_val, max_val), na.color = "transparent")
# 
# 
# # i <- 5
# 
# for (i in 1:length(WRF_STACK_image@layers)) {
#   # load the stacked raster with all the 115 images
#   WRF_STACK_image <- raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/AOD_WRFChem_02April2015_stack_5_DAYS.tif", band = i)
# #  WRF_STACK_image <- raster("D:/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = i)
# #  WRF_STACK_image <- raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = i)
#   plot(WRF_STACK_image)
#   
#   name_time <- TS[i]
# 
#   
#   # pal_WRF <- colorNumeric(c("#b7b7ff", "#ffd699", "#FFFF00", "#ffbf00", "#ffc700", "#FF0000", "#994c00"),  #"#9999FF"
#   #                         c(min_val, max_val), na.color = "transparent")
#   
#   # define popup for time scene
#   "h1 { font-size: 3px;}"
#   content <- paste('<h1><strong>', name_time,'', sep = "")
#   
#   map <- leaflet(shp_ME) %>% 
#     addTiles() %>% 
#     addTiles(group = "OSM (default)") %>%
#     addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
#     addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
#     addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
#     
#     addPopups(37, 35, content,
#               options = popupOptions(closeButton = FALSE)) %>%
#     
#     addRasterImage(WRF_STACK_image, 
#                    colors = pal, 
#                    opacity = 1, group = "WRF_CHEM") %>%
# 
#     addPolygons(stroke = TRUE, smoothFactor = 1, fillOpacity = 0,
#                 weight = 3, color = "#FFFFFF",
#                 group = "ME") %>%
#     
#     addLayersControl(
#       baseGroups = c("Toner Lite", "Road map", "Satellite"),
#       overlayGroups = c("WRF_CHEM", "ME"),
#       options = layersControlOptions(collapsed = TRUE)) %>%
#     addLegend("bottomright", pal = pal, values = c(min_val, max_val),
#               title = "<br><strong> AOD </strong>",
#               labFormat = labelFormat(prefix = ""),
#               opacity = 1)
  
  ## This is the png creation part
  # saveWidget(map, 'temp.html', selfcontained = FALSE)
  # webshot('temp.html', file = paste0(str_sub(name_time, start = 1, end = -10), "_",
  #                                    str_sub(name_time, start = 12, end = -7), "_",
  #                                    str_sub(name_time, start = 15, end = -4),
  #                                    ".png"), vwidth = 900, vheight = 900,
  #         cliprect = 'viewport')
#  print(map)
  
# }

# to make a movie.......
# to use with ImageMagik using the commnad line cmd in windows
# cd into the directory where there are the png files
# magick -delay 50 -loop 0 *.png WRF_Chem_DUST_event_02_April_2015.gif


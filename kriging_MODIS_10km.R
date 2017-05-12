
library(readr)
library(sp)
library(raster)
library(gstat)
library(rgdal)
library(RNetCDF)
library(ncdf4)
library(stringr)

library(raster)
library(leaflet)
library(htmlwidgets)

setwd("D:/Dust_Event_UAE_2015/WRF_trial_runs")

# get extent from WRFChem output-----------------------------------------------
# use the raster stack with all the WRF_chem output

# get only one image
WRF_STACK_image <- raster("DUST_WRFChem_02_April_2015_stack.tif", band = 2)
plot(WRF_STACK_image)

# get the extent of the raster
e <- extent(WRF_STACK_image)
# make a spatial polygon from the extent
p <- as(e, "SpatialPolygons")
plot(p)
crs(p) <- "proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# save shp file for the rectangular DOMAIN from WRFChem
setwd("D:/Dust_Event_UAE_2015/WRFChem_domain")
shapefile(p, "domain_d01_WRFChem.shp", overwrite=TRUE)


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

########################################################################
########################################################################


#### importing the domain shapefile to use as a masking 
dir <- "D:/Dust_Event_UAE_2015/WRFChem_domain"
shp_WRF <- readOGR(dsn = dir, layer = "domain_d01_WRFChem")


shp_WRF@data$name <- 1:nrow(shp_WRF)
plot(shp_WRF)
plot(shp_UAE, add=TRUE, lwd=1)



########################################################################
########################################################################
############## KRIGING #################################################
########################################################################

# TERRA 

# setwd("Z:/_SHARED_FOLDERS/Dust_Event_UAE_2015/MODIS_10km/TERRA")
setwd("D:/Dust_Event_UAE_2015/MODIS_10km/TERRA")
filenames_TERRA <- list.files(pattern = ".csv")

# kriging_points <- function(filenames_TERRA, resl_ras= 0.1, shp_WRF = "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRFChem_domain"){
  kriging_points <- function(filenames_TERRA, resl_ras= 0.1, shp_WRF = "D:/Dust_Event_UAE_2015/WRFChem_domain"){
    
  date <- str_sub(filenames_TERRA, start = 11, end = -14)
  dawit_TERRA <- read_csv(filenames_TERRA)
    
  #masking layer or shapefile
  
  if (is.character(shp_WRF)) {
    setwd(shp_WRF)
    dir<- shp_WRF
    shp_WRF <- readOGR(dsn = dir, layer = "domain_d01_WRFChem")
#    setwd("Z:/_SHARED_FOLDERS/Dust_Event_UAE_2015/MODIS_10km/TERRA")
    setwd("D:/Dust_Event_UAE_2015/MODIS_10km/TERRA")

  }

    limit_x_y<-extent(shp_WRF)
  
    dawit_TERRA$x <- dawit_TERRA$lon
    dawit_TERRA$y <- dawit_TERRA$lat
  
  coordinates(dawit_TERRA) = ~ x + y  ## Set spatial coordinates to create a Spatial object:
  
  spplot(dawit_TERRA, zcol="values")
  
  
  ## make a variogram----------------------------------------------------------------
  
  vargram_AOD <- variogram(values ~ 1, dawit_TERRA) # calculates sample variogram values
  nn <- floor(length(vargram_AOD$gamma)/2)
  var_for_fit<- mean(vargram_AOD[nn:nrow(vargram_AOD),3])
  
  
  # fit the variogram
  vargram_AOD_fit  <- fit.variogram(vargram_AOD, fit.ranges = FALSE, fit.sills = FALSE,
                                      vgm(var_for_fit, "Sph"), fit.kappa = TRUE)
  
  plot(vargram_AOD, vargram_AOD_fit) # plot the sample values, along with the fit model
  
  plot(vargram_AOD_fit)
  


  # make a regular empty grid
  x.range <- as.numeric(c(floor(limit_x_y[1]-1),ceiling(limit_x_y[2]+1)))  # min/max longitude of the interpolation area
  y.range <- as.numeric(c(floor(limit_x_y[3]-1),ceiling(limit_x_y[4]+1)))  # min/max latitude of the interpolation area
  
  
  ## grid at 10km resolution
  grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = resl_ras),
                     y = seq(from = y.range[1], to = y.range[2], by = resl_ras))  # expand points to grid
  coordinates(grd) <- ~x + y
  gridded(grd) <- TRUE
  
  plot(grd, cex = 1.5, col = "grey")
  points(dawit_TERRA, pch = 1, col = "red", cex = 1)
 
  
  f.1 <- as.formula(Precip_in ~ X + Y)
  # perform kriging
  dat.krg <- gstat::krige(values ~ 1, dawit_TERRA, grd, vargram_AOD_fit, nmax = 50)
  

  r <- raster(dat.krg)
  plot(shp_UAE, add=TRUE, lwd=1)
  plot(r)
  projection(r) <- CRS("+proj=longlat +datum=WGS84")
  
 # writeRaster(r, paste0("Z:/_SHARED_FOLDERS/Dust_Event_UAE_2015/MODIS_10km/TERRA/AOD_MOD04_10km_UAE","_",date,".tif"), overwrite = TRUE)
  writeRaster(r, paste0("D:/Dust_Event_UAE_2015/MODIS_10km/TERRA/AOD_MOD04_10km_UAE","_",date,".tif"), overwrite = TRUE)
  
    return(r)
}

# kriging_points(filenames_TERRA)
BBB <- lapply(filenames_TERRA, kriging_points)

######################################################################################
######################################################################################
######################################################################################

# AQUA

# setwd("Z:/_SHARED_FOLDERS/Dust_Event_UAE_2015/MODIS_10km/AQUA")
setwd("D:/Dust_Event_UAE_2015/MODIS_10km/AQUA")
filenames_AQUA <- list.files(pattern = ".csv")

# kriging_points <- function(filenames_AQUA, resl_ras= 0.1, shp_WRF = "Z:/_SHARED_FOLDERS/Dust_Event_UAE_2015/WRFChem_domain"){
  kriging_points <- function(filenames_AQUA, resl_ras= 0.1, shp_WRF = "D:/Dust_Event_UAE_2015/WRFChem_domain"){
    
  date <- str_sub(filenames_AQUA, start = 11, end = -14)
  dawit_AQUA <- read_csv(filenames_AQUA)
  
  #masking layer or shapefile
  
  if (is.character(shp_WRF)) {
    setwd(shp_WRF)
    dir<- shp_WRF
    shp_WRF <- readOGR(dsn = dir, layer = "domain_d01_WRFChem")
    # setwd("Z:/_SHARED_FOLDERS/Dust_Event_UAE_2015/MODIS_10km/AQUA")
    setwd("D:/Dust_Event_UAE_2015/MODIS_10km/AQUA")
    
  }
  
  limit_x_y<-extent(shp_WRF)
  
  dawit_AQUA$x <- dawit_AQUA$lon
  dawit_AQUA$y <- dawit_AQUA$lat
  
  coordinates(dawit_AQUA) = ~ x + y  ## Set spatial coordinates to create a Spatial object:
  
  spplot(dawit_AQUA, zcol="values")
  
  
  ## make a variogram----------------------------------------------------------------
  
  vargram_AOD <- variogram(values ~ 1, dawit_AQUA) # calculates sample variogram values
  nn <- floor(length(vargram_AOD$gamma)/2)
  var_for_fit<- mean(vargram_AOD[nn:nrow(vargram_AOD),3])
  
  
  # fit the variogram
  vargram_AOD_fit  <- fit.variogram(vargram_AOD, fit.ranges = FALSE, fit.sills = FALSE,
                                    vgm(var_for_fit, "Sph"), fit.kappa = TRUE)
  
  plot(vargram_AOD, vargram_AOD_fit) # plot the sample values, along with the fit model
  
  plot(vargram_AOD_fit)
  
  
  
  # make a regular empty grid
  x.range <- as.numeric(c(floor(limit_x_y[1]-1),ceiling(limit_x_y[2]+1)))  # min/max longitude of the interpolation area
  y.range <- as.numeric(c(floor(limit_x_y[3]-1),ceiling(limit_x_y[4]+1)))  # min/max latitude of the interpolation area
  
  
  ## grid at 10km resolution
  grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = resl_ras),
                     y = seq(from = y.range[1], to = y.range[2], by = resl_ras))  # expand points to grid
  coordinates(grd) <- ~x + y
  gridded(grd) <- TRUE
  
  plot(grd, cex = 1.5, col = "grey")
  points(dawit_AQUA, pch = 1, col = "red", cex = 1)
  
  
  f.1 <- as.formula(Precip_in ~ X + Y)
  # perform kriging
  dat.krg <- gstat::krige(values ~ 1, dawit_AQUA, grd, vargram_AOD_fit, nmax = 50)
  
  
  r <- raster(dat.krg)
  plot(shp_UAE, add=TRUE, lwd=1)
  plot(r)
  projection(r) <- CRS("+proj=longlat +datum=WGS84")
  
  # writeRaster(r, paste0("Z:/_SHARED_FOLDERS/Dust_Event_UAE_2015/MODIS_10km/AQUA/AOD_MOY04_10km_UAE","_",date,".tif"), overwrite = TRUE)
  writeRaster(r, paste0("D:/Dust_Event_UAE_2015/MODIS_10km/AQUA/AOD_MOY04_10km_UAE","_",date,".tif"), overwrite = TRUE)
  
  return(r)
}

# kriging_points(filenames_TERRA)
BBB <- lapply(filenames_AQUA, kriging_points)




#########################################################################
# average tif files from MODIS TERRA and AQUA

# Terra_filenames <- list.files(path = "Z:/_SHARED_FOLDERS/Dust_Event_UAE_2015/MODIS_10km/TERRA", pattern = ".tif")
# Aqua_filenames <- list.files(path = "Z:/_SHARED_FOLDERS/Dust_Event_UAE_2015/MODIS_10km/AQUA", pattern = ".tif")

Terra_filenames <- list.files(path = "D:/Dust_Event_UAE_2015/MODIS_10km/TERRA", pattern = ".tif")
Aqua_filenames <- list.files(path = "D:/Dust_Event_UAE_2015/MODIS_10km/AQUA", pattern = ".tif")

# Terra_filenames <- Terra_filenames[1]
# Aqua_filenames <- Aqua_filenames[1]

average_raster <- function(Terra_filenames, Aqua_filenames) {

date <- str_sub(Terra_filenames, start = 20, end = -5)
# setwd("Z:/_SHARED_FOLDERS/Dust_Event_UAE_2015/MODIS_10km/TERRA")
setwd("D:/Dust_Event_UAE_2015/MODIS_10km/TERRA")
r_Terra <- raster(Terra_filenames)
# setwd("Z:/_SHARED_FOLDERS/Dust_Event_UAE_2015/MODIS_10km/AQUA")
setwd("D:/Dust_Event_UAE_2015/MODIS_10km/AQUA")
r_Aqua <- raster(Aqua_filenames)

# make an average
r_average <- (r_Terra + r_Aqua)/2
# writeRaster(r_average, paste0("Z:/_SHARED_FOLDERS/Dust_Event_UAE_2015/MODIS_10km/AVG_TERRA_AQUA/AVG_TERRA_AQUA","_",date,".tif"), overwrite = TRUE)
writeRaster(r_average, paste0("D:/Dust_Event_UAE_2015/MODIS_10km/AVG_TERRA_AQUA/AVG_TERRA_AQUA","_",date,".tif"), overwrite = TRUE)
return(r_average)
plot(r_average)

}

# average_raster(Terra_filenames, Aqua_filenames)

FFF <- mapply(average_raster, Terra_filenames, Aqua_filenames)



##################################################################################
#### reload data and make a MAP ##################################################
# setwd("Z:/_SHARED_FOLDERS/Dust_Event_UAE_2015/MODIS_10km/AVG_TERRA_AQUA")
setwd("D:/Dust_Event_UAE_2015/MODIS_10km/AVG_TERRA_AQUA")

# multiply raster by conversion factor from AOD to PM10
  March_29 <- raster("AVG_TERRA_AQUA_88.tif")*294
  March_30 <- raster("AVG_TERRA_AQUA_89.tif")*294
  March_31 <- raster("AVG_TERRA_AQUA_90.tif")*294
  April_01 <- raster("AVG_TERRA_AQUA_91.tif")*294
  April_02 <- raster("AVG_TERRA_AQUA_92.tif")*294
  April_03 <- raster("AVG_TERRA_AQUA_93.tif")*294
  
  plot(March_29)
  plot(March_30)
  plot(March_31)
  plot(April_01)
  plot(April_02)
  plot(April_03)
  
  
# make a stack raster---------------------------------------------------------------------
  ########################################################################################
  patt<- ".tif"
  filenames <- list.files(pattern = patt)
  all_rasters <- stack()    # inizialise the raster stack
  
  # i <- 2
  
  for (i in 1:length(filenames)){   
    r <- raster(filenames[i])
    plot(r)
    all_rasters<- stack(all_rasters,r)
  }
  
  
  writeRaster(all_rasters, "MODIS_DUST_event_02_April_2015.tif" , options= "INTERLEAVE=BAND", overwrite=T)
  

########################################################################################
########################################################################################

    
  MIN_PAL <- 0.1*294
  MAX_PAL <- 4.5*294
  
  pal <- colorNumeric(c("#9999FF", "#FFFF00", "#FF0000", "#ff8000"),
                      c(MIN_PAL, MAX_PAL),na.color = "transparent")
  
  pal <- colorNumeric(c("#0000ff", "#ffffff", "#ff0000"),
                      c(MIN_PAL, MAX_PAL),na.color = "transparent")
  
  pal <- colorNumeric(rev(terrain.colors(255)),
                      c(MIN_PAL, MAX_PAL),na.color = "transparent")
  
  map <- leaflet() %>%
    addTiles(group = "OSM (default)") %>%
    addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
    addProviderTiles("Thunderforest.Landscape", group = "Topographical") %>%
    addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
    addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
    
    addRasterImage(March_29, colors = pal, opacity = 0.75,
                   group = "March_29_2015") %>%
    addRasterImage(March_30, colors = pal, opacity = 0.75,
                   group = "March_30_2015") %>%
    addRasterImage(March_31, colors = pal, opacity = 0.75,
                   group = "March_31_2015") %>%
    addRasterImage(April_01, colors = pal, opacity = 0.75,
                   group = "April_01_2015") %>%
    addRasterImage(April_02, colors = pal, opacity = 0.75,
                   group = "April_02_2015") %>%
    addRasterImage(April_03, colors = pal, opacity = 0.75,
                   group = "April_03_2015") %>%
    
    addLegend("bottomright", pal = pal, values = c(MIN_PAL, MAX_PAL), 
              # title = "<br><strong>AOD-MODIS</strong>",
              title = "<br><strong>PM<sub>10</sub> (<font face=symbol>m</font>g/m<sup>3</sup>) MODIS: </strong>",
              labFormat = labelFormat(prefix = ""),
              opacity = 0.75) %>%
    
    addLayersControl(
      baseGroups = c("Toner Lite", "Road map", "Topographical", "Satellite"),
      overlayGroups = c("March_29_2015", "March_30_2015",  "March_31_2015",
                        "April_01_2015","April_02_2015", "April_03_2015"),
      options = layersControlOptions(collapsed = TRUE)) %>%
    hideGroup(c("March_29_2015", "March_30_2015",  "March_31_2015",
                "April_01_2015", "April_03_2015")) 
  
  map
  
  # save map
  # saveWidget(map, paste0("Z:/_SHARED_FOLDERS/Dust_Event_UAE_2015/DUST_event_April_2.html"), selfcontained = FALSE)
  saveWidget(map, paste0("D:/Dust_Event_UAE_2015/DUST_event_April_2_WRF_Domain.html"), selfcontained = FALSE)
  
  

#################################################################################
#### reload data and make a MOVIE ##################################################


library(leaflet)
library(webshot)
library(htmlwidgets)
library(RColorBrewer)
library(raster)
library(classInt)
library(stringr)


# gerate a time sequence of 6 days
start <- as.POSIXct("2015-03-29 12:00")
interval <- 60*12 #minutes (1 day interval)
end <- start + as.difftime(5, units="days")  # 6 days
TS <- seq(from=start, by=interval*60*2, to=end)

# setwd("Z:/_SHARED_FOLDERS/Dust_Event_UAE_2015/MODIS_10km/AVG_TERRA_AQUA")
setwd("D:/Dust_Event_UAE_2015/MODIS_10km/AVG_TERRA_AQUA")

# i <- 2

for (i in 1:6) {

path <- ".tif"
filenames <- list.files(pattern = path)
# multiply raster by conversion factor from AOD to PM10
raster_MODIS <- raster(filenames[i])*294
plot(raster_MODIS)

name_time <- TS[i]


# define popup for time scene
"h2 { font-size: 3px;}"
content <- paste('<h2><strong>', name_time,'', sep = "")


MIN_PAL <- 0.1*294
MAX_PAL <- 4.5*294

pal_MODIS <- colorNumeric(c("#9999FF", "#FFFF00", "#FF0000", "#ff8000"),
                    c(MIN_PAL, MAX_PAL),na.color = "transparent")

pal_MODIS <- colorNumeric(c("#0000ff", "#ffffff", "#ff0000"),
                    c(MIN_PAL, MAX_PAL),na.color = "transparent")

pal_MODIS <- colorNumeric(rev(terrain.colors(255)),
                    c(MIN_PAL, MAX_PAL),na.color = "transparent")

map <- leaflet() %>% 
  addTiles() %>% 
  addTiles(group = "OSM (default)") %>%
  addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
  
  addPopups(50, 33, content,
            options = popupOptions(closeButton = FALSE)) %>%
  
  addRasterImage(raster_MODIS, 
                 colors = pal_MODIS, 
                 opacity = 0.5, group = "MODIS") %>%
  addLayersControl(
    baseGroups = c("Road map", "Toner Lite","Satellite"),
    overlayGroups = "MODIS",
    options = layersControlOptions(collapsed = TRUE)) %>%
  addLegend("bottomright", pal = pal_MODIS, values = c(MIN_PAL, MAX_PAL),
            title = "<br><strong>PM<sub>10</sub> (<font face=symbol>m</font>g/m<sup>3</sup>) MODIS: </strong>",
            labFormat = labelFormat(prefix = ""),
            opacity = 0.5)

## This is the png creation part
saveWidget(map, 'temp.html', selfcontained = FALSE)
webshot('temp.html', file = paste0(str_sub(name_time, start = 1, end = -10), "_",
                                   str_sub(name_time, start = 12, end = -7), "_",
                                   str_sub(name_time, start = 15, end = -4),
                                   ".png"), vwidth = 1000, vheight = 1000,
        cliprect = 'viewport')

}

# to make a movie.......
# to use with ImageMagik using the commnad line cmd in windows
# cd into the directory where there are the png files

# magick -delay 100 -loop 0 *.png MODIS_DUST_event_02_April_2015.gif

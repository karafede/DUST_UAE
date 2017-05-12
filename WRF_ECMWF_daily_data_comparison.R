
library(readr)
library(dplyr)
library(lubridate)
library(rgdal)
# install.packages("NISTunits", dependencies = TRUE)
library(RNetCDF)
library(rgdal)
library(ncdf4)
library(raster)
library(stringr)
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

########################################################################
########################################################################


# #### importing the domain shapefile to use as a masking 
# # dir <- "D:/Dust_Event_UAE_2015/WRFChem_domain"
# dir <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRFChem_domain"
# shp_WRF <- readOGR(dsn = dir, layer = "domain_d01_WRFChem")
# 
# 
# shp_WRF@data$name <- 1:nrow(shp_WRF)
# plot(shp_WRF)
# plot(shp_UAE, add=TRUE, lwd=1)

# load MAIAC extent-------------------------------------------

dir <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/extent_MAIAC_UAE"
### shapefile for UAE
shp_MAIAC <- readOGR(dsn = dir, layer = "extent_MAIAC")


projection(shp_MAIAC) <- CRS("+proj=longlat +datum=WGS84")
# ----- Transform to EPSG 4326 - WGS84 (required)
shp_MAIAC <- spTransform(shp_MAIAC, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
plot(shp_MAIAC)



#########################################################################################################
# read all WRF  bands at 16 pm (THAT is the ECMWF time) in a stack raster ###############################

# read all bands in a stack raster
# WRF_STACK_image <- stack("D:/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02_April_2015_stack.tif")
WRF_STACK_image <- stack("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif")
n <- length(WRF_STACK_image@layers)-1


# generate a time sequence for the WRF-Chem run at intervals of 1 hour (should be 73 images)
start <- as.POSIXct("2015-03-29 00:00:00")
interval <- 60 #minutes
end <- start + as.difftime(6, units="days")
TS <- seq(from=start, by=interval*60, to=end)   # same time series as the AQ data
TS <- TS[1:144]

# i <- 3

# average WRF rasters from 11am to 1pm (I will get 3 rasters at the same time of the MODIS/MAIAC data)
# MODIS TERRA: from 10 to 11 am
# MODIS AQUA: from 13 to 14

raster <- stack("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif") 
WRF_2015_03_29_TERRA <- mean(raster[[11:12]])   # mean from 10am to 11am
plot(WRF_2015_03_29_TERRA)
WRF_2015_03_30_TERRA <- mean(raster[[35:36]])   # mean from 10am to 11am
plot(WRF_2015_03_30_TERRA)
WRF_2015_03_31_TERRA <- mean(raster[[59:60]])   # mean from 10am to 11am
plot(WRF_2015_03_31_TERRA)
WRF_2015_04_01_TERRA <- mean(raster[[83:84]])   # mean from 10am to 11am
plot(WRF_2015_04_01_TERRA)
WRF_2015_04_02_TERRA <- mean(raster[[107:108]])   # mean from 10am to 11am
plot(WRF_2015_04_02_TERRA)
WRF_2015_04_03_TERRA <- mean(raster[[131:132]])   # mean from 10am to 11am
plot(WRF_2015_04_03_TERRA)


WRF_2015_03_29_AQUA <- mean(raster[[14:15]])   # mean from 13pm to 14pm
plot(WRF_2015_03_29_AQUA)
WRF_2015_03_30_AQUA <- mean(raster[[38:39]])   # mean from 13pm to 14pm
plot(WRF_2015_03_30_AQUA)
WRF_2015_03_31_AQUA <- mean(raster[[59:60]])   # mean from 13pm to 14pm
plot(WRF_2015_03_31_AQUA)
WRF_2015_04_01_AQUA <- mean(raster[[62:63]])   # mean from 13pm to 14pm
plot(WRF_2015_04_01_AQUA)
WRF_2015_04_02_AQUA <- mean(raster[[86:87]])   # mean from 13pm to 14pm
plot(WRF_2015_04_02_AQUA)
WRF_2015_04_03_AQUA <- mean(raster[[110:110]])   # mean from 13pm to 14pm
plot(WRF_2015_04_03_AQUA)



# make a stack
all_rasters_WRF <- stack(WRF_2015_03_29_TERRA,
                         WRF_2015_03_30_TERRA,
                         WRF_2015_03_31_TERRA,
                         WRF_2015_04_01_TERRA,
                         WRF_2015_04_02_TERRA,
                         WRF_2015_04_03_TERRA,
                         WRF_2015_03_29_AQUA,
                         WRF_2015_03_30_AQUA,
                         WRF_2015_03_31_AQUA,
                         WRF_2015_04_01_AQUA,
                         WRF_2015_04_02_AQUA,
                         WRF_2015_04_03_AQUA)

res(all_rasters_WRF)



######################################################################################################
# load MODIS data  (6 rasters for the same time range of WRF-CHEM) ###################################
######################################################################################################

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


######################################################################################################################
# load ECMWF data  (6 rasters for the same time range of WRF-CHEM) (15:00 - 16:00) ###################################
######################################################################################################################

# setwd("D:/Dust_Event_UAE_2015/ECMWF")
setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/ECMWF")

# tranform NetCDF file into rasters (do this manually)

patt<- ".nc"
filenames <- list.files(pattern = patt)
 filenames <- filenames[1]  # April 2015 6 UTC (10 am)
 
filenames <- list.files(pattern = patt)
 filenames <- filenames[2]  # April 2015 9 UTC (13 pm)

filenames <- list.files(pattern = patt)
 filenames <- filenames[3]  # March 2015 6 UTC (10 am)

filenames <- list.files(pattern = patt)
 filenames <- filenames[4]  # April 2015 9 UTC (13 pm)
 

# April ###--------------------------------------------------------------------------------------
 # generate a time sequence for the  ECMWF run @ April at intervals of 1 day (should be 30 images)
 start <- as.POSIXct("2015-04-01 00:00")
 interval <- 60*24 #minutes
 end <- start + as.difftime(29, units="days")  # April 2015
 TS <- seq(from=start, by=interval*60, to=end)
 
 
 # March ###-------------------------------------------------------------------------------------
 # generate a time sequence for the  ECMWF run @ March at intervals of 1 day (should be 31 images)
 start <- as.POSIXct("2015-03-01 00:00")
 interval <- 60*24 #minutes
 end <- start + as.difftime(30, units="days")  # March 2015
 TS <- seq(from=start, by=interval*60, to=end)
 

 
 
 ##### run the function below for March and Apri.
 ###### make raster stacks ECMWF_pm2p5 and ECMWF_pm10  and then move to the folders April 2015 and March 2015, respectively


import_nc_ECMWF <- function(filenames){
  
  ECMWF_file <- open.nc(filenames)
  ECMWF_file <- read.nc(ECMWF_file)
  name_vari <- names(ECMWF_file)
  
  
  ######
  
  # j = 6
  
  #### only variable pm2p5 and pm10 == var = 4 and 5
  
  for(j in 5:6) {     # pm25 and pm10  these are in kg/m3
  var_value <- (ECMWF_file[j])
  names(var_value) <- "xxyyzz"
  var_value <- (var_value$xxyyzz)
  LON <- ECMWF_file$longitude
  LAT <- ECMWF_file$latitude
  
  xmn= min(LON)
  xmx=max(LON)
  ymn=min(LAT)
  ymx=max(LAT)
  
  all_rasters <- stack()    # stack ALL 73 HOURS together in an unique raster
  
  # i = 3

  
  for (i in 1:dim(var_value)[3]){      # time dimension (31 days for March)
     MMM <-  t(var_value[ , , i])
   #  MMM <- MMM[nrow(MMM):1, ]
    # r <- raster((var_value[ , , i]), xmn, xmx, ymn,  ymx, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
     r <- raster(MMM, xmn, xmx, ymn,  ymx, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    plot(r)
   # plot(shp_UAE, add=TRUE, lwd=1)
    name_time <- TS[i]
    names(r)<- paste("WRF_Chem_", name_time, sep = "")
    all_rasters <- stack(all_rasters,r)
  }
  
  all_rasters <- crop(all_rasters, extent(shp_MAIAC))
  all_rasters <- mask(all_rasters, shp_MAIAC)
  plot(all_rasters)
  writeRaster(all_rasters, paste("ECMWF_March_9UTC_", name_vari[j], ".tif", sep="" ) , options= "INTERLEAVE=BAND", overwrite=T )
  write.csv(names(all_rasters), paste("Layer_names_", name_vari[j], ".csv", sep="" ))
  
     }
}


BBB <- lapply(filenames, import_nc_ECMWF)   





#######################################################################################################
# read all ECMWF  bands at for 29 March, April 3 and April 2 2015 #####################################

# read all bands in a stack raster---PM10---------------------------------------------
ECMWF_STACK_image_March_6UTC <- stack("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/ECMWF/March_2015/ECMWF_March_6UTC_pm10.tif")
ECMWF_STACK_image_March_6UTC@layers
ECMWF_STACK_image_March_9UTC <- stack("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/ECMWF/March_2015/ECMWF_March_9UTC_pm10.tif")
ECMWF_STACK_image_March_9UTC@layers


ECMWF_STACK_image_April_6UTC <- stack("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/ECMWF/April_2015/ECMWF_April_6UTC_pm10.tif")
ECMWF_STACK_image_April_6UTC@layers
ECMWF_STACK_image_April_9UTC <- stack("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/ECMWF/April_2015/ECMWF_April_9UTC_pm10.tif")
ECMWF_STACK_image_April_9UTC@layers



# load the 6 days rasters----------------

# convert data from kg/m3 to g/m3  (6UTC that is 10am....MODIS/TERRA)
ECMWF_2015_03_29_TERRA <- raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/ECMWF/March_2015/ECMWF_March_6UTC_pm10.tif", band = 29)*1e+09 # 29 March
plot(ECMWF_2015_03_29_TERRA)
ECMWF_2015_03_30_TERRA <- raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/ECMWF/March_2015/ECMWF_March_6UTC_pm10.tif", band = 30)*1e+09 # 30 March
plot(ECMWF_2015_03_30_TERRA)
ECMWF_2015_03_31_TERRA <- raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/ECMWF/March_2015/ECMWF_March_6UTC_pm10.tif", band = 31)*1e+09 # 31 March
plot(ECMWF_2015_03_31_TERRA)
ECMWF_2015_04_01_TERRA <- raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/ECMWF/April_2015/ECMWF_April_6UTC_pm10.tif", band = 1)*1e+09 # 1 April
plot(ECMWF_2015_04_01_TERRA)
ECMWF_2015_04_02_TERRA <- raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/ECMWF/April_2015/ECMWF_April_6UTC_pm10.tif", band = 2)*1e+09 # 2 April
plot(ECMWF_2015_04_02_TERRA)
ECMWF_2015_04_03_TERRA <- raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/ECMWF/April_2015/ECMWF_April_6UTC_pm10.tif", band = 3)*1e+09 # 3 April
plot(ECMWF_2015_04_03_TERRA)


# convert data from kg/m3 to g/m3  (9UTC that is 13am....MODIS/AQUA)
ECMWF_2015_03_29_AQUA <- raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/ECMWF/March_2015/ECMWF_March_9UTC_pm10.tif", band = 29)*1e+09 # 29 March
plot(ECMWF_2015_03_29_AQUA)
ECMWF_2015_03_30_AQUA <- raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/ECMWF/March_2015/ECMWF_March_9UTC_pm10.tif", band = 30)*1e+09 # 30 March
plot(ECMWF_2015_03_30_AQUA)
ECMWF_2015_03_31_AQUA <- raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/ECMWF/March_2015/ECMWF_March_9UTC_pm10.tif", band = 31)*1e+09 # 31 March
plot(ECMWF_2015_03_31_AQUA)
ECMWF_2015_04_01_AQUA <- raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/ECMWF/April_2015/ECMWF_April_9UTC_pm10.tif", band = 1)*1e+09 # 1 April
plot(ECMWF_2015_04_01_AQUA)
ECMWF_2015_04_02_AQUA <- raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/ECMWF/April_2015/ECMWF_April_9UTC_pm10.tif", band = 2)*1e+09 # 2 April
plot(ECMWF_2015_04_02_AQUA)
ECMWF_2015_04_03_AQUA <- raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/ECMWF/April_2015/ECMWF_April_9UTC_pm10.tif", band = 3)*1e+09 # 3 April
plot(ECMWF_2015_04_03_AQUA)


all_rasters_ECMWF <- stack(ECMWF_2015_03_29_TERRA,
                           ECMWF_2015_03_30_TERRA,
                           ECMWF_2015_03_31_TERRA,
                           ECMWF_2015_04_01_TERRA,
                           ECMWF_2015_04_02_TERRA,
                           ECMWF_2015_04_03_TERRA,
                           ECMWF_2015_03_29_AQUA,
                           ECMWF_2015_03_30_AQUA,
                           ECMWF_2015_03_31_AQUA,
                           ECMWF_2015_04_01_AQUA,
                           ECMWF_2015_04_02_AQUA,
                           ECMWF_2015_04_03_AQUA)

res(all_rasters_ECMWF)

# make rasters WRF as same extent of raster MODIS
all_rasters_ECMWF = projectRaster(all_rasters_ECMWF, all_rasters_MODIS)
res(all_rasters_ECMWF)

# save raster stack of ECMWF at 1km resolution ################
writeRaster(all_rasters_ECMWF, "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/ECMWF/ECMWF_stack_DUST_29_March_3_April_2015.tif", overwrite = T)

################################################################################################

# make a spatial correlation between all/or/each raster ########################################
### correlation 
z <- stack(all_rasters_WRF,       # 12 rasters from WRF
           all_rasters_MODIS,     # 12 rasters from MODIS-MAIAC
           all_rasters_ECMWF)     # 12 rasters from ECMWF
length(z@layers) 

res(z)

# spatial correlations of all the layers (first 3 layes are from WRF, the second 3 layers are from ECMWF)
# r <- calc(z, fun=function(x) cor(x[1:12], x[25:36], method='pearson', use= "pairwise.complete.obs"))   # WRF and ECMWF (use 24 rasters)
# plot(r)
r <- calc(z, fun=function(x) cor(x[13:24], x[25:36], method='pearson', use= "pairwise.complete.obs"))   # MAIAC and ECMWF (use 24 rasters)
plot(r)


plot(r)
# overlay shape of UAE border
plot(shp_UAE, add=TRUE, lwd=1)




# jpeg('Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/ECMWF/hist_WRF_ECMWF.jpg',
     jpeg('Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/ECMWF/hist_MAIAC_ECMWF.jpg',
     quality = 100, bg = "white", res = 200, width = 5, height = 5, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


hist((r), breaks = 10)

par(oldpar)
dev.off()





# jpeg('Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/ECMWF/boxplot_WRF_ECMWF.jpg',   
     jpeg('Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/ECMWF/boxplot_MAIC_ECMWF.jpg',   
     quality = 100, bg = "white", res = 200, width = 5, height = 5, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

boxplot(r)

par(oldpar)
dev.off()


###############################################################
# map spatial correlation map ##################################

MIN_PAL <- -1
MAX_PAL <- +1

# pal <- colorNumeric(c("#9999FF", "#FFFF00", "#FF0000", "#ff8000"),
#                     c(MIN_PAL, MAX_PAL),na.color = "transparent")

pal <- colorNumeric(c("#0000ff", "#ffffff", "#ff0000"),
                    c(MIN_PAL, MAX_PAL), na.color = "#ffff00")

# pal <- colorNumeric(rev(terrain.colors(255)),
#                     c(MIN_PAL, MAX_PAL),na.color = "transparent")

map <- leaflet() %>%
  setView(46, 26, 6) %>%
  addTiles(group = "OSM (default)") %>%
  addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
  
  addRasterImage(r, colors = pal, opacity = 0.4,
                 group = "correlation", maxBytes = 8 * 1024 * 1024) %>%
 
  addLegend("bottomright", pal = pal, values = c(MIN_PAL, MAX_PAL), 
          #  title = "<br><strong>R<sup>2</sup> ECMWF-WRFChem: </strong>",
        #    title = "<br><strong>R ECMWF-WRFChem: </strong>",
          title = "<br><strong>R MAIAC-ECMWF: </strong>",
            labFormat = labelFormat(prefix = ""),
            opacity = 0.4) %>%
  
  addLayersControl(
    baseGroups = c("Toner Lite", "Road map", "Satellite"),
    overlayGroups = c("correlation"),
    options = layersControlOptions(collapsed = TRUE))
#  hideGroup(c("March_29_2015")) 

map

# save map
# saveWidget(map, paste0("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/ECMWF/correlation_WRF_ECMWF.html"), selfcontained = FALSE)
# webshot('Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/ECMWF/correlation_WRF_ECMWF.html',
#         file = "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/ECMWF/correlation_WRF_ECMWF.png", vwidth = 1200, vheight = 1200,
#         cliprect = 'viewport')



# save map
saveWidget(map, paste0("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/ECMWF/correlation_MAIAC_ECMWF.html"), selfcontained = FALSE)
webshot('Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/ECMWF/correlation_MAIAC_ECMWF.html',
        file = "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/ECMWF/correlation_MAIAC_ECMWF.jpg", vwidth = 1200, vheight = 1200,
        cliprect = 'viewport')


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


setwd("D:/Dust_Event_UAE_2015")


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


#### importing the domain shapefile to use as a masking 
dir <- "D:/Dust_Event_UAE_2015/WRFChem_domain"
shp_WRF <- readOGR(dsn = dir, layer = "domain_d01_WRFChem")


shp_WRF@data$name <- 1:nrow(shp_WRF)
plot(shp_WRF)
plot(shp_UAE, add=TRUE, lwd=1)



###############################################################################################
# read all WRF  bands at 11 am,  12pm and 1pm in a stack raster ###############################

# read all bands in a stack raster
WRF_STACK_image <- stack("D:/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02_April_2015_stack.tif")
n <- length(WRF_STACK_image@layers)-1


# generate a time sequence for the WRF-Chem run at intervals of 1 hour (should be 73 images)
start <- as.POSIXct("2015-03-31 00:00:00")
interval <- 60 #minutes
end <- start + as.difftime(3, units="days")
TS <- seq(from=start, by=interval*60, to=end)   # same time series as the AQ data

# i <- 3

# average WRF rasters from 11am to 1pm (I will get 3 rasters at the same time of the MODIS data)

raster <- stack("D:/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02_April_2015_stack.tif") 
WRF_2015_03_31 <- mean(raster[[12:14]])   # mean from 11am to 1pm
plot(WRF_2015_03_31)
WRF_2015_04_01 <- mean(raster[[36:38]])   # mean from 11am to 1pm
plot(WRF_2015_04_01)
WRF_2015_04_02 <- mean(raster[[60:62]])   # mean from 11am to 1pm
plot(WRF_2015_04_02)

# make a stack
all_rasters_WRF <- stack(WRF_2015_03_31,
                         WRF_2015_04_01,
                         WRF_2015_04_02)

res(all_rasters_WRF)



######################################################################################################
# load MODIS data  (3 rasters for the same time range of WRF-CHEM) ###################################
######################################################################################################


setwd("D:/Dust_Event_UAE_2015/MODIS_10km/AVG_TERRA_AQUA")

all_rasters_MODIS <- stack("D:/Dust_Event_UAE_2015/MODIS_10km/AVG_TERRA_AQUA/MODIS_DUST_event_31March_1_2_April_2015.tif")
n <- length(all_rasters_MODIS@layers)  # 3 layers
n <- length(all_rasters_WRF@layers)  # 3 layers

res(all_rasters_MODIS)
res(all_rasters_WRF)

# make rasters WRF as same extent of raster MODIS
all_rasters_WRF = projectRaster(all_rasters_WRF, all_rasters_MODIS)
res(all_rasters_WRF)


######################################################################################################
# load ECMWF data  (3 rasters for the same time range of WRF-CHEM) ###################################
######################################################################################################

setwd("D:/Dust_Event_UAE_2015/ECMWF")

# tranform NetCDF file into rasters

patt<- ".nc"
filenames <- list.files(pattern = patt)
 filenames <- filenames[2]  # march 2015
filenames <- list.files(pattern = patt)
 filenames <- filenames[1]  # april 2015


# generate a time sequence for the  ECMWF run @ March at intervals of 1 day (should be 31 images)
 start <- as.POSIXct("2015-03-01 00:00")
 interval <- 60*24 #minutes
 end <- start + as.difftime(30, units="days")  # March 2015
 TS <- seq(from=start, by=interval*60, to=end)
 
 # generate a time sequence for the  ECMWF run @ April at intervals of 1 day (should be 30 images)
 start <- as.POSIXct("2015-04-01 00:00")
 interval <- 60*24 #minutes
 end <- start + as.difftime(29, units="days")  # March 2015
 TS <- seq(from=start, by=interval*60, to=end)


import_nc_ECMWF <- function(filenames){
  
  ECMWF_file <- open.nc(filenames)
  ECMWF_file <- read.nc(ECMWF_file)
  name_vari <- names(ECMWF_file)
  
  
  ######
  
  # j = 4
  
  #### only variable pm2p5 and pm10 == var = 4 and 5
  
  for(j in 4:5) {     # pm25 and pm10
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
  
  # i = 5

  
  for (i in 1:dim(var_value)[3]){      # time dimension (31 days for March)
    # MMM <-  t(var_value[ , , i])
    # MMM <- MMM[nrow(MMM):1, ]
     r <- raster((var_value[ , , i]), xmn, xmx, ymn,  ymx, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    # r <- raster(MMM, xmn, xmx, ymn,  ymx, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    plot(r)
    name_time <- TS[i]
    names(r)<- paste("WRF_Chem_", name_time, sep = "")
    all_rasters <- stack(all_rasters,r)
  }
  
  all_rasters <- crop(all_rasters, extent(shp_WRF))
  all_rasters <- mask(all_rasters, shp_WRF)
  plot(all_rasters)
  writeRaster(all_rasters, paste("ECMWF_", name_vari[j], ".tif", sep="" ) , options= "INTERLEAVE=BAND", overwrite=T )
  write.csv(names(all_rasters), paste("Layer_names_", name_vari[j], ".csv", sep="" ))
  
     }
}


BBB <- lapply(filenames, import_nc_ECMWF)   



#######################################################################################################
# read all ECMWF  bands at for 31 March, April 1 and April 2 2015 #####################################

# read all bands in a stack raster---PM10---------------------------------------------
ECMWF_STACK_image_March <- stack("D:/Dust_Event_UAE_2015/ECMWF/March_2015/ECMWF_pm10.tif")
ECMWF_STACK_image_March@layers
ECMWF_STACK_image_April <- stack("D:/Dust_Event_UAE_2015/ECMWF/April_2015/ECMWF_pm10.tif")
ECMWF_STACK_image_April@layers

ECMWF_2015_03_31 <- raster("D:/Dust_Event_UAE_2015/ECMWF/March_2015/ECMWF_pm10.tif", band = 31)   # 31 March
plot(ECMWF_2015_03_31)
ECMWF_2015_04_01 <- raster("D:/Dust_Event_UAE_2015/ECMWF/April_2015/ECMWF_pm10.tif", band = 1)   # 1 April
plot(ECMWF_2015_04_01)
ECMWF_2015_04_02 <- raster("D:/Dust_Event_UAE_2015/ECMWF/April_2015/ECMWF_pm10.tif", band = 2)    # 2 April
plot(ECMWF_2015_04_02)


all_rasters_ECMWF <- stack(ECMWF_2015_03_31,
                           ECMWF_2015_04_01,
                           ECMWF_2015_04_02)

res(all_rasters_ECMWF)

# make rasters WRF as same extent of raster MODIS
all_rasters_ECMWF = projectRaster(all_rasters_ECMWF, all_rasters_MODIS)
res(all_rasters_ECMWF)

################################################################################################

# make a spatial correlation between all/or/each raster ########################################
### correlation 
z <- stack(all_rasters_WRF,       # 3 rasters from WRF
           all_rasters_MODIS,     # 3 rasters from MODIS
           all_rasters_ECMWF)     # 3 rasters from ECMWF
length(z@layers) 

res(z)

# spatial correlations of all the layers (first 3 layes are from WRF, the second 3 layers are from ECMWF)
# r <- calc(z, fun=function(x) cor(x[1:3], x[7:9], method='spearman', use= "pairwise.complete.obs"))   # WRF and ECMWF
# plot(r)
r <- calc(z, fun=function(x) cor(x[1:3], x[7:9], method='pearson', use= "pairwise.complete.obs"))   # WRF and ECMWF
plot(r)
# r <- calc(z, fun=function(x) cor(x[1:3], x[7:9], method='kendall', use= "pairwise.complete.obs"))   # WRF and ECMWF
# plot(r)
# r <- calc(z, fun=function(x) cor(x[1:3], x[4:6], use= "pairwise.complete.obs"))

plot(r)
# overlay shape of UAE border
plot(shp_UAE, add=TRUE, lwd=1)

hist((r), breaks = 10)
boxplot(r)


###############################################################
# map spatial correlation map ##################################

MIN_PAL <- -1
MAX_PAL <- +1

# pal <- colorNumeric(c("#9999FF", "#FFFF00", "#FF0000", "#ff8000"),
#                     c(MIN_PAL, MAX_PAL),na.color = "transparent")

pal <- colorNumeric(c("#0000ff", "#ffffff", "#ff0000"),
                    c(MIN_PAL, MAX_PAL),na.color = "transparent")

# pal <- colorNumeric(rev(terrain.colors(255)),
#                     c(MIN_PAL, MAX_PAL),na.color = "transparent")

map <- leaflet() %>%
  addTiles(group = "OSM (default)") %>%
  addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
  
  addRasterImage(r, colors = pal, opacity = 0.4,
                 group = "correlation") %>%
 
  addLegend("bottomright", pal = pal, values = c(MIN_PAL, MAX_PAL), 
          #  title = "<br><strong>R<sup>2</sup> ECMWF-WRFChem: </strong>",
            title = "<br><strong>R ECMWF-WRFChem: </strong>",
            labFormat = labelFormat(prefix = ""),
            opacity = 0.4) %>%
  
  addLayersControl(
    baseGroups = c("Toner Lite", "Road map", "Satellite"),
    overlayGroups = c("correlation"),
    options = layersControlOptions(collapsed = TRUE))
#  hideGroup(c("March_29_2015")) 

map

# save map
saveWidget(map, paste0("D:/Dust_Event_UAE_2015/ECMWF/correlation_WRF_ECMWF.html"), selfcontained = FALSE)
webshot('D:/Dust_Event_UAE_2015/ECMWF/correlation_WRF_ECMWF.html',
        file = "D:/Dust_Event_UAE_2015/ECMWF/correlation_WRF_ECMWF.png", vwidth = 1200, vheight = 1200,
        cliprect = 'viewport')


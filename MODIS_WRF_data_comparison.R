
library(readr)
library(dplyr)
library(lubridate)
library(rgdal)
# install.packages("NISTunits", dependencies = TRUE)
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

# load one WRF raster image.....just a trial...only one hour....2015-03-31_03_00

WRF_STACK_image <- raster("D:/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02_April_2015_stack.tif", band = 3)
plot(WRF_STACK_image)

# overlay shape of UAE border
plot(shp_UAE, add=TRUE, lwd=1)

# WRF-chem resolution
res(WRF_STACK_image)



############################################################################
# read all WRF  bands in a stack raster ####################################

WRF_STACK_image <- stack("D:/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02_April_2015_stack.tif")
n <- length(WRF_STACK_image@layers)-1


# generate a time sequence for the WRF-Chem run at intervals of 1 hour (should be 73 images)
start <- as.POSIXct("2015-03-31 00:00:00")
interval <- 60 #minutes
end <- start + as.difftime(3, units="days")
TS <- seq(from=start, by=interval*60, to=end)   # same time series as the AQ data


# average WRF rasters from 11am to 1pm (I will get 3 rasters at the same time of the MODIS data)

WRF_2015_03_31 <- (raster("D:/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02_April_2015_stack.tif", band = 12) +
                   raster("D:/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02_April_2015_stack.tif", band = 13) +
                  raster("D:/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02_April_2015_stack.tif", band = 14))/3

plot(WRF_2015_03_31)


WRF_2015_04_01 <- (raster("D:/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02_April_2015_stack.tif", band = 36) +
                     raster("D:/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02_April_2015_stack.tif", band = 37) +
                     raster("D:/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02_April_2015_stack.tif", band = 38))/3

plot(WRF_2015_04_01)


WRF_2015_04_02 <- (raster("D:/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02_April_2015_stack.tif", band = 60) +
                     raster("D:/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02_April_2015_stack.tif", band = 61) +
                     raster("D:/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02_April_2015_stack.tif", band = 62))/3

plot(WRF_2015_04_02)


# make a stack
all_rasters_WRF <- stack(WRF_2015_03_31,
                         WRF_2015_04_01,
                         WRF_2015_04_02)

res(all_rasters_WRF)

######################################################################################################
######################################################################################################

# load modis data  (3 rasters for the same time range of WRF-CHEM)

setwd("D:/Dust_Event_UAE_2015/MODIS_10km/AVG_TERRA_AQUA")

all_rasters_MODIS <- stack("D:/Dust_Event_UAE_2015/MODIS_10km/AVG_TERRA_AQUA/MODIS_DUST_event_31March_1_2_April_2015.tif")
n <- length(all_rasters_MODIS@layers)  # 3 layers
n <- length(all_rasters_WRF@layers)  # 3 layers

res(all_rasters_MODIS)
res(all_rasters_WRF)

# make rasters WRF as same extent of raster MODIS
all_rasters_WRF = projectRaster(all_rasters_WRF, all_rasters_MODIS)
res(all_rasters_WRF)

# make a spatial correlation between all/or/each raster

### correlation 
z <- stack(all_rasters_WRF,
           all_rasters_MODIS)
length(z@layers) 

res(z)

# spatial correlations of all the layers (first 3 layes are from WRF, the second 3 layers are from MODIS)
r <- calc(z, fun=function(x) cor(x[1:3], x[4:6], method='pearson', use= "pairwise.complete.obs"))
plot(r)
# r <- calc(z, fun=function(x) cor(x[1:3], x[4:6], use= "pairwise.complete.obs"))

plot(r)
# overlay shape of UAE border
plot(shp_UAE, add=TRUE, lwd=1)


hist((r), breaks = 10)
boxplot(r)



#### make correlation using points from raster

WRF <- values(all_rasters_WRF)
MODIS<- values(all_rasters_MODIS)

data_cor<- data.frame()

for (i in 1:nrow(WRF)){
  z <-cor(WRF[i,],MODIS[i,],   use= "pairwise.complete.obs")
  data_cor<- rbind(data_cor,z)
  
}


# get latitude and longitude from one sample raster--------------------
sample <- rasterToPoints(all_rasters_MODIS[[1]])
colnames(sample)[3] <- "value"
sample<- cbind(sample[,-3],data_cor)
rr<- rasterFromXYZ(sample)
plot(rr)
hist((rr),breaks= 100)
plot(rr-r) ### this difference shoudl be 0

# overlay shape of UAE border
plot(shp_UAE, add=TRUE, lwd=1)

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
            title = "<br><strong>R MODIS-WRFChem: </strong>",
            labFormat = labelFormat(prefix = ""),
            opacity = 0.4) %>%
  
  addLayersControl(
    baseGroups = c("Toner Lite", "Road map", "Satellite"),
    overlayGroups = c("correlation"),
    options = layersControlOptions(collapsed = TRUE))
#  hideGroup(c("March_29_2015")) 

map

# save map
saveWidget(map, paste0("D:/Dust_Event_UAE_2015/MODIS_10km/AVG_TERRA_AQUA/correlation_WRF_MODIS.html"), selfcontained = FALSE)
webshot('D:/Dust_Event_UAE_2015/MODIS_10km/AVG_TERRA_AQUA/correlation_WRF_MODIS.html',
        file = "D:/Dust_Event_UAE_2015/MODIS_10km/AVG_TERRA_AQUA/correlation_WRF_MODIS.png", vwidth = 1200, vheight = 1200,
        cliprect = 'viewport')

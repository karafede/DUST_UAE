
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

# # load one WRF raster image.....just a trial...only one hour....2015-03-31_03_00
# 
# # WRF_STACK_image <- raster("D:/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02_April_2015_stack.tif", band = 3)
# WRF_STACK_image <- raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02_April_2015_stack.tif", band = 3)
# plot(WRF_STACK_image)
# 
# # overlay shape of UAE border
# plot(shp_UAE, add=TRUE, lwd=1)
# 
# # WRF-chem resolution
# res(WRF_STACK_image)

############################################################################
# read all WRF  bands in a stack raster ####################################

# WRF_STACK_image <- stack("D:/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02_April_2015_stack.tif")
WRF_STACK_image <- stack("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif")
n <- length(WRF_STACK_image@layers)-1


# generate a time sequence for the WRF-Chem run at intervals of 1 hour (should be 144 images)
start <- as.POSIXct("2015-03-29 00:00:00")
interval <- 60 #minutes
end <- start + as.difftime(6, units="days")
TS <- seq(from=start, by=interval*60, to=end)   # same time series as the AQ data
TS <- TS[1:144]


# average WRF rasters from 11am to 1pm (I will get 3 rasters at the same time of the MODIS/MAIAC data)
# MODIS TERRA: from 10 to 11 am
# MODIS AQUA: from 13 to 14

WRF_2015_03_29_TERRA <- (raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = 11) +
                     raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = 12))/2

plot(WRF_2015_03_29_TERRA)


WRF_2015_03_30_TERRA <- (raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = 35) +
                     raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = 36))/2

plot(WRF_2015_03_30_TERRA)



WRF_2015_03_31_TERRA <- (raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = 59) +
                   raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = 60))/2

plot(WRF_2015_03_31_TERRA)



WRF_2015_04_01_TERRA <- (raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = 83) +
                     raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = 84))/2

plot(WRF_2015_04_01_TERRA)



WRF_2015_04_02_TERRA <- (raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = 107) +
                     raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = 108))/2

plot(WRF_2015_04_02_TERRA)


WRF_2015_04_03_TERRA <- (raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = 131) +
                     raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = 132))/2

plot(WRF_2015_04_03_TERRA)


# MODIS AQUA: from 13 to 14 ##############################################################################################################

WRF_2015_03_29_AQUA <- (raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = 14) +
                           raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = 15))/2

plot(WRF_2015_03_29_AQUA)



WRF_2015_03_30_AQUA <- (raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = 38) +
                           raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = 39))/2

plot(WRF_2015_03_30_AQUA)



WRF_2015_03_31_AQUA <- (raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = 62) +
                           raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = 63))/2

plot(WRF_2015_03_31_AQUA)




WRF_2015_04_01_AQUA <- (raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = 86) +
                           raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = 87))/2

plot(WRF_2015_04_01_AQUA)




WRF_2015_04_02_AQUA <- (raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = 110) +
                           raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = 111))/2

plot(WRF_2015_04_02_AQUA)



WRF_2015_04_03_AQUA <- (raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = 134) +
                           raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = 135))/2

plot(WRF_2015_04_03_AQUA)



# WRF_2015_03_29 <- (raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = 12) +
#                      raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = 13) +
#                      raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = 14))/3
# 
# plot(WRF_2015_03_29)
# 
# 
# 
# WRF_2015_03_30 <- (raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = 36) +
#                      raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = 37) +
#                      raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = 38))/3
# 
# plot(WRF_2015_03_30)
# 
# 
# 
# WRF_2015_03_31 <- (raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = 60) +
#                      raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = 61) +
#                      raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = 62))/3
# 
# plot(WRF_2015_03_31)
# 
# 
# 
# WRF_2015_04_01 <- (raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = 84) +
#                      raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = 85) +
#                      raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = 86))/3
# 
# plot(WRF_2015_04_01)
# 
# 
# 
# WRF_2015_04_02 <- (raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = 108) +
#                      raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = 109) +
#                      raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = 110))/3
# 
# plot(WRF_2015_04_02)
# 
# 
# WRF_2015_04_03 <- (raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = 132) +
#                      raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = 133) +
#                      raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = 134))/3
# 
# plot(WRF_2015_04_03)



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

# make a spatial correlation between all/or/each raster

### correlation 
# make a combine rasters of 6 + 6 = 12 layers
z <- stack(all_rasters_WRF,
           all_rasters_MODIS)
length(z@layers) 

res(z)


setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/")

# spatial correlations of all the layers (first 12 layes are from WRF, the second 12 layers are from MODIS)
r <- calc(z, fun=function(x) cor(x[1:12], x[13:24], method='pearson', use= "pairwise.complete.obs"))
plot(r)
# r <- calc(z, fun=function(x) cor(x[1:3], x[4:6], use= "pairwise.complete.obs"))

plot(r)
# overlay shape of UAE border
plot(shp_UAE, add=TRUE, lwd=1)




jpeg('Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/hist_MAIC_WRF.jpg',   
     quality = 100, bg = "white", res = 200, width = 5, height = 5, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

hist((r), breaks = 10)

par(oldpar)
dev.off()




jpeg('Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs//box_MAIC_WRF.jpg',   
     quality = 100, bg = "white", res = 200, width = 5, height = 5, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


boxplot(r)

par(oldpar)
dev.off()


getwd()
writeRaster(r, "Correlation_WRFCHem_MAIAC_02April2015_stack_6_DAYS_LARGE.tif" , options= "INTERLEAVE=BAND", overwrite=T)


#### make correlation using points from raster

# WRF <- values(all_rasters_WRF)
# MODIS<- values(all_rasters_MODIS)
# 
# data_cor<- data.frame()
# 
# for (i in 1:nrow(WRF)){
#   z <-cor(WRF[i,],MODIS[i,],   use= "pairwise.complete.obs")
#   data_cor<- rbind(data_cor,z)
#   
# }
# 
# 
# # get latitude and longitude from one sample raster--------------------
# sample <- rasterToPoints(all_rasters_MODIS[[1]])
# colnames(sample)[3] <- "value"
# sample<- cbind(sample[,-3],data_cor)
# rr<- rasterFromXYZ(sample)
# plot(rr)
# hist((rr),breaks= 100)
# plot(rr-r) ### this difference shoudl be 0
# 
# # overlay shape of UAE border
# plot(shp_UAE, add=TRUE, lwd=1)

###############################################################
# map spatial correlation map ##################################



setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs")
r <- raster("Correlation_WRFCHem_MAIAC_02April2015_stack_6_DAYS_LARGE.tif")
plot(r)

# r <- raster("Correlation_WRFCHem_MAIAC_02April2015_stack_6_DAYS_LARGE.tif")
# 
# # reduce the size of the raster
# extent(r)
# raster("MAIAC_DUST_event_02_April_2015.tif")
# extent(r)
# 
# # Load d01 WRF domain
# dir <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRFChem_domain"
# ### shapefile for UAE
# shp_d01 <- readOGR(dsn = dir, layer = "domain_d01_WRFChem")
# projection(shp_d01) <- CRS("+proj=longlat +datum=WGS84")
# 
# # crop 
# r <- crop(r, extent(shp_d01))
# r <- mask(r, shp_d01)
# 
# extent(r)



MIN_PAL <- -1
MAX_PAL <- +1

# pal <- colorNumeric(c("#9999FF", "#FFFF00", "#FF0000", "#ff8000"),
#                     c(MIN_PAL, MAX_PAL),na.color = "transparent")


pal <- colorNumeric(c("#0000ff", "#ffffff", "#ff0000"),
                    c(MIN_PAL, MAX_PAL), na.color = "#ffff00")



map <- leaflet() %>%
  setView(46, 26, 6) %>%
  addTiles(group = "OSM (default)") %>%
  addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
  
  addRasterImage(r, colors = pal, opacity = 0.4,
                 group = "correlation", maxBytes = 32 * 1024 * 1024) %>%
 
  addLegend("bottomright", pal = pal, values = c(MIN_PAL, MAX_PAL), 
            title = "<br><strong>R MAIAC-WRFChem: </strong>",
            labFormat = labelFormat(prefix = ""),
            opacity = 0.4) %>%
  
  addLayersControl(
    baseGroups = c("Toner Lite", "Road map", "Satellite"),
    overlayGroups = c("correlation"),
    options = layersControlOptions(collapsed = TRUE))
#  hideGroup(c("March_29_2015")) 

map


saveWidget(map, paste0("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/correlation_WRF_MODIS_MAIAC.html"), selfcontained = FALSE)
webshot("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/correlation_WRF_MODIS_MAIAC.html",
        file = "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/correlation_WRF_MODIS_MAIAC.jpg", vwidth = 1100, vheight = 1100,
        cliprect = 'viewport')



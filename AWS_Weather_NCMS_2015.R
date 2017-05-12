
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

setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER")
# setwd("D:/Dust_Event_UAE_2015/AWS_2015 WEATHER")


#### importing the UAE shapefile to use as extent ####################################

dir <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/website_MODIS/UAE_boundary"
### shapefile for UAE
shp_UAE <- readOGR(dsn = dir, layer = "uae_emirates")

# ----- Transform to EPSG 4326 - WGS84 (required)
shp_UAE <- spTransform(shp_UAE, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
# names(shp)

plot(shp_UAE)

######################################################################################
######################################################################################
# list met files #####################################################################
######################################################################################

path <- ".csv$"
filenames <- list.files(pattern = path)

# subset data from 29 March to 4 April 2015 (DUST event)
# make hourly averages of all the variables


# remove rows with NA from the DateTime column
# DB_NCMS <- DB_NCMS[!(is.na(DB_NCMS$DateTime)), ]

# filenames <- filenames[4]
# i <- 35

All_AWS_data <- NULL

for (i in 1:length(filenames)) {
  
  station <- str_sub(filenames[i], start = 1, end = -10)
  AWS_data <- read.csv(filenames[i])
  AWS_data <- AWS_data %>%
    mutate(DateTime = mdy_hm(Date.UTC.4.))
  
  # names(AWS_data)
  # [1] "Station"                  "Date.UTC.4."              "Wind.Dir...."            
  # [4] "Wind.Speed..m.s."         "Temp.Dry...C."            "Temp.DewPoint...C."      
  # [7] "Vapour.Press..hPa."       "RelHumidity...."          "Radiation.Global..Wh.m2."
  # [10] "Press.QFF..hPa."          "Prec..mm."                "DateTime"                
  # [13] "date"                     "hour" 
           
  AWS_data <- AWS_data %>%
    mutate(date = date(DateTime),
           hour = hour(DateTime),
           station = station)
  
  
  # filter data between 29 March and 4 April 2015
  AWS_data <- AWS_data %>%
    filter(date >= "2015-03-29" & date <= "2015-04-04")  

   # make hourly average 
   AWS_data <- AWS_data %>%
    group_by(date,
             hour,
             station) %>%
    dplyr::summarise(wind_direction = mean(Wind.Dir...., na.rm = TRUE),
                     wind_speed = mean(Wind.Speed..m.s., na.rm = TRUE),
                     RH = mean(RelHumidity...., na.rm = TRUE),
                     Radiation = mean(Radiation.Global..Wh.m2., na.rm = TRUE),
                     T_dry = mean(Temp.Dry...C., na.rm = TRUE),
                     T_dew = mean(Temp.DewPoint...C., nam.rm = TRUE))
   
   All_AWS_data <- rbind(All_AWS_data, AWS_data)
   
}

write_csv(All_AWS_data, "dust_event_outputs/AWS_concatenated_DUST_2_April_2015.csv")

#############################################################################################
##### create single files for each time stamp ###############################################
#############################################################################################

setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/")
setwd("D:/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs")
All_AWS_data <- read_csv("AWS_concatenated_DUST_2_April_2015.csv")

# rebuild DateTime variable
 All_AWS_data$DateTime <- paste0(All_AWS_data$date, " ", All_AWS_data$hour, "_00", " ", "UTC")


# load coordinates of the monitoring stations:

STATIONS_COORDS <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/stations/stations_clean_FK.csv") 
colnames(STATIONS_COORDS) <- c("station", "latitude", "longitude")

# join coordinated of the the station with the total dataset
All_AWS_data <- All_AWS_data %>%
  left_join(STATIONS_COORDS, by = c("station"))


# missing lat and lof of Jabal Jais, Jabal Hafet, Yasat

# list DateTime
DateHour <- All_AWS_data[!duplicated(All_AWS_data[c("DateTime")]),]
DateHour <- as.list(DateHour[,10])
DateHour <- unlist(DateHour)

# list stations
STATIONS_NAMES <- All_AWS_data[!duplicated(All_AWS_data[c("station")]),]
STATIONS_NAMES <- as.list(STATIONS_NAMES[,3])
STATIONS_NAMES <- unlist(STATIONS_NAMES)

# generate a time sequence for the WRF-Chem run at intervals of 1 hour (should be 168 images, 7 DAYS)
start <- as.POSIXct("2015-03-29 00:00:00")
interval <- 60 #minutes
end <- start + as.difftime(7, units="days")
TS <- seq(from=start, by=interval*60, to=end)   # same time series as the AQ data
TS <- TS[1:168]


#  STATIONS_NAMES[1]
#  DateHour[1]
# 

# i <- 3
# j <- 3

setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/hourly_data")
setwd("D:/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/hourly_data")
getwd()

for (i in 1:length(DateHour)) {
  
  hourly_AWS_data <- NULL
  
  for (j in 1:length(STATIONS_NAMES)) {
      name_time <- TS[i]
       AAA <- All_AWS_data %>%
         filter(DateTime == DateHour[i],
                station == as.character(STATIONS_NAMES[j]))
       hourly_AWS_data <- rbind(hourly_AWS_data, AAA)
       write.csv(hourly_AWS_data, paste0(str_sub(name_time, start = 1, end = -10), "_",
                                         str_sub(name_time, start = 12, end = -7), "_",
                                         str_sub(name_time, start = 15, end = -4),
                                         ".csv"))
  }
  }




######################################################################################
######################################################################################
######################################################################################
###### KRIGING FUNCTION ##############################################################
######################################################################################
######################################################################################
######################################################################################

library(readr)
library(sp)
library(raster)
library(gstat)
library(rgdal)
library(RNetCDF)
library(ncdf4)
library(stringr)

setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/hourly_data")
setwd("D:/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/hourly_data")

path <- ".csv$"
filenames_hourly_NCMS <- list.files(pattern = path)


# order filenames by UTC time
# filenames_hourly_NCMS <- c(filenames_hourly_NCMS[1:2],  filenames_hourly_NCMS[13],  filenames_hourly_NCMS[18:24], filenames_hourly_NCMS[3:12],  filenames_hourly_NCMS[14:17],
#           # +24
#            filenames_hourly_NCMS[25:26],filenames_hourly_NCMS[37],filenames_hourly_NCMS[42:48], filenames_hourly_NCMS[27:36], filenames_hourly_NCMS[38:41],
#           # + 48
#  filenames_hourly_NCMS[49:50],  filenames_hourly_NCMS[61],  filenames_hourly_NCMS[66:72], filenames_hourly_NCMS[51:60],  filenames_hourly_NCMS[62:65],
#           # + 72
#  filenames_hourly_NCMS[73:74],  filenames_hourly_NCMS[85],  filenames_hourly_NCMS[90:96], filenames_hourly_NCMS[75:84],  filenames_hourly_NCMS[86:89],
#          # + 96
#  filenames_hourly_NCMS[97:98],  filenames_hourly_NCMS[109],  filenames_hourly_NCMS[114:120], filenames_hourly_NCMS[99:108],  filenames_hourly_NCMS[110:113],
#           # + 120
#  filenames_hourly_NCMS[121:122],  filenames_hourly_NCMS[133],  filenames_hourly_NCMS[138:144], filenames_hourly_NCMS[123:132],  filenames_hourly_NCMS[134:137],
#         # + 144
#  filenames_hourly_NCMS[145:146],  filenames_hourly_NCMS[157],  filenames_hourly_NCMS[162:168], filenames_hourly_NCMS[147:156],  filenames_hourly_NCMS[158:161])

# filenames_hourly_NCMS <- filenames_hourly_NCMS[2] 

############################## 
#### KRIGING function ########
##############################

# i <- 4

# initiaite raster stack for temperature----------------

# kriging_points <- function(filenames_hourly_NCMS, resl_ras= 0.1, shp_UAE = "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/website_MODIS/UAE_boundary"){
  kriging_points <- function(filenames_hourly_NCMS, resl_ras= 0.01, shp_UAE = "D:/website_MODIS/UAE_boundary"){
    
    all_rasters <- stack()  
    
  name <- str_sub(filenames_hourly_NCMS, start = 1, end = -5)
  federico_AWS <- read.csv(filenames_hourly_NCMS)
#  federico_AWS <- read.csv(filenames_hourly_NCMS[i])

  # remove rows with NA from the column of lat/or long
  federico_AWS <- federico_AWS[!(is.na(federico_AWS$latitude)), ]
  federico_AWS <- federico_AWS[!(is.na(federico_AWS$longitude)), ]
  
  
  #masking layer or shapefile
  if (is.character(shp_UAE)) {
    setwd(shp_UAE)
    dir <- shp_UAE
    shp_UAE <- readOGR(dsn = dir, layer = "uae_emirates")
#    setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/hourly_data")
    setwd("D:/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/hourly_data")
    }
  
  limit_x_y <-  extent(shp_UAE)
  
  federico_AWS$x <- federico_AWS$longitude
  federico_AWS$y <- federico_AWS$latitude
  
  coordinates(federico_AWS) = ~ x + y  ## Set spatial coordinates to create a Spatial object:
  
   plot(federico_AWS)
   # overlay shape of UAE border
   plot(shp_UAE, add=TRUE, lwd=1)
  
  
  ## make a variogram----------------------------------------------------------------
  
  vargram_T_dry <- variogram(T_dry ~ 1, federico_AWS) # calculates sample variogram values for the dry temperature
  nn <- floor(length(vargram_T_dry$gamma)/2)
  var_for_fit<- mean(vargram_T_dry[nn:nrow(vargram_T_dry),3])
  
  
  # fit the variogram
  vargram_T_dry_fit  <- fit.variogram(vargram_T_dry, fit.ranges = FALSE, fit.sills = FALSE,
                                    vgm(var_for_fit, "Sph"), fit.kappa = TRUE)
  
  

  plot(vargram_T_dry, vargram_T_dry_fit) # plot the sample values, along with the fit model
  
  
  # make a regular empty grid
  x.range <- as.numeric(c(floor(limit_x_y[1]-1),ceiling(limit_x_y[2]+1)))  # min/max longitude of the interpolation area
  y.range <- as.numeric(c(floor(limit_x_y[3]-1),ceiling(limit_x_y[4]+1)))  # min/max latitude of the interpolation area
  
  
  ## grid at 10km resolution
  grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = resl_ras),
                     y = seq(from = y.range[1], to = y.range[2], by = resl_ras))  # expand points to grid
  coordinates(grd) <- ~x + y
  gridded(grd) <- TRUE
  
  plot(grd, cex = 1.5, col = "grey")
  points(federico_AWS, pch = 1, col = "red", cex = 1)
  
  
  # f.1 <- as.formula(Precip_in ~ X + Y)
  # perform kriging
    dat.krg <- gstat::krige(T_dry ~ 1, federico_AWS, grd, vargram_T_dry_fit, nmax = 50)
  

  
  r <- raster(dat.krg)
  plot(shp_UAE, add=TRUE, lwd=1)
  projection(r) <- CRS("+proj=longlat +datum=WGS84")
  
  r <- crop(r, extent(shp_UAE))
  r <- mask(r, shp_UAE)
  plot(r)
  
  # stack rsters in the loop----
  all_rasters <- stack(all_rasters,r)
 
  }
  
  # filenames_hourly_NCMS <- filenames_hourly_NCMS[1:3]
  # make kriging of all the files
  BBB <- lapply(filenames_hourly_NCMS, kriging_points)
  
  
  
  # make a large stack raster with all the rasters
  ras_stack <- stack()
  
  
#  jj <- 99
  
  for (jj in 1:length(BBB)){      
    plot(BBB[[jj]])
    ras <- BBB[[jj]]
    ras_stack<- stack(ras_stack,ras)
  }

writeRaster(ras_stack, paste0("D:/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/hourly_data/rasters/Dry_Temperature_NCMS_1km.tif"), overwrite = TRUE)



#################################################################################
##### MAPPING ###################################################################
#################################################################################
#################################################################################
#################################################################################
#################################################################################
#################################################################################
#################################################################################
#################################################################################
#################################################################################
#################################################################################
#################################################################################
##### MAPPING ###################################################################
#################################################################################

setwd("D:/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/hourly_data")
path <- ".csv$"
filenames_hourly_NCMS <- list.files(pattern = path)

setwd("D:/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/hourly_data/rasters")
NCMS_DRY_TEMP_STACK_image <- stack("Dry_Temperature_NCMS_10km.tif")

plot(NCMS_DRY_TEMP_STACK_image[[34]])

# i <- 1
# j <- 1

for (i in 1:length(NCMS_DRY_TEMP_STACK_image@layers)) {
  # load the stacked raster with all the images
  NCMS_STACK_image <- raster("D:/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/hourly_data/rasters/Dry_Temperature_NCMS_10km.tif", band = i)
  plot(NCMS_STACK_image)
  

  name_time <- str_sub(filenames_hourly_NCMS[i], start = 1, end = -5)
  
  min_temp <- 10
  max_temp <- 42
  
  cool = rainbow(50, start=rgb2hsv(col2rgb('cyan'))[1], end=rgb2hsv(col2rgb('blue'))[1])
  warm = rainbow(50, start=rgb2hsv(col2rgb('red'))[1], end=rgb2hsv(col2rgb('yellow'))[1])
  cols = c(rev(cool), rev(warm))
  mypalette <- colorRampPalette(cols)(255)
  
  pal = colorBin(mypalette, domain = 10:42, bins = 11, na.color = "transparent")

  
  # define popup for time scene
  "h1 { font-size: 3px;}"
  content <- paste('<h1><strong>', name_time,'', sep = "")
  
  map <- leaflet() %>% 
    addTiles() %>% 
    addTiles(group = "OSM (default)") %>%
    addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
    addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
    addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
    
    addPopups(53, 25, content,
              options = popupOptions(closeButton = FALSE)) %>%
    
    addRasterImage(NCMS_STACK_image, 
                   colors = pal, 
                   opacity = 0.5, group = "Dry_Temp_NCMS") %>%
    addLayersControl(
      baseGroups = c("Road map", "Toner Lite","Satellite"),
      overlayGroups = "Dry_Temp_NCMS",
      options = layersControlOptions(collapsed = TRUE)) %>%
    addLegend("bottomright", pal = pal, values = c(min_temp, max_temp),
              title = "<br><strong> Dry Temperature (C): </strong>",
              labFormat = labelFormat(prefix = ""),
              opacity = 0.5)
  map
  
  ## This is the png creation part
  saveWidget(map, 'temp.html', selfcontained = FALSE)
  webshot('temp.html', file = paste0(name_time,".png"), vwidth = 1100, vheight = 900,
          cliprect = 'viewport')
  
  } 

# to make a movie.......
# to use with ImageMagik using the commnad line cmd in windows
# cd into the directory where there are the png files
# magick -delay 50 -loop 0 *.png Dry_Temperatue_NCMS_10km_DUST_event_02_April_2015.gif

  


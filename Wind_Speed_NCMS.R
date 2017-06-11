
library(readr)
library(sp)
library(raster)
library(gstat)
library(rgdal)
library(RNetCDF)
library(ncdf4)
library(stringr)
library(htmlwidgets)
library(webshot)
library(leaflet)

setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/hourly_data")
# setwd("D:/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/hourly_data")


vec_all <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/AWS_concatenated_DUST_2_April_2015.csv")
vec_all <- vec_all %>%
  dplyr::filter(hour == 0)

vec_all <- as.vector(vec_all)

max_val<- ceiling(max(vec_all$wind_speed, na.rm = T))
min_val<- floor(min(vec_all$wind_speed,  na.rm = T))


stat_dat <- summary(vec_all$wind_speed)
IQR <- floor(as.numeric((stat_dat[5]-stat_dat[2])* 1.5))# n is the space after IQR

low_IQR <-floor(as.numeric((stat_dat[2]- IQR)))
high_IQR <-floor(as.numeric((stat_dat[5]+IQR)))


vec_all_1 <- vec_all$wind_speed[vec_all$wind_speed >= low_IQR & vec_all$wind_speed <= high_IQR & !is.na(vec_all$wind_speed) ]

xxx <- pretty(vec_all_1, n=15)

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



path <- ".csv$"
filenames_hourly_NCMS <- list.files(pattern = path)


# filenames_hourly_NCMS <- filenames_hourly_NCMS[10] 
# i <- 10

for (i in 1:length(filenames_hourly_NCMS)) {
  
name_time <- str_sub(filenames_hourly_NCMS[i], start = 1, end = -5)
  
tag_time <- paste0(str_sub(name_time, start = 1, end = -7), " ",
                   str_sub(name_time, start = 12, end = -4), ":",
                    str_sub(name_time, start = 15, end = -1))
  
  
  # define popup for time scene
  "h1 { font-size: 3px;}"
  content <- paste('<h1><strong>', tag_time,'', sep = "")

df <- read.csv(filenames_hourly_NCMS[i])
colnames(df)[colnames(df) == 'X'] <- 'id'


coordinates(df) <- c("longitude", "latitude")
proj4string(df) <- CRS("+proj=longlat +datum=WGS84")  ## for example

# df <- spTransform(df, CRS("+proj=utm +zone=51 ellps=WGS84"))
df <- spTransform(df, CRS("+init=epsg:3857"))
df <- as.data.frame(df)

############################################################################################
############################################################################################
###### WIND SPEED ##########################################################################

#Line parameters
line.length <- 20000 #length of polylines representing wind in the map (meters)
arrow.length <- 6000 #lenght of arrowhead leg (meters)
arrow.angle <- 120 #angle of arrowhead leg (degrees azimuth)

#Generate data frame with auxiliary coordinates
end.xy.df <- data.frame(end.x=NA,end.y=NA,end.arrow.x=NA,end.arrow.y=NA)

# i <- 10


for (i in c(1:nrow(df))){
  
  #coordinates of end points for wind lines (the initial points are the ones where data was observed)
  if (df$wind_direction[i] <= 90) {
    end.x <- df$longitude[i] + (cos((90 - df$wind_direction[i]) * 0.0174532925) * line.length)
  } else if (df$wind_direction[i] > 90 & df$wind_direction[i] <= 180) {
    end.x <- df$longitude[i] + (cos((df$wind_direction[i] - 90) * 0.0174532925) * line.length)
  } else if (df$wind_direction[i] > 180 & df$wind_direction[i] <= 270) {
    end.x <- df$longitude[i] - (cos((270 - df$wind_direction[i]) * 0.0174532925) * line.length)
  } else {end.x <- df$longitude[i] - (cos((df$wind_direction[i] - 270) * 0.0174532925) * line.length)}
  
  if (df$wind_direction[i] <= 90) {
    end.y <- df$latitude[i] + (sin((90 - df$wind_direction[i]) * 0.0174532925) * line.length)
  } else if (df$wind_direction[i] > 90 & df$wind_direction[i] <= 180) {
    end.y <- df$latitude[i] - (sin((df$wind_direction[i] - 90) * 0.0174532925) * line.length)
  } else if (df$wind_direction[i] > 180 & df$wind_direction[i] <= 270) {
    end.y <- df$latitude[i] - (sin((270 - df$wind_direction[i]) * 0.0174532925) * line.length)
  } else {end.y <- df$latitude[i] + (sin((df$wind_direction[i] - 270) * 0.0174532925) * line.length)}
  
  #coordinates of end points for arrowhead leg lines (the initial points are the previous end points)
  end.arrow.x <- end.x + (cos((df$wind_direction[i] + arrow.angle) * 0.0174532925) * arrow.length)
  end.arrow.y <- end.y - (sin((df$wind_direction[i] + arrow.angle) * 0.0174532925) * arrow.length)
  
  end.xy.df <- rbind(end.xy.df,c(end.x,end.y,end.arrow.x,end.arrow.y)) 
}

end.xy <- end.xy.df[-1,]
df <- data.frame(df,end.xy) #df with observed and auxiliary variables
head(df,3)

#------------------------------
#Step 3 - Create an object of class `SpatialLinesDataFrame` to use within `leaflet`.

lines <- data.frame(cbind(lng=c(df$longitude, df$end.x, df$end.arrow.x),
                          lat=c(df$latitude, df$end.y, df$end.arrow.y),
                          id=c(rep(df$id,3))))

lines.list <- list()

library(sp)

for (i in c(1:max(lines$id))){
  line <- subset(lines,lines$id==i)
  line <- as.matrix(line[,c(1:2)])
  line <- Line(line) #object of class 'Line'
  lines.list[[i]] <- Lines(list(line), ID = i) #list of 'objects'Lines' 
}

sp.lines <- SpatialLines(lines.list) #object of class 'SpatialLines'
proj4string(sp.lines) <- CRS("+init=epsg:3857") #define CRS

#Convert CRS to geographic coordinates (http://spatialreference.org/ref/epsg/4326/)
#for overlaying on OpenStreetMaps tiles in Leaflet
sp.lines <- spTransform(sp.lines, CRS("+init=epsg:4326"))


rownames(df) = df$id
#Join wind variables (id, speed, direction and date) to object of class 'SpatialLines'
# sp.lines.df <- SpatialLinesDataFrame(sp.lines, df[,c(1,4:6)]) #object of class 'SpatialLinesDataFrame'
sp.lines.df <- SpatialLinesDataFrame(sp.lines, df[,c(1,6:7,2)]) #object of class 'SpatialLinesDataFrame'


#------------------------------
#Step 4 - Generate interactive and **static** map of wind speed and direction.

library(leaflet)

#popup settings
labels <- paste0("ID: ",sp.lines.df@data$id,
                 "<br>Wind speed: ",sp.lines.df@data$wind_speed," m/s<br>",
                 "Wind direction: ",sp.lines.df@data$wind_direction," degrees azimuth<br>",
                 "Date: ", sp.lines.df@data$DateTime)

#pallete settings
# pal <- colorNumeric(palette = colorRampPalette(c("red", "blue"))(5),
#                     domain = 0:max(sp.lines.df@data$wind_speed))

#Create object fo class 'leaflet' 'htmlwidget'
map <- leaflet(sp.lines.df) %>%
  
  addPopups(53, 25, content,
            options = popupOptions(closeButton = FALSE)) %>%
  
  addTiles() %>% 
  addTiles(group = "OSM (default)") %>%
  addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
  
  addPolylines(color = ~ pal(wind_speed), opacity=1, weigh = 3, popup = labels) %>%
  addLegend("bottomright", pal = pal, values = ~ wind_speed,
            title = "Wind speed <br> (m/s)",
            opacity = 1) %>%
  addLayersControl(
    baseGroups = c("Toner Lite" ,"Road map" ,"Satellite"),
    overlayGroups = "WS_WD",
    options = layersControlOptions(collapsed = TRUE)) %>%
 # fitBounds(sp.lines.df@bbox[1,1], sp.lines.df@bbox[2,1], sp.lines.df@bbox[1,2], sp.lines.df@bbox[2,2])
fitBounds(51.48227, 22.71417, 56.34467, 25.97694)

#Plot map
map

saveWidget(map, 'temp.html', selfcontained = FALSE)
webshot('temp.html', file = paste0(name_time,".png"), vwidth = 1100, vheight = 900,
        cliprect = 'viewport')

}

# to make a movie.......
# to use with ImageMagik using the commnad line cmd in windows
# cd into the directory where there are the png files
# magick -delay 50 -loop 0 *.png Wind_Speed_NCMS_1km_DUST_event_02_April_2015.gif




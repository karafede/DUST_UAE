

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
 pal <- colorNumeric(palette = colorRampPalette(c("red", "blue"))(5),
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
plot(sp.lines[5])
#Plot map
map

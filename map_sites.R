
library(raster)
library(leaflet)
library(htmlwidgets)
library(readr)

#load PM10 stations in the UAE
stations_PM10 <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/sites_PM10_DUST_EVENT.csv")
stations_PM10_selected <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/sites_PM10_DUST_EVENT_selected.csv")

# load coordinates of the NCMS monitoring stations:
NCMS_STATIONS_COORDS <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/stations/stations_clean_FK.csv") 
colnames(NCMS_STATIONS_COORDS) <- c("station", "latitude", "longitude")

NCMS_STATIONS_COORDS_selected <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/stations/stations_clean_FK_selected.csv") 
colnames(NCMS_STATIONS_COORDS_selected) <- c("station", "latitude", "longitude")



map <- leaflet() %>%
  addTiles(group = "OSM (default)") %>%
  addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
  addProviderTiles("Thunderforest.Landscape", group = "Topographical") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
  
  # addCircleMarkers(data = stations_PM10,
  # lng = ~ Longitude, lat = ~ Latitude,
  # radius = 5, stroke = FALSE, fillOpacity = 0.5, popup = ~ Site,
  # group = "sites_PM10") %>%
  
  addCircleMarkers(data = NCMS_STATIONS_COORDS_selected,
                   lng = ~ longitude, lat = ~ latitude,
                   radius = 9, stroke = TRUE, fillOpacity = 1, popup = ~ station,
                   color = "red", 
                   group = "sites_NCMS_selected") %>%

  
    addCircleMarkers(data = NCMS_STATIONS_COORDS,
                   lng = ~ longitude, lat = ~ latitude,
                   radius = 4, stroke = FALSE, fillOpacity = 0.5, popup = ~ station,
                   color = "blue",
                   group = "sites_NCMS") %>%
  

  
  
  # addMarkers(data = NCMS_STATIONS_COORDS_selected , lng = ~ longitude, lat = ~ latitude,
  #            popup = ~ station, group = "sites_NCMS_selected") %>%

  addLayersControl(
    baseGroups = c("Toner Lite", "Road map", "Topographical", "Satellite"),
    overlayGroups = c("sites_NCMS_selected",  "sites_NCMS"),
    options = layersControlOptions(collapsed = TRUE)) %>%
  hideGroup(c("sites_NCMS_selected")) 

map



# save map
saveWidget(map, "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/met_stations_NCMS.html", selfcontained = FALSE)


map <- leaflet() %>%
  addTiles(group = "OSM (default)") %>%
  addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
  addProviderTiles("Thunderforest.Landscape", group = "Topographical") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
  
  
  addCircleMarkers(data = stations_PM10_selected,
                   lng = ~ Longitude, lat = ~ Latitude,
                   radius = 9, stroke = TRUE, fillOpacity = 1, popup = ~ Site,
                   color = "black", 
                   group = "sites_PM10_selected") %>%
  
  
  addCircleMarkers(data = stations_PM10,
                   lng = ~ Longitude, lat = ~ Latitude,
                   radius = 4, stroke = FALSE, fillOpacity = 0.5, popup = ~ Site,
                   color = "blue",
                   group = "sites_PM10") %>%
  
  
  addLayersControl(
    baseGroups = c("Toner Lite", "Road map", "Topographical", "Satellite"),
    overlayGroups = c("sites_PM10_selected",  "sites_PM10"),
    options = layersControlOptions(collapsed = TRUE)) %>%
  hideGroup(c("sites_PM10_selected")) 

map

saveWidget(map, paste0("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/Stations_PM10.html"), selfcontained = FALSE)

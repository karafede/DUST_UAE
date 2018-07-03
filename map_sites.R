
library(rgdal)
library(raster)
library(leaflet)
library(htmlwidgets)
library(readr)

#load PM10 stations in the UAE
stations_PM10 <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/sites_PM10_DUST_EVENT.csv")
stations_PM10_selected <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/sites_PM10_DUST_EVENT_selected.csv")
all_UAE_stations <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/all_station_info1.csv")

# load coordinates of the NCMS monitoring stations:
NCMS_STATIONS_COORDS <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/stations/stations_clean_FK.csv") 
colnames(NCMS_STATIONS_COORDS) <- c("station", "latitude", "longitude")

NCMS_STATIONS_COORDS_selected <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/stations/stations_clean_FK_selected.csv") 
colnames(NCMS_STATIONS_COORDS_selected) <- c("station", "latitude", "longitude")

Masdar_Station <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/Masdar_Site.csv")
colnames(Masdar_Station) <- c("station", "latitude", "longitude")



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

map <- leaflet(shp_UAE) %>%
  addTiles(group = "OSM (default)") %>%
  addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
  addProviderTiles("Thunderforest.Landscape", group = "Topographical") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
  
  # addCircleMarkers(data = stations_PM10,
  # lng = ~ Longitude, lat = ~ Latitude,
  # radius = 5, stroke = FALSE, fillOpacity = 0.5, popup = ~ Site,
  # group = "sites_PM10") %>%
  
  addPolygons(stroke = TRUE, smoothFactor = 1, fillOpacity = 0,
              weight = 0.7, color = "#000000",
              group = "shp_UAE") %>%
  
  addCircleMarkers(data = NCMS_STATIONS_COORDS_selected,
                   lng = ~ longitude, lat = ~ latitude,
                   radius = 9, stroke = FALSE, fillOpacity = 1, popup = ~ station,
                   color = "red", 
                   group = "sites_NCMS_selected") %>%

  
    # addCircleMarkers(data = NCMS_STATIONS_COORDS,
    #                lng = ~ longitude, lat = ~ latitude,
    #                radius = 4, stroke = FALSE, fillOpacity = 0.5, popup = ~ station,
    #                color = "blue",
    #                group = "sites_NCMS") %>%
  
  addCircleMarkers(data = Masdar_Station,
                   lng = ~ longitude, lat = ~ latitude,
                   radius = 9, stroke = FALSE, fillOpacity = 1, popup = ~ station,
                   color = "blue",
                   group = "Masdar_Site") %>%
  

  
  
  # addMarkers(data = NCMS_STATIONS_COORDS_selected , lng = ~ longitude, lat = ~ latitude,
  #            popup = ~ station, group = "sites_NCMS_selected") %>%

  addLayersControl(
    baseGroups = c("Toner Lite", "Road map", "Topographical", "Satellite"),
    overlayGroups = c("sites_NCMS_selected",  "sites_NCMS"),
    options = layersControlOptions(collapsed = TRUE)) # %>%
 # hideGroup(c("sites_NCMS_selected")) 

map



# save map
saveWidget(map, "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/met_stations_NCMS.html", selfcontained = FALSE)


##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################

popup <- paste0("<strong><i>",
                stations_PM10$Site,
                "</i></strong><br>Hourly PM<sub>10</sub>: <strong> ", round(stations_PM10$Value, digits = 2), " </strong>(<font face=symbol>m</font>g/m<sup>3</sup>)")


map <- leaflet() %>%
  addTiles(group = "OSM (default)") %>%
  addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
  addProviderTiles("Thunderforest.Landscape", group = "Topographical") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
  
  
  addCircleMarkers(data = stations_PM10_selected,
                   lng = ~ Longitude, lat = ~ Latitude,
                   radius = 9, stroke = TRUE, fillOpacity = 1, popup = stations_PM10_selected$Site,
                   color = "black", 
                   group = "sites_PM10_selected") %>%
  
  
  addCircleMarkers(data = stations_PM10,
                   lng = ~ Longitude, lat = ~ Latitude,
                   radius = 4, stroke = FALSE, fillOpacity = 0.5, popup = popup,
                   color = "blue",
                   group = "sites_PM10") %>%
  
  
  addLayersControl(
    baseGroups = c("Toner Lite", "Road map", "Topographical", "Satellite"),
    overlayGroups = c("sites_PM10_selected",  "sites_PM10"),
    options = layersControlOptions(collapsed = TRUE)) %>%
  hideGroup(c("sites_PM10_selected")) 

map

saveWidget(map, paste0("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/Stations_PM10.html"), selfcontained = FALSE)


#####################################################################
#####################################################################
######### all stations ##############################################
#####################################################################
#####################################################################

map <- leaflet() %>%
  addTiles(group = "OSM (default)") %>%
  addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
  addProviderTiles("Thunderforest.Landscape", group = "Topographical") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
 
  addCircleMarkers(data = all_UAE_stations,
                   lng = ~ Longitude, lat = ~ Latitude,
                   radius = 4, stroke = FALSE, fillOpacity = 0.5, popup = popup,
                   color = "blue",
                   group = "sites_PM10") %>%
  addLayersControl(
    baseGroups = c("Toner Lite", "Road map", "Topographical", "Satellite"),
    overlayGroups = c("sites_PM10_selected",  "sites_PM10"),
    options = layersControlOptions(collapsed = TRUE)) %>%
  hideGroup(c("sites_PM10_selected")) 

map

saveWidget(map, paste0("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/ALL_Stations_UAE.html"), selfcontained = FALSE)



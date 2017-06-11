library(sp)
library(maptools)

# points_to_line <- function(data, long, lat, id_field = NULL, sort_field = NULL) {
#   
#   # Convert to SpatialPointsDataFrame
#   coordinates(data) <- c(long, lat)
#   
#   # If there is a sort field...
#   if (!is.null(sort_field)) {
#     if (!is.null(id_field)) {
#       data <- data[order(data[[id_field]], data[[sort_field]]), ]
#     } else {
#       data <- data[order(data[[sort_field]]), ]
#     }
#   }
#   
#   # If there is only one path...
#   if (is.null(id_field)) {
#     
#     lines <- SpatialLines(list(Lines(list(Line(data)), "id")))
#     
#     return(lines)
#     
#     # Now, if we have multiple lines...
#   } else if (!is.null(id_field)) {  
#     
#     # Split into a list by ID field
#     paths <- sp::split(data, data[[id_field]])
#     
#     sp_lines <- SpatialLines(list(Lines(list(Line(paths[[1]])), "line1")))
#     
#     # I like for loops, what can I say...
#     for (p in 2:length(paths)) {
#       id <- paste0("line", as.character(p))
#       l <- SpatialLines(list(Lines(list(Line(paths[[p]])), id)))
#       sp_lines <- spRbind(sp_lines, l)
#     }
#     
#     return(sp_lines)
#   }
# }
# 
# 
# Fed<- points_to_line(xy, long= xy$lon, lat=xy$lat, id_field = xy$ID, sort_field = xy$ID_seq)



data_abudhabi<- data_abudhabi %>%
  mutate(ID= (yday(date)+ (hour(date)/24)))%>%
  mutate(ID_seq= hour.inc+73)

coordinates(data_abudhabi) <- c("lon", "lat")


paths <- sp::split(data_abudhabi, data_abudhabi[["ID"]])

sp_lines <- SpatialLines(list(Lines(list(Line(paths[[1]])), as.character(data_abudhabi$ID[0*73+1]))))

for (jj in 2:49){
  kk<- jj-1
  
l <- SpatialLines(list(Lines(list(Line(paths[[jj]])), as.character(data_abudhabi$ID[kk*73+1]))))
sp_lines <- spRbind(sp_lines, l)
}





#########################################################################################

# make a quick leaflet map with site loaction only


# MAX_speed <- max(traffic$averagespeed)+0.5  
# MIN_speed <- min(traffic$averagespeed)-0.5

MAX_speed <- max(traffic$freeflowspeed)+0.5  
MIN_speed <- min(traffic$freeflowspeed)-0.5

pal_speed <- colorNumeric(c("#0000ff", "#ff0000"),
                          c(MIN_speed, MAX_speed),na.color = "transparent")

# define popup for the iteractive map
# popup_speed <- paste0("<p>Avg Speed", ": <strong> ", traffic$averagespeed, " </strong>km/h")
popup_speed <- paste0("<p>Flow Speed", ": <strong> ", traffic$freeflowspeed, " </strong>(km/h)")


# Marker + lines + Static Label using custom label options
map <- leaflet(data = traffic[,]) %>%
  #  setView(-5.355, 36.150, 16) %>%
  addProviderTiles("Hydda.Full", group = "Hydda_Full") %>%
  addProviderTiles("Thunderforest.Landscape", group = "Topographical") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
  addCircleMarkers(lng = ~lon, lat = ~lat, 
                   # weight = 1, radius=5, color = 'black',
                   # stroke = FALSE, fillOpacity = 1,
                   popup = popup_speed,
                   weight = 3, radius= 4, 
                   color = pal_speed(traffic$freeflowspeed), stroke = FALSE, fillOpacity = 1,
                   label = ~as.character(traffic$informationstatus),
                   #  label = ~as.character( format(round(traffic$ID, 2), nsmall = 2) ),
                   labelOptions = labelOptions(noHide = F),
                   group = "Traffic Flow") %>%
  addPolylines(data = OSM_GIB , color='blue', group='OSM', weight = 1) %>%
  addPolylines(data = gI, color="blue", group='intersects', weight = 2,
               label = as.character(traffic$informationstatus),
               labelOptions = labelOptions(noHide = F)) %>%
  addLegend(
    "bottomright", pal = pal_speed, values = c(MIN_speed, MAX_speed),
    title = paste("<strong>Flow Speed Km/h:"),
    labFormat = labelFormat(prefix = ""), labels = "black", opacity = 1) %>%
  addLayersControl(
    baseGroups = c("Hydda_Full", "Topographical", "Satellite", "Toner Lite"),
    overlayGroups = c("OSM", "Traffic Flow", "intersects"),
    options = layersControlOptions(collapsed = TRUE)) %>%
  hideGroup(c("OSM","intersects"))

map

saveWidget(map, 'TomTom_14_15_June_Traffic_Flow_free_speed.html', selfcontained = FALSE)

library(leaflet)

cool = rainbow(25, start=rgb2hsv(col2rgb('#00ffff'))[1], end=rgb2hsv(col2rgb('#0000ff'))[1])
warm = rainbow(24, start=rgb2hsv(col2rgb('red'))[1], end=rgb2hsv(col2rgb('yellow'))[1])
cols = c(rev(cool), rev(warm))

daw<- rev(terrain.colors(49))

pal = colorBin(daw, na.color = "transparent")
mypalette <- colorRampPalette(cols)(49)

map <- leaflet(data = data_abudhabi) %>%
  #  setView(-5.355, 36.150, 16) %>%
  addProviderTiles("Hydda.Full", group = "Hydda_Full") %>%
  addProviderTiles("Thunderforest.Landscape", group = "Topographical") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
  addPolylines(data = sp_lines[1], color= "blue", weight = 1, group='HYSPLIT') %>%
  # addLegend(
  #   "bottomright", pal = pal_speed, values = c(MIN_speed, MAX_speed),
  #   title = paste("<strong>Flow Speed Km/h:"),
  #   labFormat = labelFormat(prefix = ""), labels = "black", opacity = 1) %>%
  addLayersControl(
    baseGroups = c("Toner Lite", "Hydda_Full", "Topographical", "Satellite"),
    overlayGroups = c("HYSPLIT"),
    options = layersControlOptions(collapsed = TRUE))


for(kk in (2:49)) {
  
  map_2<- leaflet(data = data_abudhabi)%>%
    addPolylines(data = sp_lines[kk], color="red", weight = 1, group='HYSPLIT')
  
  
  map$x$calls[[5+kk]]<-map_2$x$calls[[1]]
      
}
  
plot()
  
yyy<- map_2$x$calls[[1]]

yyy$args[[1]]$lng[1] $color

  str(map_2)
  
  str(sp_lines)
  
library(raster)
cols

plot(sp_lines[])




yday(as.Date("2009-01-01"))
hour()






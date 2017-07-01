
library(sp)
library(maptools)
library(leaflet)
library(htmlwidgets)
library(webshot)

data_abudhabi<- data_abudhabi %>%
  mutate(ID= floor(((yday(date)+ (hour(date)/24)))*100000))%>%
  mutate(ID_seq= hour.inc+73)


data_abu<- data_abudhabi

coordinates(data_abu) <- c("lon", "lat")


paths <- sp::split(data_abu, data_abu[["ID"]])

sp_lines <- SpatialLines(list(Lines(list(Line(paths[[1]])), as.character(data_abu$ID[0*73+1]))))

for (jj in 2:25){
  kk<- jj-1
  
l <- SpatialLines(list(Lines(list(Line(paths[[jj]])), as.character(data_abu$ID[kk*73+1]))))
sp_lines <- spRbind(sp_lines, l)
}



############# map ###############################################################################
#################################################################################################

col_names<- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/colorbar.csv")


map <- leaflet(data = data_abudhabi) %>%
  setView(53, 23, 5) %>%
  addProviderTiles("Hydda.Full", group = "Hydda_Full") %>%
  addProviderTiles("Thunderforest.Landscape", group = "Topographical") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
  addPolylines(data = sp_lines[1], color= col_names$R_colors[1], weight = 2, group='HYSPLIT', opacity = 1, fillOpacity= 1) %>%
  # addLegend(
  #   "bottomright", bins = 8, colors= c(as.vector(col_names$R_colors)[3],
  #                                      as.vector(col_names$R_colors)[17],
  #                                      as.vector(col_names$R_colors)[25],
  #                                      as.vector(col_names$R_colors)[39],
  #                                      as.vector(col_names$R_colors)[43],
  #                                      as.vector(col_names$R_colors)[47]),
  #   title = paste("<strong>24 hours runs"),
  #   labFormat = labelFormat(prefix = ""), labels = c("2015-04-01 01:00","2015-04-01 09:00", "2015-04-01 13:00",
  #                                                    "2015-04-01 18:00", "2015-04-01 20:00" ,"2015-04-01 23:00"), opacity = 1) %>%
  # # labFormat = labelFormat(prefix = ""), labels = c("2015-04-02 01:00","2015-04-02 09:00", "2015-04-02 13:00",
  # #                                                  "2015-04-02 18:00", "2015-04-02 20:00" ,"2015-04-02 23:00"), opacity = 1) %>%
  addLayersControl(
    baseGroups = c("Toner Lite", "Hydda_Full", "Topographical", "Satellite"),
    overlayGroups = c("HYSPLIT"),
    options = layersControlOptions(collapsed = TRUE))
map

# updated for one day

qqq <- seq(from=1, to= 49, by =2)
 for(kk in (2:25)) {
#  for(kk in (2:49)) {
  qq <- qqq[(kk)]
 map_2<- leaflet(data = data_abudhabi)%>%
    addPolylines(data = sp_lines[kk], color="red", weight = 3, group='HYSPLIT', opacity = 1, fillOpacity= 1)
  
  
  map$x$calls[[5+kk]]<-map_2$x$calls[[1]]
  map$x$calls[[5+kk]]$args[[4]]$color<- col_names$R_colors[qq] 
}

map


# save data ####

setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/hysplit")

saveWidget(map, 'temp.html', selfcontained = FALSE)
webshot('temp.html', file = "Abu_Dhabi_500m_2April_2015.png", vwidth = 900, vheight = 900,
        cliprect = 'viewport')

#########################################################################################

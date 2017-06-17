
library(raster)
library(rgdal)
library(leaflet)
library(sp)
library(dplyr)
library(htmlwidgets)


setwd("Z:/_SHARED_FOLDERS/CONFERENCES AND PRESENTATIONS/Federico")


moisture <- raster("Salinity1.tif")
plot(moisture)
res(moisture)

moisture_pt <-  rasterToPoints(moisture)
moisture_pt <- as.data.frame(moisture_pt)

ext <- extent(moisture_pt)
xmn = min(ext@xmin)
xmx = max(ext@xmax)
ymn = min(ext@ymin)
ymx = max(ext@ymax)

# crs_moist <- projection(moisture) 

salinity <- raster("salinity.tif")
plot(salinity)
res(salinity)


salinity_pt <-  rasterToPoints(salinity)
salinity_pt <- as.data.frame(salinity_pt)



d <- data.frame(lon=salinity_pt$x, lat=salinity_pt$y)
coordinates(d) <- c("lon", "lat")
proj4string(d) <- CRS("+init=epsg:4326") # WGS 84
CRS.new <- CRS("+proj=tmerc +lat_0=0 +lon_0=54 +k=0.99995 +x_0=500000 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
new_coords <- spTransform(d, CRS.new)
new_coords <- as.data.frame(new_coords)

salinity_pt <- cbind(salinity_pt, new_coords[, 1:2])

salinity_pt <- salinity_pt %>%
  select(lon,
         lat,
         Salinity)


# convert back dataframe into raster
colnames(salinity_pt) <- c('x', 'y', 'z')


xmn = min(salinity_pt$x)
xmx = max(salinity_pt$x)
ymn = min(salinity_pt$y)
ymx = max(salinity_pt$y)


x.range <- as.numeric(c(xmn,xmx ))
y.range <- as.numeric(c(ymn,ymx))

# x.range <- as.numeric(c(253887.5,705181.8 ))
# y.range <- as.numeric(c(2505415,2756329))

grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by =  105.5763),
                   y = seq(from = y.range[1], to = y.range[2], by =  105.5763))  # expand points to grid


# grd_1 <- dplyr::filter(grd, grd$x == 253887.5)
# nrow(grd_1)
# grd_2<- dplyr::filter(grd, grd$y == 2505415)
# nrow(grd_2)

grd_1 <- dplyr::filter(grd, grd$x == xmn)
nrow(grd_1)
grd_2<- dplyr::filter(grd, grd$y == ymn)
nrow(grd_2)


r <- raster(xmn=min(salinity_pt$x), xmx=max(salinity_pt$x), ymn=min(salinity_pt$y),
            ymx=max(salinity_pt$y), ncol=nrow(grd_2), nrow= nrow(grd_1))


raster_salinity <- rasterize(salinity_pt[, 1:2], r, salinity_pt[,3], fun=mean)
res(raster_salinity)


projection(raster_salinity) <- CRS("+proj=tmerc +lat_0=0 +lon_0=54 +k=0.99995 +x_0=500000 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

plot(raster_salinity)
plot(moisture)


writeRaster(raster_salinity, "Z:/_SHARED_FOLDERS/CONFERENCES AND PRESENTATIONS/Federico/salinity_meters.tif", overwrite = T)
writeRaster(moisture, "Z:/_SHARED_FOLDERS/CONFERENCES AND PRESENTATIONS/Federico/moisture_meters.tif", overwrite = TRUE)

### END #########
##################




##################
###### map #######


MIN_PAL_sal <- 1
MAX_PAL_sal <- 7

pal_sal <- colorNumeric(c("#9999FF", "#FFFF00", "#FF0000", "#ff8000"),
                    c(MIN_PAL_sal, MAX_PAL_sal),na.color = "transparent")


MIN_PAL_mois <- 1
MAX_PAL_mois <- 2.5

pal_mois <- colorNumeric(c("#9999FF", "#FFFF00", "#FF0000", "#ff8000"),
                        c(MIN_PAL_mois, MAX_PAL_mois),na.color = "transparent")




map <- leaflet() %>%
  addTiles(group = "OSM (default)") %>%
  addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
  addProviderTiles("Thunderforest.Landscape", group = "Topographical") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
  
 
  addRasterImage(raster_salinity, colors = pal_sal, opacity = 0.5,
                 group = "salinity") %>%

  addRasterImage(moisture, colors = pal_mois, opacity = 0.5,
                 group = "moisture") %>%
  

  # addLegend("bottomright", pal = pal, values = c(MIN_PAL, MAX_PAL), # values = getValues(BIAS), , #  values = getValues(R2_regression), # ,  , 
  #           title = "<br><strong>PM2.5</strong>",
  #           #  title = "<br><strong>AOD</strong>",
  #           labFormat = labelFormat(prefix = ""),
  #           opacity = 0.6) %>%
  
  addLayersControl(
    baseGroups = c("Toner Lite", "Road map", "Topographical", "Satellite"),
    overlayGroups = c("moisture", "salinity"),
    options = layersControlOptions(collapsed = TRUE)) %>%
  hideGroup(c("salinity")) 

map



# save map
saveWidget(map, paste0("Amal_maps.html"), selfcontained = FALSE)

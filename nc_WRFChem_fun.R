
library(RNetCDF)
library(rgdal)
library(ncdf4)
library(raster)
library(stringr)


# list .nc files
setwd("D:/Dust_Event_UAE_2015/WRF_trial_runs")
setwd("E:/MASDAR_FK/Air Quality/Phase 2/WRF_Chem/DUST_ADO_FK")
patt<- ".nc"
filenames <- list.files(pattern = patt)
filenames <- filenames

# inizialise an empty raster to stack ALL HOURS together in an unique raster 

 import_nc_WRF <- function(filenames){

  WRF_file <- open.nc(filenames)
  WRF_file <- read.nc(WRF_file)
  name_vari <- names(WRF_file)
  name <- str_sub(filenames, start = 12, end = -4)
  
  ######
  #### looping the variables of the nc files
  # select only variables DUST_1, DUST_2, DUST_3, DUST_4, DUST_5, DUST_6 

  
 # j = 152
  
  #### only one variable (DUST_5) == var = 156
  
  # for(j in 156:156) {
     var_value<-(WRF_file[156])    #  only one variable (DUST_5) == var = 156
     names(var_value)<- "xxyyzz"
     var_value<- (var_value$xxyyzz)
     LON<-WRF_file$XLONG[, ,2]
     LAT<-WRF_file$XLAT[, ,2]
     
     xmn= min(LON)
     xmx=max(LON)
     ymn=min(LAT)
     ymx=max(LAT)
     
  all_rasters <- stack()    # stack ALL HOURS together in an unique raster
     
     # i = 2
   
  for (i in 1:dim(var_value)[4]){      # time dimension (always 3)
    # take only the first level in altitude
    r <- raster((var_value[ , , 1,i]), xmn, xmx, ymn,  ymx, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    plot(r)
    name_time <- WRF_file$Times[i]
    names(r)<- paste("WRF_", str_sub(name_time, start = 1, end = -7), sep="")
    all_rasters<- stack(all_rasters,r)
   # names(all_rasters)<- c(name,name,name)
  }
 
   
#   }
  
  # writeRaster(all_rasters, paste("WRF_", name_vari[j], ".tif", sep="" ) , options= "INTERLEAVE=BAND", overwrite=T)
  write.csv(names(all_rasters), paste("Layer_names_", name_vari[156],name, ".csv", sep=""))
  return(all_rasters)
}

 
 BBB <- lapply(filenames, import_nc_WRF) 
 
 
 # make a large stack raster with the 19*3=57 layeer for DUST_1
 ras_stack<- stack()
 

 for (jj in 1:24){
   for (kk in 1:3){
 #plot(BBB[[jj]],kk)
     ras <- raster(BBB[[jj]], kk)
     ras_stack<- stack(ras_stack,ras)
   }
 }
 
 
 AAA <- ras_stack[[70]]
 plot(AAA) 
 
 
writeRaster(ras_stack, "DUST_5_WRFChem.tif" , options= "INTERLEAVE=BAND", overwrite=T)


#########################################################################################

library(leaflet)
# reload rasters by band or layers (time)
DUST_5_WRFChem <- raster("D:/Dust_Event_UAE_2015/trial_runs/DUST_5_WRFChem.tif", band = 70)

plot(DUST_5_WRFChem)

# make a leaflet plot
pal_WRF <- colorNumeric(c("#0000ff", "#ffff00", "#ff0000"),
                           getValues(DUST_5_WRFChem),na.color = "transparent")

MIN_PAL <- min(getValues(DUST_5_WRFChem))
MAX_PAL <- max(getValues(DUST_5_WRFChem))


map <- leaflet() %>%
  addTiles(group = "OSM (default)") %>%
  addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
  addProviderTiles("Thunderforest.Landscape", group = "Topographical") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
  
  addRasterImage(DUST_5_WRFChem, colors = pal_WRF, opacity = 0.6,
                 group = "April 2 @ 9am") %>%
  
  addLegend("bottomright", pal = pal_WRF, values = c(MIN_PAL, MAX_PAL), 
            title = "<br><strong>AOD-MODIS</strong>",
            labFormat = labelFormat(prefix = ""),
            opacity = 0.6) %>%
  
  addLayersControl(
    baseGroups = c("Toner Lite", "Road map", "Topographical", "Satellite"),
    overlayGroups = c("April 2 @ 9am"),
    options = layersControlOptions(collapsed = TRUE)) 

# %>%
 # hideGroup(c("April 2 @ 9am")) 

map


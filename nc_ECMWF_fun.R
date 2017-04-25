library(RNetCDF)
library(rgdal)
library(ncdf4)
library(raster)

import_nc_ECMWF<- function(directory="Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR_V1/ECMWF model files", year_input=2013 ){

  
  #### importing the UAE shapefile to use as a masking 
  dir <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/website_MODIS/UAE_boundary"
  ### shapefile for UAE
  shp_UAE <- readOGR(dsn = dir, layer = "uae_emirates")
  
  # ----- Transform to EPSG 4326 - WGS84 (required)
  shp_UAE <- spTransform(shp_UAE, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  # names(shp)
  
  shp_UAE@data$name <- 1:nrow(shp_UAE)
  # plot(shp_UAE)
  
  
  ##### importing the NC files for the specific year 
  
  setwd(directory)
  patt<- paste(year_input, ".nc", sep="")
  filenames <- list.files(pattern = patt)
  
  ECMWF_file <- open.nc(filenames)
  ECMWF_file <- read.nc(ECMWF_file)
  name_vari<- names(ECMWF_file)
  
  ######
  # creating and setting output directory
  
  dir.create(file.path("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR_V1/ECMWF model files", year_input), showWarnings = F)
  setwd(file.path("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR_V1/ECMWF model files/", year_input))
  
  #### looping the variables of the nc files
  
  for( j in 4:length(name_vari) ){
    var_value<-(ECMWF_file[j])
    names(var_value)<- "xxyyzz"
    var_value<- (var_value$xxyyzz)
    LON<-ECMWF_file$longitude
    LAT<-ECMWF_file$latitude
    TIME<-ECMWF_file$time
    
 
    xmn= min(LON)
    xmx=max(LON)
    ymn=min(LAT)
    ymx=max(LAT)
    
    all_rasters<-stack()
    
    #### looping the days of the year 
    
    for (i in 1:dim(var_value)[3]){
      r <- raster((var_value[ , ,i]), xmn, xmx, ymn,  ymx, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
      name_time <- as.POSIXct(TIME[i]*3600, origin="1900-01-01", tz="UTC")
      names(r)<- paste("ECMWF_",name_time,sep="")
      all_rasters<- stack(all_rasters,r)
    }
    
    all_rasters <- crop(all_rasters, extent(shp_UAE))
    all_rasters <- mask(all_rasters, shp_UAE)
    writeRaster(all_rasters, paste("ECMWF_", name_vari[j], ".tif", sep="" ) , options= "INTERLEAVE=BAND", overwrite=T )
    write.csv(names(all_rasters), paste("Layer_names_", name_vari[j], ".csv", sep="" ))
  }
}



library(RNetCDF)
library(rgdal)
library(ncdf4)
library(raster)
library(stringr)

########################
#### for MET data ######
########################


########################
# Relative Humidity ####
########################


# list .nc files
setwd("D:/Dust_Event_UAE_2015/WRF_trial_runs")
# 4km
setwd("D:/Dust_Event_UAE_2015/WRF_trial_runs/DUST_AOD_FK/extinction55/4km")

patt<- ".nc"
filenames <- list.files(pattern = patt)
filenames <- filenames

#############################################
## function to import multiple .nc files ####
#############################################

# filenames <- filenames[1]


# gerate a time sequence 
start <- as.POSIXct("2015-03-31 00:00")
interval <- 60 #minutes
end <- start + as.difftime(6, units="days")
TS <- seq(from=start, by=interval*60, to=end)
TS <- TS[1:96]



import_nc_WRF <- function(filenames){
  
  ######
  #### looping the variables of the nc files
  
  # jj = 1
  
  all_rasters <- stack()
  qq<- 1
  
  for(jj in 1:length(filenames)) {
    
    
    WRF_file <- open.nc(filenames[jj])
    WRF_file <- read.nc(WRF_file)
    name_vari <- names(WRF_file)
    name <- str_sub(filenames[jj], start = 1, end = -25)
    
    
    var_value <- (WRF_file[15])    #  only one variable (rh_2m) == var = 19
    names(var_value)<- "xxyyzz"
    var_value<- (var_value$xxyyzz)
    LON <- WRF_file$lon
    LAT <- WRF_file$lat
    
    xmn= min(LON)
    xmx=max(LON)
    ymn=min(LAT)
    ymx=max(LAT)
    
    # j = 5
    
    
    
    for (j in 1:dim(var_value)[3]){      # time dimension (always 24)
      MMM <-  t(var_value[ , ,j])   # map is upside down 
      MMM <- MMM[nrow(MMM):1, ]
      r <- raster(MMM, xmn, xmx, ymn,  ymx, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
      plot(r)
      name_time <- TS[qq]
      names(r) <- paste("AOD_WRF_Chem_", name_time, sep = "")
      all_rasters <- stack(all_rasters,r)
      qq<- qq+1
    }
  }
  
  write.csv(names(all_rasters), paste(name,  ".csv", sep=""))
  return(all_rasters)
}


BBB <- import_nc_WRF(filenames)


writeRaster(BBB, "RH_4km_WRFChem_DUST1_Em3.tif" , options= "INTERLEAVE=BAND", overwrite=T)
writeRaster(BBB, "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_AOD_FK/extinction55/4km/RH_4km_WRFChem_DUST1_Em3.tif" , options= "INTERLEAVE=BAND", overwrite=T)



########################
# Pressure #############
########################


# list .nc files
setwd("D:/Dust_Event_UAE_2015/WRF_trial_runs")
# 4km
setwd("D:/Dust_Event_UAE_2015/WRF_trial_runs/DUST_AOD_FK/extinction55/12km")

patt<- ".nc"
filenames <- list.files(pattern = patt)
filenames <- filenames

#############################################
## function to import multiple .nc files ####
#############################################

# filenames <- filenames[1]


# gerate a time sequence 
start <- as.POSIXct("2015-03-31 00:00")
interval <- 60 #minutes
end <- start + as.difftime(6, units="days")
TS <- seq(from=start, by=interval*60, to=end)
TS <- TS[1:96]



import_nc_WRF <- function(filenames){
  
  ######
  #### looping the variables of the nc files
  
  # jj = 1
  
  all_rasters <- stack()
  qq<- 1
  
  for(jj in 1:length(filenames)) {
    
    
    WRF_file <- open.nc(filenames[jj])
    WRF_file <- read.nc(WRF_file)
    name_vari <- names(WRF_file)
    name <- str_sub(filenames[jj], start = 1, end = -25)
    
    
    var_value <- (WRF_file[13])    #  only one variable (p_sfc) == var = 13
    names(var_value)<- "xxyyzz"
    var_value<- (var_value$xxyyzz)
    LON <- WRF_file$lon
    LAT <- WRF_file$lat
    
    xmn= min(LON)
    xmx=max(LON)
    ymn=min(LAT)
    ymx=max(LAT)
    
    # j = 5
    
    
    
    for (j in 1:dim(var_value)[3]){      # time dimension (always 24)
      MMM <-  t(var_value[ , ,j])   # map is upside down 
      MMM <- MMM[nrow(MMM):1, ]
      r <- raster(MMM, xmn, xmx, ymn,  ymx, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
      plot(r)
      name_time <- TS[qq]
      names(r) <- paste("AOD_WRF_Chem_", name_time, sep = "")
      all_rasters <- stack(all_rasters,r)
      qq<- qq+1
    }
  }
  
  write.csv(names(all_rasters), paste(name,  ".csv", sep=""))
  return(all_rasters)
}


BBB <- import_nc_WRF(filenames)


writeRaster(BBB, "pressure_12km_WRFChem_DUST1_Em3.tif" , options= "INTERLEAVE=BAND", overwrite=T)
writeRaster(BBB, "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_AOD_FK/extinction55/12km/pressure_12km_WRFChem_DUST1_Em3.tif" , options= "INTERLEAVE=BAND", overwrite=T)




## End

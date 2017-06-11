
library(RNetCDF)
library(rgdal)
library(ncdf4)
library(raster)
library(stringr)


# list .nc files

setwd("Z:/_SHARED_FOLDERS/Air Quality/WRFChem/20150402_dust_only/big_domain")

# generate raster stacks for Temperature, Irradiance, Wind Speed, Wind Direction, Rleative Humidity.

#################
# temperature ###
#################

patt<- ".nc"
filenames <- list.files(pattern = patt)
# load WRFChem output from Mike (only dust concentration simulation for the 2nd April 2015 dust storm)
filenames <- "T_2m.nc"  # 2m temperature data from WRFChem

# gerate a time sequence for the WRF-Chem run at intervals of 1 hour (should be 145 images), 6 days
start <- as.POSIXct("2015-03-29 00:00")
interval <- 60 #minutes
end <- start + as.difftime(6, units="days")
TS <- seq(from=start, by=interval*60, to=end)
TS <- TS[1:145]
name <- str_sub(filenames, start = 1, end = -4)


# inizialise an empty raster to stack ALL HOURS together in an unique raster 

 import_nc_WRF <- function(filenames){

  WRF_file <- open.nc(filenames)
  WRF_file <- read.nc(WRF_file)
  name_vari <- names(WRF_file)
  
  
#### only one variable (T_2m) == var = 4
  
     var_value<-(WRF_file[4])
     names(var_value) <- "xxyyzz"
     var_value <- (var_value$xxyyzz)
     LON <-WRF_file$lon
     LAT <-WRF_file$lat
     
     xmn = min(LON)
     xmx = max(LON)
     ymn = min(LAT)
     ymx = max(LAT)
     
  all_rasters <- stack()    # stack ALL 144 HOURS together in an unique raster
     
     # i = 5
  
  
  for (i in 1:dim(var_value)[3]){      # time dimension (always 145)
    MMM <-  t(var_value[ , , i])    # map is upside down 
    MMM <- MMM[nrow(MMM):1, ]
    r <- raster(MMM, xmn, xmx, ymn,  ymx, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    plot(r)
    name_time <- TS[i]
    names(r)<- paste("WRF_Chem_", name_time, sep = "")
    all_rasters<- stack(all_rasters,r)
  }

  
  write.csv(names(all_rasters), paste(name,  ".csv", sep=""))
  return(all_rasters)
}

 
 
 BBB <- lapply(filenames, import_nc_WRF) 
 
 ras_stack <- stack()
 
# kk <- 50
 
 for (kk in 1:145){          # number of hours (time stamp) 6 days, 145 hours
   plot(BBB[[1]],kk)
   ras <- raster(BBB[[1]], kk)
   ras_stack<- stack(ras_stack,ras)
 }
 
 
writeRaster(ras_stack, "Temp_2m_WRFChem_02April2015_stack_6_DAYS_LARGE.tif" , options= "INTERLEAVE=BAND", overwrite=T)

#########################################################################################
#########################################################################################


#################
# Iradiance #####
#################

patt<- ".nc"
filenames <- list.files(pattern = patt)
# load WRFChem output from Mike (only dust concentration simulation for the 2nd April 2015 dust storm)
filenames <- "swdown.nc"  # GHI (W/m2). This is normally what you use to compare to irradiation.

# gerate a time sequence for the WRF-Chem run at intervals of 1 hour (should be 145 images), 6 days
start <- as.POSIXct("2015-03-29 00:00")
interval <- 60 #minutes
end <- start + as.difftime(6, units="days")
TS <- seq(from=start, by=interval*60, to=end)
TS <- TS[1:145]
name <- str_sub(filenames, start = 1, end = -4)


# inizialise an empty raster to stack ALL HOURS together in an unique raster 

import_nc_WRF <- function(filenames){
  
  WRF_file <- open.nc(filenames)
  WRF_file <- read.nc(WRF_file)
  name_vari <- names(WRF_file)
  
  
  #### only one variable (swdon) == var = 4
  
  var_value<-(WRF_file[4])
  names(var_value) <- "xxyyzz"
  var_value <- (var_value$xxyyzz)
  LON <-WRF_file$lon
  LAT <-WRF_file$lat
  
  xmn = min(LON)
  xmx = max(LON)
  ymn = min(LAT)
  ymx = max(LAT)
  
  all_rasters <- stack()    # stack ALL 144 HOURS together in an unique raster
  
  # i = 5
  
  
  for (i in 1:dim(var_value)[3]){      # time dimension (always 145)
    MMM <-  t(var_value[ , , i])    # map is upside down 
    MMM <- MMM[nrow(MMM):1, ]
    r <- raster(MMM, xmn, xmx, ymn,  ymx, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    plot(r)
    name_time <- TS[i]
    names(r)<- paste("WRF_Chem_", name_time, sep = "")
    all_rasters<- stack(all_rasters,r)
  }
  
  
  write.csv(names(all_rasters), paste(name,  ".csv", sep=""))
  return(all_rasters)
}



BBB <- lapply(filenames, import_nc_WRF) 

ras_stack <- stack()

# kk <- 50

for (kk in 1:145){          # number of hours (time stamp) 6 days, 145 hours
  plot(BBB[[1]],kk)
  ras <- raster(BBB[[1]], kk)
  ras_stack<- stack(ras_stack,ras)
}


writeRaster(ras_stack, "Irradiance_Wm2_WRFChem_02April2015_stack_6_DAYS_LARGE.tif" , options= "INTERLEAVE=BAND", overwrite=T)


###################################################################################################
###################################################################################################


#################
# wind Speed ####
#################

patt<- ".nc"
filenames <- list.files(pattern = patt)
# load WRFChem output from Mike (only dust concentration simulation for the 2nd April 2015 dust storm)
filenames <- "ws_10m.nc"  # nc    10m wind speed

# gerate a time sequence for the WRF-Chem run at intervals of 1 hour (should be 145 images), 6 days
start <- as.POSIXct("2015-03-29 00:00")
interval <- 60 #minutes
end <- start + as.difftime(6, units="days")
TS <- seq(from=start, by=interval*60, to=end)
TS <- TS[1:145]
name <- str_sub(filenames, start = 1, end = -4)


# inizialise an empty raster to stack ALL HOURS together in an unique raster 

import_nc_WRF <- function(filenames){
  
  WRF_file <- open.nc(filenames)
  WRF_file <- read.nc(WRF_file)
  name_vari <- names(WRF_file)
  
  
  #### only one variable (swdon) == var = 4
  
  var_value<-(WRF_file[4])
  names(var_value) <- "xxyyzz"
  var_value <- (var_value$xxyyzz)
  LON <-WRF_file$lon
  LAT <-WRF_file$lat
  
  xmn = min(LON)
  xmx = max(LON)
  ymn = min(LAT)
  ymx = max(LAT)
  
  all_rasters <- stack()    # stack ALL 144 HOURS together in an unique raster
  
  # i = 5
  
  
  for (i in 1:dim(var_value)[3]){      # time dimension (always 145)
    MMM <-  t(var_value[ , , i])    # map is upside down 
    MMM <- MMM[nrow(MMM):1, ]
    r <- raster(MMM, xmn, xmx, ymn,  ymx, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    plot(r)
    name_time <- TS[i]
    names(r)<- paste("WRF_Chem_", name_time, sep = "")
    all_rasters<- stack(all_rasters,r)
  }
  
  
  write.csv(names(all_rasters), paste(name,  ".csv", sep=""))
  return(all_rasters)
}



BBB <- lapply(filenames, import_nc_WRF) 

ras_stack <- stack()

# kk <- 50

for (kk in 1:145){          # number of hours (time stamp) 6 days, 145 hours
  plot(BBB[[1]],kk)
  ras <- raster(BBB[[1]], kk)
  ras_stack<- stack(ras_stack,ras)
}


writeRaster(ras_stack, "Wind_Speed_Wm2_WRFChem_02April2015_stack_6_DAYS_LARGE.tif" , options= "INTERLEAVE=BAND", overwrite=T)




####################
# wind direction ###
####################



patt<- ".nc"
filenames <- list.files(pattern = patt)
# load WRFChem output from Mike (only dust concentration simulation for the 2nd April 2015 dust storm)
filenames <- "wd_10m.nc"  # nc    10m wind speed

# gerate a time sequence for the WRF-Chem run at intervals of 1 hour (should be 145 images), 6 days
start <- as.POSIXct("2015-03-29 00:00")
interval <- 60 #minutes
end <- start + as.difftime(6, units="days")
TS <- seq(from=start, by=interval*60, to=end)
TS <- TS[1:145]
name <- str_sub(filenames, start = 1, end = -4)


# inizialise an empty raster to stack ALL HOURS together in an unique raster 

import_nc_WRF <- function(filenames){
  
  WRF_file <- open.nc(filenames)
  WRF_file <- read.nc(WRF_file)
  name_vari <- names(WRF_file)
  
  
  #### only one variable (wind direction) == var = 4
  
  var_value<-(WRF_file[4])
  names(var_value) <- "xxyyzz"
  var_value <- (var_value$xxyyzz)
  LON <-WRF_file$lon
  LAT <-WRF_file$lat
  
  xmn = min(LON)
  xmx = max(LON)
  ymn = min(LAT)
  ymx = max(LAT)
  
  all_rasters <- stack()    # stack ALL 144 HOURS together in an unique raster
  
  # i = 5
  
  
  for (i in 1:dim(var_value)[3]){      # time dimension (always 145)
    MMM <-  t(var_value[ , , i])    # map is upside down 
    MMM <- MMM[nrow(MMM):1, ]
    r <- raster(MMM, xmn, xmx, ymn,  ymx, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    plot(r)
    name_time <- TS[i]
    names(r)<- paste("WRF_Chem_", name_time, sep = "")
    all_rasters<- stack(all_rasters,r)
  }
  
  
  write.csv(names(all_rasters), paste(name,  ".csv", sep=""))
  return(all_rasters)
}



BBB <- lapply(filenames, import_nc_WRF) 

ras_stack <- stack()

# kk <- 50

for (kk in 1:145){          # number of hours (time stamp) 6 days, 145 hours
  plot(BBB[[1]],kk)
  ras <- raster(BBB[[1]], kk)
  ras_stack<- stack(ras_stack,ras)
}


writeRaster(ras_stack, "Wind_Direction_WRFChem_02April2015_stack_6_DAYS_LARGE.tif" , options= "INTERLEAVE=BAND", overwrite=T)




#######################
# Relative Humidity ###
#######################


patt<- ".nc"
filenames <- list.files(pattern = patt)
# load WRFChem output from Mike (only dust concentration simulation for the 2nd April 2015 dust storm)
filenames <- "rh_2m.nc"  # nc    2m wind speed

# gerate a time sequence for the WRF-Chem run at intervals of 1 hour (should be 145 images), 6 days
start <- as.POSIXct("2015-03-29 00:00")
interval <- 60 #minutes
end <- start + as.difftime(6, units="days")
TS <- seq(from=start, by=interval*60, to=end)
TS <- TS[1:145]
name <- str_sub(filenames, start = 1, end = -4)


# inizialise an empty raster to stack ALL HOURS together in an unique raster 

import_nc_WRF <- function(filenames){
  
  WRF_file <- open.nc(filenames)
  WRF_file <- read.nc(WRF_file)
  name_vari <- names(WRF_file)
  
  
  #### only one variable (rh_2m) == var = 4
  
  var_value<-(WRF_file[4])
  names(var_value) <- "xxyyzz"
  var_value <- (var_value$xxyyzz)
  LON <-WRF_file$lon
  LAT <-WRF_file$lat
  
  xmn = min(LON)
  xmx = max(LON)
  ymn = min(LAT)
  ymx = max(LAT)
  
  all_rasters <- stack()    # stack ALL 144 HOURS together in an unique raster
  
  # i = 5
  
  
  for (i in 1:dim(var_value)[3]){      # time dimension (always 145)
    MMM <-  t(var_value[ , , i])    # map is upside down 
    MMM <- MMM[nrow(MMM):1, ]
    r <- raster(MMM, xmn, xmx, ymn,  ymx, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    plot(r)
    name_time <- TS[i]
    names(r)<- paste("WRF_Chem_", name_time, sep = "")
    all_rasters<- stack(all_rasters,r)
  }
  
  
  write.csv(names(all_rasters), paste(name,  ".csv", sep=""))
  return(all_rasters)
}



BBB <- lapply(filenames, import_nc_WRF) 

ras_stack <- stack()

# kk <- 50

for (kk in 1:145){          # number of hours (time stamp) 6 days, 145 hours
  plot(BBB[[1]],kk)
  ras <- raster(BBB[[1]], kk)
  ras_stack<- stack(ras_stack,ras)
}


writeRaster(ras_stack, "RelativeHumidity_WRFChem_02April2015_stack_6_DAYS_LARGE.tif" , options= "INTERLEAVE=BAND", overwrite=T)





#######################
# Pressure ############
#######################


patt<- ".nc"
filenames <- list.files(pattern = patt)
# load WRFChem output from Mike (only dust concentration simulation for the 2nd April 2015 dust storm)
filenames <- "p_sfc.nc"  # nc    surface pressure

# gerate a time sequence for the WRF-Chem run at intervals of 1 hour (should be 145 images), 6 days
start <- as.POSIXct("2015-03-29 00:00")
interval <- 60 #minutes
end <- start + as.difftime(6, units="days")
TS <- seq(from=start, by=interval*60, to=end)
TS <- TS[1:145]
name <- str_sub(filenames, start = 1, end = -4)


# inizialise an empty raster to stack ALL HOURS together in an unique raster 

import_nc_WRF <- function(filenames){
  
  WRF_file <- open.nc(filenames)
  WRF_file <- read.nc(WRF_file)
  name_vari <- names(WRF_file)
  
  
  #### only one variable (p_sfc) == var = 4
  
  var_value<-(WRF_file[4])
  names(var_value) <- "xxyyzz"
  var_value <- (var_value$xxyyzz)
  LON <-WRF_file$lon
  LAT <-WRF_file$lat
  
  xmn = min(LON)
  xmx = max(LON)
  ymn = min(LAT)
  ymx = max(LAT)
  
  all_rasters <- stack()    # stack ALL 144 HOURS together in an unique raster
  
  # i = 5
  
  
  for (i in 1:dim(var_value)[3]){      # time dimension (always 145)
    MMM <-  t(var_value[ , , i])    # map is upside down 
    MMM <- MMM[nrow(MMM):1, ]
    r <- raster(MMM, xmn, xmx, ymn,  ymx, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    plot(r)
    name_time <- TS[i]
    names(r)<- paste("WRF_Chem_", name_time, sep = "")
    all_rasters<- stack(all_rasters,r)
  }
  
  
  write.csv(names(all_rasters), paste(name,  ".csv", sep=""))
  return(all_rasters)
}



BBB <- lapply(filenames, import_nc_WRF) 

ras_stack <- stack()

# kk <- 50

for (kk in 1:145){          # number of hours (time stamp) 6 days, 145 hours
  plot(BBB[[1]],kk)
  ras <- raster(BBB[[1]], kk)
  ras_stack<- stack(ras_stack,ras)
}


writeRaster(ras_stack, "Pressure_hPa_WRFChem_02April2015_stack_6_DAYS_LARGE.tif" , options= "INTERLEAVE=BAND", overwrite=T)



###########################################################
# Radiance simulated with AOD = constant value ############
###########################################################

setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/WRF_solar_radiation")
# setwd("D:/Dust_Event_UAE_2015/WRF_trial_runs/WRF_solar_radiation")

patt<- ".nc"
filenames <- list.files(pattern = patt)
filenames <- "AOD_4_5_wrfpost.nc"  # nc    short wave solar radiance

# gerate a time sequence for the WRF-Chem run at intervals of 1 hour (should be 145 images), 6 days
start <- as.POSIXct("2015-03-31 00:00")
interval <- 60 #minutes
end <- start + as.difftime(6, units="days")
TS <- seq(from=start, by=interval*60, to=end)
TS <- TS[1:96]
name <- str_sub(filenames, start = 1, end = -4)


# inizialise an empty raster to stack ALL HOURS together in an unique raster 

import_nc_WRF <- function(filenames){
  
  WRF_file <- open.nc(filenames)
  WRF_file <- read.nc(WRF_file)
  name_vari <- names(WRF_file)
  
  
  #### only one variable (sw_d) == var = 10   (short wave down)
  
  var_value <- WRF_file[11]
  names(var_value) <- "xxyyzz"
  var_value <- (var_value$xxyyzz)
  LON <-WRF_file$lon
  LAT <-WRF_file$lat
  
  xmn = min(LON)
  xmx = max(LON)
  ymn = min(LAT)
  ymx = max(LAT)
  
  all_rasters <- stack()    # stack ALL 144 HOURS together in an unique raster
  
  # i = 5
  
  
  for (i in 1:dim(var_value)[3]){      # time dimension (always 145)
    MMM <-  t(var_value[ , , i])    # map is upside down 
    MMM <- MMM[nrow(MMM):1, ]
    r <- raster(MMM, xmn, xmx, ymn,  ymx, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    plot(r)
    name_time <- TS[i]
    names(r)<- paste("WRF_Chem_", name_time, sep = "")
    all_rasters<- stack(all_rasters,r)
  }
  
  
  write.csv(names(all_rasters), paste(name,  ".csv", sep=""))
  return(all_rasters)
}



BBB <- lapply(filenames, import_nc_WRF) 

ras_stack <- stack()

# kk <- 50

for (kk in 1:96){          # number of hours (time stamp) 6 days, 145 hours
  plot(BBB[[1]],kk)
  ras <- raster(BBB[[1]], kk)
  ras_stack<- stack(ras_stack,ras)
}


writeRaster(ras_stack, "Radiance_AOD_4_5_WRFChem_02April2015_stack_4_DAYS_LARGE.tif" , options= "INTERLEAVE=BAND", overwrite=T)







library(dplyr)
library(lubridate)
library(raster)
library(rgdal)
# install.packages("NISTunits", dependencies = TRUE)
library(NISTunits)
library(stringr)
library(lubridate)
library(dplyr)
library(ggplot2)

# setwd("D:/Dust_Event_UAE_2015")
 setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015")
source("extract_pnt_raster.R")


# 2015   MASDAR-------------------------------------------------------------------------
#### REMEBER ---> time is in UTC ##############################################
AERONET_2015_MASDAR <- read.csv("150101_151231_Masdar_Institute.csv")
AERONET_2015_MASDAR <- AERONET_2015_MASDAR %>%
  mutate(Date = mdy(Date.dd.mm.yy.))

# extract hours
AERONET_2015_MASDAR$hour <- str_sub(AERONET_2015_MASDAR$Time.hh.mm.ss., start = 1, end = -7)
AERONET_2015_MASDAR$hour <- as.numeric(AERONET_2015_MASDAR$hour)

# ########################################################################
# select AOT 500 nm 
AERONET_2015_MASDAR <- AERONET_2015_MASDAR %>%
  dplyr::select(Date,
                hour,
                AOT_500)

AERONET_2015_MASDAR <- AERONET_2015_MASDAR %>%
  group_by(Date,
           hour) %>%
dplyr::summarise(AOT = mean(AOT_500, na.rm = TRUE))



AERONET_2015_MASDAR <- AERONET_2015_MASDAR %>%
  filter(Date >= "2015-03-31" & Date <= "2015-04-03")

# add AERONET site in MASDAR
AERONET_2015_MASDAR$Site <- "AERONET Masdar"


site_AERONET_MASDAR <- read.csv("site_AERONET_MASDAR.csv")

##############################################################################
## make a function that reads each station at each time and extract points ####
##############################################################################


# MODIS TERRA (AOD data)
 WRF_STACK_image <- stack("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_AOD_FK/extinction55/4km/AOD_4km_WRFChem_DUST1_Em3.tif")
# WRF_STACK_image <- stack("D:/Dust_Event_UAE_2015/WRF_trial_runs/DUST_AOD_FK/extinction55/4km/AOD_4km_WRFChem_DUST1_Em3.tif")
# WRF_STACK_image <- stack("Z:/_SHARED_FOLDERS/Air Quality/WRFChem/dust_opt3_chem_opt300_aod/AOD_WRFChem_02April2015_aod_dust_opt3_chem_opt300.tif")


# WRF_STACK_image <- stack("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_AOD_FK/extinction55/12km/AOD_12km_WRFChem_DUST1_Em3.tif")
n <- length(WRF_STACK_image@layers)  # 96 layers


# generate a time sequence
start <- as.POSIXct("2015-03-31 00:00")
interval <- 60 #minutes
end <- start + as.difftime(6, units="days")
TS <- seq(from=start, by=interval*60, to=end)
TS <- TS[1:96]


###########################################################################################
###########################################################################################


#### importing the UAE shapefile to use as a masking
dir <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/website_MODIS/UAE_boundary"
### shapefile for UAE
shp_UAE <- readOGR(dsn = dir, layer = "uae_emirates")

# ----- Transform to EPSG 4326 - WGS84 (required)
shp_UAE <- spTransform(shp_UAE, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
# names(shp)

shp_UAE@data$name <- 1:nrow(shp_UAE)
plot(shp_UAE)


#############################################################
############ WRF-chem AOD  ##################################
#############################################################

extracted_WRF_AOD <- NULL
DateTime_AOD <- NULL
site_MASDAR <- NULL

# i <- 2

for (i in 1:n) {      # this are the 96 days time
  #  for (i in 1:3) {

# WRF_STACK_image <- raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_AOD_FK/extinction55/4km/AOD_4km_WRFChem_DUST1_Em3.tif", band = i)*12
# WRF_STACK_image <- raster("Z:/_SHARED_FOLDERS/Air Quality/WRFChem/dust_opt3_chem_opt300_aod/AOD_WRFChem_02April2015_aod_dust_opt3_chem_opt300.tif", band = i)
WRF_STACK_image <- raster("D:/Dust_Event_UAE_2015/WRF_trial_runs/DUST_AOD_FK/extinction55/4km/AOD_4km_WRFChem_DUST1_Em3.tif", band = i)*6.25
# WRF_STACK_image <- raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_AOD_FK/extinction55/12km/AOD_12km_WRFChem_DUST1_Em3.tif", band = i)*6.25
  # crop raster stack from MAIAC-TERRA over the UAE only
  WRF_STACK_image <- crop(WRF_STACK_image, extent(shp_UAE))
  WRF_STACK_image <- mask(WRF_STACK_image, shp_UAE)

  plot(WRF_STACK_image)

  EXTRACTED_WRF_AOD <- extract_points(WRF_STACK_image, site_AERONET_MASDAR)
  extracted_WRF_AOD = rbind(extracted_WRF_AOD, EXTRACTED_WRF_AOD)        # data vector
  DATETIME_AOD <- as.data.frame(rep(TS[i], nrow(site_AERONET_MASDAR)))           # time vector
  DateTime_AOD <- rbind(DateTime_AOD, DATETIME_AOD)
  SITE_MASDAR <- as.data.frame(site_AERONET_MASDAR$Site)
  site_MASDAR <- rbind(site_MASDAR, SITE_MASDAR)

}


extracted_WRF_AOD <- cbind(DateTime_AOD, extracted_WRF_AOD, site_MASDAR)   # it should be the same length od the AQ_data_2015
colnames(extracted_WRF_AOD) <- c("DATETIME", "WRF_Chem", "Site")

# setwd("Z:/_SHARED_FOLDERS/Air Quality/WRFChem/dust_opt3_chem_opt300_aod")
# setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_AOD_FK/extinction55/4km")
# setwd("D:/Dust_Event_UAE_2015/WRF_trial_runs/DUST_AOD_FK/extinction55/4km")
 setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_AOD_FK/extinction55/4km")

# save data-------------------------------------
write.csv(extracted_WRF_AOD, "extracted_WRF_AOD_AERONET.csv")
extracted_WRF_AOD <- read.csv("extracted_WRF_AOD_AERONET.csv")

extracted_WRF_AOD <- extracted_WRF_AOD %>%
  mutate(DATETIME = ymd_hms(DATETIME))


extracted_WRF_AOD <- extracted_WRF_AOD %>%
  mutate(DATETIME = DATETIME + 14400)

# extracted_WRF_AOD <- extracted_WRF_AOD %>%
#   mutate(DATETIME = DATETIME + 14400)


# get the hour #######
extracted_WRF_AOD <- extracted_WRF_AOD %>%
  mutate(DATETIME = ymd_hms(DATETIME),
         hour = hour(DATETIME))


extracted_WRF_AOD <- extracted_WRF_AOD %>%
  mutate(Date = date(DATETIME))



######################################################################################################
######################################################################################################

# merge extracted AERONET data and WRF data --------------------------------------------

AERONET_WRF_2015_DAYS <- AERONET_2015_MASDAR %>%
  merge(extracted_WRF_AOD, by = c("Site", "Date", "hour"))

AERONET_WRF_2015_DAYS$ratio <- (AERONET_WRF_2015_DAYS$AOT)/(AERONET_WRF_2015_DAYS$WRF_Chem)

# write.csv(AERONET_WRF_2015_DAYS, "AOD_AERONET_WRF_2_April_2015.csv")



###################################################################################################################
######### plot TIME-SERIES of AERONET and WRF-chem data ###########################################################
###################################################################################################################

library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(splines)
library(plyr)

# setwd("Z:/_SHARED_FOLDERS/Air Quality/WRFChem/dust_opt3_chem_opt300_aod")
setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_AOD_FK/extinction55/4km")
# setwd("D:/Dust_Event_UAE_2015/WRF_trial_runs/DUST_AOD_FK/extinction55/4km")
AERONET_WRF_2015_DAYS <- read.csv("AOD_AERONET_WRF_2_April_2015.csv")

AERONET_WRF_2015_DAYS <- AERONET_WRF_2015_DAYS %>%
  mutate(DATETIME = ymd_hms(DATETIME))

str(AERONET_WRF_2015_DAYS)


jpeg('Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_AOD_FK/extinction55/4km/WRF_AERONET_AOD_TimeSeries.jpg',
#     jpeg('D:/Dust_Event_UAE_2015/WRF_trial_runs/DUST_AOD_FK/extinction55/4km/WRF_AERONET_AOD_TimeSeries.jpg',
          quality = 100, bg = "white", res = 300, width = 12, height = 9, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


plot <- ggplot(AERONET_WRF_2015_DAYS, aes(DATETIME, WRF_Chem)) + 
  theme_bw() +
  geom_line(aes(y = WRF_Chem, col = "WRF_Chem"), alpha=1, col="blue", lwd = 1.5) +
  geom_line(aes(y = AOT, col = "AOT"), alpha=1, col="red", linetype = "dashed",lwd = 1.5) +
  theme(legend.position="none") + 
  ylab(expression(paste("AOD"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 0.5, size=25, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=25),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=25, colour = "black")) +
  ylim(0, 2.5)  
plot


par(oldpar)
dev.off()




################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
###################                                      #######################################################
################### MEZAIRA - Liwa Oasis AERONET STATION #######################################################
###################                                      #######################################################
################################################################################################################
################################################################################################################
################################################################################################################

# 2015   MEZAIRA-------------------------------------------------------------------------
#### REMEBER ---> time is in UTC ##############################################
AERONET_2015_MEZAIRA <- read.csv("150101_151231_Mezaira_AD.csv")
# AERONET_2015_MEZAIRA <- read.csv("150101_151231_Mezaira.lev20_AD.csv")
AERONET_2015_MEZAIRA <- AERONET_2015_MEZAIRA %>%
  mutate(Date = mdy(Date.dd.mm.yy.))

# extract hours
AERONET_2015_MEZAIRA$hour <- str_sub(AERONET_2015_MEZAIRA$Time.hh.mm.ss., start = 1, end = -7)
AERONET_2015_MEZAIRA$hour <- as.numeric(AERONET_2015_MEZAIRA$hour)

# ########################################################################
# select AOT 500 nm 
AERONET_2015_MEZAIRA <- AERONET_2015_MEZAIRA %>%
  dplyr::select(Date,
                hour,
                AOT_500)

AERONET_2015_MEZAIRA <- AERONET_2015_MEZAIRA %>%
  group_by(Date,
           hour) %>%
  dplyr::summarise(AOT = mean(AOT_500, na.rm = TRUE))



AERONET_2015_MEZAIRA <- AERONET_2015_MEZAIRA %>%
  filter(Date >= "2015-03-31" & Date <= "2015-04-03")

# add AERONET site in MEZAIRA
AERONET_2015_MEZAIRA$Site <- "AERONET Mezaira"


site_AERONET_MEZAIRA <- read.csv("site_AERONET_MEZAIRA.csv")

##############################################################################
## make a function that reads each station at each time and extract points ####
##############################################################################


# MODIS TERRA (AOD data)
WRF_STACK_image <- stack("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_AOD_FK/extinction55/4km/AOD_4km_WRFChem_DUST1_Em3.tif")
# WRF_STACK_image <- stack("D:/Dust_Event_UAE_2015/WRF_trial_runs/DUST_AOD_FK/extinction55/4km/AOD_4km_WRFChem_DUST1_Em3.tif")
# WRF_STACK_image <- stack("Z:/_SHARED_FOLDERS/Air Quality/WRFChem/dust_opt3_chem_opt300_aod/AOD_WRFChem_02April2015_aod_dust_opt3_chem_opt300.tif")


# WRF_STACK_image <- stack("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_AOD_FK/extinction55/12km/AOD_12km_WRFChem_DUST1_Em3.tif")
n <- length(WRF_STACK_image@layers)  # 96 layers


# generate a time sequence
start <- as.POSIXct("2015-03-31 00:00")
interval <- 60 #minutes
end <- start + as.difftime(6, units="days")
TS <- seq(from=start, by=interval*60, to=end)
TS <- TS[1:96]


###########################################################################################
###########################################################################################


#### importing the UAE shapefile to use as a masking
dir <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/website_MODIS/UAE_boundary"
### shapefile for UAE
shp_UAE <- readOGR(dsn = dir, layer = "uae_emirates")

# ----- Transform to EPSG 4326 - WGS84 (required)
shp_UAE <- spTransform(shp_UAE, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
# names(shp)

shp_UAE@data$name <- 1:nrow(shp_UAE)
plot(shp_UAE)


#############################################################
############ WRF-chem AOD  ##################################
#############################################################

extracted_WRF_AOD <- NULL
DateTime_AOD <- NULL
site_MEZAIRA <- NULL

# i <- 2

for (i in 1:n) {      # this are the 96 days time
  #  for (i in 1:3) {
  
  # WRF_STACK_image <- raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_AOD_FK/extinction55/4km/AOD_4km_WRFChem_DUST1_Em3.tif", band = i)*12
  # WRF_STACK_image <- raster("Z:/_SHARED_FOLDERS/Air Quality/WRFChem/dust_opt3_chem_opt300_aod/AOD_WRFChem_02April2015_aod_dust_opt3_chem_opt300.tif", band = i)
  WRF_STACK_image <- raster("D:/Dust_Event_UAE_2015/WRF_trial_runs/DUST_AOD_FK/extinction55/4km/AOD_4km_WRFChem_DUST1_Em3.tif", band = i)*6.25
  # WRF_STACK_image <- raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_AOD_FK/extinction55/12km/AOD_12km_WRFChem_DUST1_Em3.tif", band = i)*6.25
  # crop raster stack from MAIAC-TERRA over the UAE only
  WRF_STACK_image <- crop(WRF_STACK_image, extent(shp_UAE))
  WRF_STACK_image <- mask(WRF_STACK_image, shp_UAE)
  
  plot(WRF_STACK_image)
  
  EXTRACTED_WRF_AOD <- extract_points(WRF_STACK_image, site_AERONET_MEZAIRA)
  extracted_WRF_AOD = rbind(extracted_WRF_AOD, EXTRACTED_WRF_AOD)        # data vector
  DATETIME_AOD <- as.data.frame(rep(TS[i], nrow(site_AERONET_MEZAIRA)))           # time vector
  DateTime_AOD <- rbind(DateTime_AOD, DATETIME_AOD)
  SITE_MEZAIRA <- as.data.frame(site_AERONET_MEZAIRA$Site)
  site_MEZAIRA <- rbind(site_MEZAIRA, SITE_MEZAIRA)
  
}


extracted_WRF_AOD <- cbind(DateTime_AOD, extracted_WRF_AOD, site_MEZAIRA)   # it should be the same length od the AQ_data_2015
colnames(extracted_WRF_AOD) <- c("DATETIME", "WRF_Chem", "Site")

# setwd("Z:/_SHARED_FOLDERS/Air Quality/WRFChem/dust_opt3_chem_opt300_aod")
# setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_AOD_FK/extinction55/4km")
# setwd("D:/Dust_Event_UAE_2015/WRF_trial_runs/DUST_AOD_FK/extinction55/4km")
setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_AOD_FK/extinction55/4km")

# save data-------------------------------------
write.csv(extracted_WRF_AOD, "extracted_WRF_AOD_AERONET_Mezaira.csv")
extracted_WRF_AOD <- read.csv("extracted_WRF_AOD_AERONET_Mezaira.csv")

extracted_WRF_AOD <- extracted_WRF_AOD %>%
  mutate(DATETIME = ymd_hms(DATETIME))


extracted_WRF_AOD <- extracted_WRF_AOD %>%
  mutate(DATETIME = DATETIME + 14400)

# extracted_WRF_AOD <- extracted_WRF_AOD %>%
#   mutate(DATETIME = DATETIME + 14400)


# get the hour #######
extracted_WRF_AOD <- extracted_WRF_AOD %>%
  mutate(DATETIME = ymd_hms(DATETIME),
         hour = hour(DATETIME))


extracted_WRF_AOD <- extracted_WRF_AOD %>%
  mutate(Date = date(DATETIME))



######################################################################################################
######################################################################################################

# merge extracted AERONET data and WRF data --------------------------------------------

AERONET_WRF_2015_DAYS <- AERONET_2015_MEZAIRA %>%
  merge(extracted_WRF_AOD, by = c("Site", "Date", "hour"))

# write.csv(AERONET_WRF_2015_DAYS, "AOD_AERONET_WRF_2_April_2015_Mezaira.csv")



###################################################################################################################
######### plot TIME-SERIES of AERONET and WRF-chem data ###########################################################
###################################################################################################################

library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(splines)
library(plyr)

# setwd("Z:/_SHARED_FOLDERS/Air Quality/WRFChem/dust_opt3_chem_opt300_aod")
setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_AOD_FK/extinction55/4km")
# setwd("D:/Dust_Event_UAE_2015/WRF_trial_runs/DUST_AOD_FK/extinction55/4km")
AERONET_WRF_2015_DAYS <- read.csv("AOD_AERONET_WRF_2_April_2015_Mezaira.csv")
str(AERONET_WRF_2015_DAYS)

# AERONET_WRF_2015_DAYS <- AERONET_WRF_2015_DAYS %>%
#   dplyr::mutate(DATETIME = ymd_hms(DATETIME))

AERONET_WRF_2015_DAYS <- AERONET_WRF_2015_DAYS %>%
  dplyr::mutate(DATETIME = mdy_hm(DATETIME))

str(AERONET_WRF_2015_DAYS)



plot <- ggplot(AERONET_WRF_2015_DAYS, aes(DATETIME, WRF_Chem)) + 
  theme_bw() +
  geom_line(aes(y = WRF_Chem, col = "WRF_Chem"), alpha=1, col="blue", lwd = 1.5) +
  geom_line(aes(y = AOT, col = "AOT"), alpha=1, col="red", linetype = "dashed",lwd = 1.5) +
  theme(legend.position="none") + 
  ylab(expression(paste("AOD"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 0.5, size=25, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=25),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=25, colour = "black")) +
  ylim(0, 1)  
plot


output_folder <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_AOD_FK/extinction55/4km/" 
png(paste0(output_folder,"WRF_AERONET_MEZARIA_AOD_TimeSeries.jpg"),
    width = 10, height = 8, units = "in", pointsize = 30,
    bg = "white", res = 150)
print(plot)
dev.off()






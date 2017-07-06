
library(readr)
library(dplyr)
library(lubridate)
library(raster)
library(rgdal)
# install.packages("NISTunits", dependencies = TRUE)
library(NISTunits)
library(ggplot2)


setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015")
source("extract_pnt_raster.R")


# read all AWS NCMS data
setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs")
All_AWS_data <- read_csv("AWS_concatenated_DUST_2_April_2015.csv")


# load coordinates of the monitoring stations:
STATIONS_COORDS <- read_csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/stations/stations_clean_FK.csv") 
colnames(STATIONS_COORDS) <- c("station", "latitude", "longitude")

# join coordinated of the the station with the total dataset
All_AWS_data <- All_AWS_data %>%
  left_join(STATIONS_COORDS, by = c("station"))


# select unique sites of the AWS NCMS data
sites_stations_AWS_NCMS <- All_AWS_data[!duplicated(All_AWS_data[c("station", "latitude", "longitude" )]),]
# rename latitude and longitude
colnames(sites_stations_AWS_NCMS)[colnames(sites_stations_AWS_NCMS) == 'latitude'] <- 'Latitude'
colnames(sites_stations_AWS_NCMS)[colnames(sites_stations_AWS_NCMS) == 'longitude'] <- 'Longitude'

# omit stations without latitude and longitude
sites_stations_AWS_NCMS <- sites_stations_AWS_NCMS[!(is.na(sites_stations_AWS_NCMS$Latitude)), ]


##############################################################################
# read IRRADIATION data from WRF Chem ########################################
##############################################################################
## make a function that reads each station at each time and extract points ###
##############################################################################

# read all bands in a stack raster
# WRF_STACK_image_Irr <- stack("Z:/_SHARED_FOLDERS/Air Quality/WRFChem/20150402_dust_only/big_domain/Irradiance_Wm2_WRFChem_02April2015_stack_6_DAYS_LARGE.tif")
# WRF_STACK_image_Irr <- stack("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/WRF_solar_radiation/Radiance_AOD_2_WRFChem_02April2015_stack_4_DAYS_LARGE.tif")
# WRF_STACK_image_Irr <- stack("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/WRF_solar_radiation/Radiance_AOD_3_WRFChem_02April2015_stack_4_DAYS_LARGE.tif")
# WRF_STACK_image_Irr <- stack("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/WRF_solar_radiation/Radiance_AOD_3_6_WRFChem_02April2015_stack_4_DAYS_LARGE.tif")
# WRF_STACK_image_Irr <- stack("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/WRF_solar_radiation/Radiance_AOD_0_8_WRFChem_02April2015_stack_4_DAYS_LARGE.tif")
WRF_STACK_image_Irr <- stack("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/WRF_solar_radiation/Radiance_AOD_4_5_WRFChem_02April2015_stack_4_DAYS_LARGE.tif")

# WRF_STACK_image_Irr <- stack("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_AOD_FK/extinction55/4km/radiance_4km_WRFChem_DUST1_Em3.tif")

n <- length(WRF_STACK_image_Irr@layers)
# n <- length(WRF_STACK_image_Irr@layers)-1


# generate a time sequence for the WRF-Chem run at intervals of 1 hour (should be 144 images)
start <- as.POSIXct("2015-03-31 00:00:00")
interval <- 60 #minutes
end <- start + as.difftime(6, units="days")
TS <- seq(from=start, by=interval*60, to=end)   # same time series as the AQ data
TS <- TS[1:96]

# i <- 3

#### Radiance from WRFChem ###############################################################
###########################################################################################

# make an empty vector

extracted_WRF_Irr <- NULL
DateTime_Irr <- NULL
site_Irr <- NULL

 for (i in 1:n) {   # this is a time
  
 # WRF_STACK_image_Irr <- raster("Z:/_SHARED_FOLDERS/Air Quality/WRFChem/20150402_dust_only/big_domain/Irradiance_Wm2_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = i)
#  WRF_STACK_image_Irr <- raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/WRF_solar_radiation/Radiance_AOD_2_WRFChem_02April2015_stack_4_DAYS_LARGE.tif", band = i) 
#  WRF_STACK_image_Irr <- raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/WRF_solar_radiation/Radiance_AOD_3_WRFChem_02April2015_stack_4_DAYS_LARGE.tif", band = i)  
#  WRF_STACK_image_Irr <- raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/WRF_solar_radiation/Radiance_AOD_3_6_WRFChem_02April2015_stack_4_DAYS_LARGE.tif", band = i) 
#  WRF_STACK_image_Irr <- raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/WRF_solar_radiation/Radiance_AOD_0_8_WRFChem_02April2015_stack_4_DAYS_LARGE.tif", band = i)  
# WRF_STACK_image_Irr <- raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/WRF_solar_radiation/Radiance_AOD_4_WRFChem_02April2015_stack_4_DAYS_LARGE.tif", band = i)  

WRF_STACK_image_Irr <- raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/WRF_solar_radiation/Radiance_AOD_4_2_WRFChem_02April2015_stack_4_DAYS_LARGE.tif", band = i)  

# WRF_STACK_image_Irr <- raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_AOD_FK/extinction55/4km/radiance_4km_WRFChem_DUST1_Em3.tif", band = i)
   
      
    plot(WRF_STACK_image_Irr)
  EXTRACTED_WRF_Irr <- extract_points(WRF_STACK_image_Irr, sites_stations_AWS_NCMS)
  extracted_WRF_Irr = rbind(extracted_WRF_Irr, EXTRACTED_WRF_Irr)    # data vector
  DATETIME_Irr <- as.data.frame(rep(TS[i], nrow(sites_stations_AWS_NCMS)))           # time vector
  DateTime_Irr <- rbind(DateTime_Irr, DATETIME_Irr)
  SITE_Irr <- as.data.frame(sites_stations_AWS_NCMS$station)
  site_Irr <- rbind(site_Irr, SITE_Irr)
  
 }

extracted_WRF_Irr <- cbind(DateTime_Irr, extracted_WRF_Irr, site_Irr)
colnames(extracted_WRF_Irr) <- c("DateTime", "WRF_CHEM_Irr", "station")


# save data-------------------------------------
# write.csv(extracted_WRF_Irr, "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/extracted_WRF_Irradiance.csv")
# extracted_WRF_Irr <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/extracted_WRF_Irradiance.csv")


# write.csv(extracted_WRF_Irr, "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/extracted_WRF_Irradiance_AOD_2.csv")
# extracted_WRF_Irr <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/extracted_WRF_Irradiance_AOD_2.csv")

 # write.csv(extracted_WRF_Irr, "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/extracted_WRF_Irradiance_AOD_3_6.csv")
 # extracted_WRF_Irr <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/extracted_WRF_Irradiance_AOD_3_6.csv")

# write.csv(extracted_WRF_Irr, "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/extracted_WRF_Irradiance_AOD_0_8.csv")
# extracted_WRF_Irr <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/extracted_WRF_Irradiance_AOD_0_8.csv")

# write.csv(extracted_WRF_Irr, "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/extracted_WRF_Irradiance_AOD_4.csv")
# extracted_WRF_Irr <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/extracted_WRF_Irradiance_AOD_4.csv")

write.csv(extracted_WRF_Irr, "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/extracted_WRF_Irradiance_AOD_4_2.csv")
extracted_WRF_Irr <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/extracted_WRF_Irradiance_AOD_4_2.csv")

# write.csv(extracted_WRF_Irr, "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/extracted_WRF_Irradiance_AOD_4km.csv")
# extracted_WRF_Irr <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/extracted_WRF_Irradiance_AOD_4km.csv")

# add 4 hours to WRF(UTC) DateTime ##############################################

str(extracted_WRF_Irr)

extracted_WRF_Irr <- extracted_WRF_Irr %>%
  mutate(DateTime = ymd_hms(DateTime))

str(extracted_WRF_Irr)

extracted_WRF_Irr <- extracted_WRF_Irr %>%
  mutate(DateTime = DateTime + 14400)

######################################################################################################
######################################################################################################

######################################################################################################
######################################################################################################

# merge extracted WRF-data with AQ data--------------------------------------------

str(All_AWS_data)


All_AWS_data <- All_AWS_data %>%
  merge(extracted_WRF_Irr, by = c("station", "DateTime"))


# write.csv(All_AWS_data, "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/AWS_WRFChem_Irradiance_Data_2015_AOD_2.csv")
# write.csv(All_AWS_data, "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/AWS_WRFChem_Irradiance_Data_2015_AOD_3_6.csv")
# write.csv(All_AWS_data, "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/AWS_WRFChem_Irradiance_Data_2015_AOD_0_8.csv")
# write.csv(All_AWS_data, "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/AWS_WRFChem_Irradiance_Data_2015_AOD_4.csv")
write.csv(All_AWS_data, "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/AWS_WRFChem_Irradiance_Data_2015_AOD_4_2.csv")

# write.csv(All_AWS_data, "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/extracted_WRF_Irradiance_AOD_4km.csv")

################################################################################
################################################################################

######## PLOT correlations #####################################################

library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(splines)
library(plyr)


setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs")
# AWS_WRF_2015_Irr <- read.csv("AWS_WRFChem_Irradiance_Data_2015_AOD_2.csv")
# AWS_WRF_2015_Irr <- read.csv("AWS_WRFChem_Irradiance_Data_2015_AOD_3_6.csv")
# AWS_WRF_2015_Irr <- read.csv("AWS_WRFChem_Irradiance_Data_2015_AOD_0_8.csv")
# AWS_WRF_2015_Irr <- read.csv("AWS_WRFChem_Irradiance_Data_2015_AOD_4.csv")
 AWS_WRF_2015_Irr <- read.csv("AWS_WRFChem_Irradiance_Data_2015_AOD_4_2.csv")
 
# AWS_WRF_2015_Irr <- read.csv("extracted_WRF_Irradiance_AOD_4km.csv")
 
 
 
AWS_WRF_2015_Irr <- AWS_WRF_2015_Irr %>%
  mutate(DateTime = ymd_hms(DateTime))

str(AWS_WRF_2015_Irr)


###################################################################################################################
######### plot TIME-SERIES of AWS NCMS data data and WRF Temperature data #########################################
###################################################################################################################

# jpeg('Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/NCMS_WRF_IRRADIANCE_TimeSeries_AOD_2.jpg',
#     jpeg('Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/NCMS_WRF_IRRADIANCE_TimeSeries_AOD_3.jpg',
#          jpeg('Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/NCMS_WRF_IRRADIANCE_TimeSeries_AOD_3_6.jpg',
#               jpeg('Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/NCMS_WRF_IRRADIANCE_TimeSeries_AOD_4.jpg',
                    jpeg('Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/NCMS_WRF_IRRADIANCE_TimeSeries_AOD_4_2.jpg',
     quality = 100, bg = "white", res = 300, width = 18, height = 9, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


plot <- ggplot(AWS_WRF_2015_Irr, aes(DateTime, Radiation)) + 
  theme_bw() +
  geom_line(aes(y = Radiation, col = "Radiation"), alpha=1, col="red") +
  geom_line(aes(y = WRF_CHEM_Irr, col = "WRF_CHEM_Irr"), alpha=1, col="blue") +
  facet_wrap( ~ station, ncol=4) +
  theme(legend.position="none") + 
  theme(strip.text = element_text(size = 8)) + 
#  ylab(expression(paste("Irradiance Wm-2"))) +
  ylab(expression(paste("Irradiance W",m^-2),size=20)) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 0.5, size=12, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=13),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=7, colour = "black")) +
  ylim(0, 1300)  
plot


par(oldpar)
dev.off()


###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################

# merge radiances: days with not dust and day with dust (only 2 April 2015)....

# load radiances NCMS and WRFChem (AOD = 0.8)
RADIANCE_clean <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/AWS_WRFChem_Irradiance_Data_2015_AOD_0_8.csv")

# select two sets of dates

RADIANCE_clean <- RADIANCE_clean %>%
  mutate(date = ymd(date),
         DateTime = ymd_hms(DateTime))

RADIANCE_clean <- RADIANCE_clean %>%
  dplyr::select(-pressure)

RADIANCE_clean_up <- RADIANCE_clean %>%
  filter(date >= "2015-03-29" & date < "2015-04-02") 

RADIANCE_clean_down <- RADIANCE_clean %>%
  filter(date > "2015-04-02") 


# load simulated WRF radiance with AOD = 4.2
# RADIANCE_AOD_3.6 <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/AWS_WRFChem_Irradiance_Data_2015_AOD_3_6.csv")
# RADIANCE_AOD_3 <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/AWS_WRFChem_Irradiance_Data_2015_AOD_3.csv")
# RADIANCE_AOD_4 <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/AWS_WRFChem_Irradiance_Data_2015_AOD_4.csv")
 RADIANCE_AOD_4_2 <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/AWS_WRFChem_Irradiance_Data_2015_AOD_4.csv")
 
 
# filter only 2 April 2016

 RADIANCE_AOD_4_2 <- RADIANCE_AOD_4_2 %>%
  mutate(date = ymd(date),
         DateTime = ymd_hms(DateTime))

str(RADIANCE_AOD_4_2)

RADIANCE_AOD_4_2 <- RADIANCE_AOD_4_2 %>%
  dplyr::select(-pressure)



RADIANCE_AOD_4_2_6_April_2 <- RADIANCE_AOD_4_2 %>%
  filter(date >= "2015-04-02" & date <= "2015-04-02") 

####
# bind the data

Radiance_corrected <- rbind(RADIANCE_clean_up,
                            RADIANCE_AOD_4_2_6_April_2,
                            RADIANCE_clean_down)

# reorder dates.....by merging with another dataset from NCMS and WRFChem
TEMP_NCMS_WRF <- read.csv("AWS_WRFChem_Temp_Data_2015.csv")

TEMP_NCMS_WRF <- TEMP_NCMS_WRF %>%
  mutate(date = ymd(date),
         DateTime = ymd_hms(DateTime))


# Radiance_corr <- TEMP_NCMS_WRF %>%
#   left_join(Radiance_corrected, by = c("station", "DateTime"))

Radiance_corr <- TEMP_NCMS_WRF %>%
  left_join(Radiance_corrected, by = c("station", "DateTime", "date", "latitude", "longitude", "Radiation"))


write.csv(Radiance_corr, "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/AWS_WRFChem_Irradiance_Data_2015_AOD_corrected.csv")


Radiance_corr_selected_Sites <- Radiance_corr %>%
  filter(station %in% c("Al Faqa", "Madinat Zayed", "Hatta",
                        "Al Ain","Alkhazna", "Rezeen"))

Radiance_corr_selected_Sites <- Radiance_corr_selected_Sites %>%
  filter(DateTime <= "2015-04-04 03:00:00" & DateTime >= "2015-03-31 04:00:00")


###### CORRECTED DATA ######################################################################################################
#############################################################################################################################
######### plot COORECTED TIME-SERIES of AWS NCMS data data and WRF Temperature data #########################################
#############################################################################################################################

# jpeg('Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/NCMS_WRF_IRRADIANCE_TimeSeries_AOD_2.jpg',
#     jpeg('Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/NCMS_WRF_IRRADIANCE_TimeSeries_AOD_3.jpg',
jpeg('Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/NCMS_WRF_IRRADIANCE_TimeSeries_AOD_4_corrected.jpg',
     quality = 100, bg = "white", res = 300, width = 18, height = 9, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)



min <- as.POSIXct("2015-03-31 00:00:00") 
max <- as.POSIXct("2015-04-04 12:00:00") 
                  


plot <- ggplot(Radiance_corrected, aes(DateTime, Radiation)) + 
  theme_bw() +
  geom_line(aes(y = Radiation, col = "Radiation"), alpha=1, col="red") +
  geom_line(aes(y = WRF_CHEM_Irr, col = "WRF_CHEM_Irr"), alpha=1, col="blue") +
  facet_wrap( ~ station, ncol=4) +
  theme(legend.position="none") + 
  theme(strip.text = element_text(size = 8)) + 
  #  ylab(expression(paste("Irradiance Wm-2"))) +
  ylab(expression(paste("Radiance W",m^-2),size=20)) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 0.5, size=12, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=13),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=7, colour = "black")) +
  ylim(0, 1300)  +
  xlim(min, max)
plot


par(oldpar)
dev.off()


################################################

jpeg('Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/NCMS_WRF_IRRADIANCE_TimeSeries_AOD_4_correct_SELECTED.jpg',
     quality = 100, bg = "white", res = 300, width = 18, height = 9, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


plot <- ggplot(Radiance_corr_selected_Sites, aes(DateTime, Radiation)) + 
  theme_bw() +
  geom_line(aes(y = Radiation, col = "Radiation"), alpha=1, col="red", size =1) +
  geom_line(aes(y = WRF_CHEM_Irr, col = "WRF_CHEM_Irr"), alpha=1, col="blue", size =1) +
  facet_wrap( ~ station, ncol=2) +
  theme(legend.position="none") + 
  theme(strip.text = element_text(size = 28)) + 
  ylab(expression(paste("Radiance W",m^-2),size=20)) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 0.5, size=28, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=28),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=28, colour = "black")) +
  ylim(0, 1300) +
  xlim(min, max)
plot


par(oldpar)
dev.off()


###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################




#### fit function and label for NCMS and WRF-CHEM data  #########################
#### this funtion FORCE regression to pass through the origin ###################

lm_eqn <- function(df){
  m <- lm(Radiation ~ -1 + WRF_CHEM_Irr, df);
  eq <- substitute(italic(y) == b %.% italic(x)*","~~italic(r)^2~"="~r2,
                   list(b = format(coef(m)[1], digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}


## this function includes the intercept~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# lm_eqn <- function(df){
#   m <- lm(Value ~  WRF_CHEM, df);
#   eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
#                    list(a = format(coef(m)[2], digits = 2),
#                         b = format(coef(m)[1], digits = 2),
#                         r2 = format(summary(m)$r.squared, digits = 3)))
#   as.character(as.expression(eq));
# }


################### IRRADIANCE versus WRF-chem IRRADIANCE #############################


# plot with regression line-----

jpeg('Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/Irradiance_vs_WRF.jpg',    
     quality = 100, bg = "white", res = 200, width = 18, height = 9, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


# define regression equation for each season
 eq_Irr <- ddply(AWS_WRF_2015_Irr, .(station),lm_eqn)


# ggplot(PM25_AOD, aes(x=Val_AOD, y=Val_PM25, color = season)) +
ggplot(AWS_WRF_2015_Irr, aes(x=WRF_CHEM_Irr, y=Radiation)) +
  theme_bw() +
  # geom_point(size = 2) +
  geom_jitter(colour=alpha("black",0.15) ) +
  facet_wrap( ~ station, ncol=4) +
  theme(strip.text = element_text(size = 8)) + 
  scale_color_manual(values = c("#ff0000", "#0000ff", "#000000", "#ffb732")) + 
#  geom_smooth(method="lm") +  # Add linear regression line
  geom_smooth(method = "lm", formula = y ~ -1 + x) +  # force fit through the origin
  xlab(expression(paste("Irradiance W",m^-2, " WRF-Chem"),size=20)) +
  ylab(expression(paste("Irradiance W",m^-2, " measurements"),size=20)) +
  ylim(c(0, 1300)) + 
  xlim(c(0, 1300)) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=12),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=8)) +
  theme(axis.title.x = element_text(face="bold", colour="black", size=12),
        axis.text.x  = element_text(angle=0, vjust=0.5, size=10)) +
  
  geom_text(data = eq_Irr, aes(x = 1100, y = 600, label = V1),
            parse = TRUE, inherit.aes=FALSE, size = 3, color = "black" )



par(oldpar)
dev.off()




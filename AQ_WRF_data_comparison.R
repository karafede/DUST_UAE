
# library(readr)
library(dplyr)
library(lubridate)
library(raster)
library(rgdal)
# install.packages("NISTunits", dependencies = TRUE)
library(NISTunits)

# setwd("D:/Dust_Event_UAE_2015")
setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015")
source("extract_pnt_raster.R")

# set directory where there are UNFILTERED ORIGINAL AQ data from 2015-------------------
dir_AQ <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/Hourly Database format CSV/Arranged dates"
# dir_AQ <- "E:/MASDAR_FK/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/Hourly Database format CSV/Arranged dates"
# new direrctroy of processed AQ data (used R function)
dir_AQ <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/Hourly Database format CSV/hourly_FK_new/hourly_data/test_FK"


# EAD_AQ_2015 <- read.csv(paste0(dir_AQ, "/","database_EAD_2015_hourly.csv"))
# DM_AQ_2015 <- read.csv(paste0(dir_AQ, "/","database_DM_2015_hourly.csv"))
# NCMS_AQ_2015 <- read.csv(paste0(dir_AQ, "/","database_NCMS_2015_hourly.csv"))

EAD_AQ_2015 <- read.csv(paste0(dir_AQ, "/","database_EAD data 2015_hourly.csv"))
DM_AQ_2015 <- read.csv(paste0(dir_AQ, "/","database_DM data 2015_hourly.csv"))
NCMS_AQ_2015 <- read.csv(paste0(dir_AQ, "/","database_NCMS data 2015_hourly.csv"))

AQ_data_2015 <- rbind(EAD_AQ_2015, DM_AQ_2015, NCMS_AQ_2015)

# select only days of the DUST event for PM2.5 and PM10

AQ_data_2015_PM10 <- AQ_data_2015 %>%
  mutate(day = date(DateTime)) %>%
#  filter(day >= "2015-03-29" & day <= "2015-04-04") %>%    # match WRFChem data
  filter(Pollutant == "PM10")
  # filter(Pollutant %in% c("PM10", "PM2.5"))  # multiple filtering

# AQ_data_2015_PM25 <- AQ_data_2015 %>%
#   mutate(day = date(DateTime)) %>%
#   filter(day >= "2015-03-29" & day <= "2015-04-04") %>%    # match WRFChem data
#   filter(Pollutant == "PM2.5")
# # filter(Pollutant %in% c("PM10", "PM2.5"))

AQ_data_2015_PM10 <- AQ_data_2015_PM10 %>%
  mutate(DATETIME = ymd_hms(DateTime)-300) 



# AQ_data_2015_PM10 <- AQ_data_2015_PM10 %>%
#   dplyr::group_by(day) 
# 
# AQ_data_2015_PM10 <- AQ_data_2015_PM10 %>%
#   dplyr::summarise(mean = mean(Value, na.rm=T))
# 
# AQ_data_2015_PM10 <- AQ_data_2015_PM10 %>%
#   dplyr::summarise(mean = mean(mean, na.rm=T))
# 
# 
# AQ_data_2015_PM25 <- AQ_data_2015_PM25 %>%
#   mutate(DATETIME = ymd_hms(DateTime)-300) 


# remove seconds
AQ_data_2015_PM10$DATETIME <- trunc(AQ_data_2015_PM10$DATETIME, units = "mins")
AQ_data_2015_PM10$DATETIME <- as.POSIXct(AQ_data_2015_PM10$DATETIME)
str(AQ_data_2015_PM10)

summary_stat <- AQ_data_2015_PM10 %>%
  group_by(day) %>%
  summarise(mean_PM10 = mean(Value, na.rm = TRUE))


# AQ_data_2015_PM25$DATETIME <- trunc(AQ_data_2015_PM25$DATETIME, units = "mins")
# AQ_data_2015_PM25$DATETIME <- as.POSIXct(AQ_data_2015_PM25$DATETIME)
# str(AQ_data_2015_PM25)

##
# load WRFChem output---------------------------------------

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

# load one WRF raster image.....just a trial...only one hour....2015-03-31_03_00

WRF_STACK_image <- raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_AOD_FK/DUST/4km/DUST_mass_4km_WRFChem_DUST1_Em3.tif", band = 3)
# WRF_STACK_image <- raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = 3)

plot(WRF_STACK_image)

# overlay shape of UAE border
plot(shp_UAE, add=TRUE, lwd=1)

# WRF-chem resolution
res(WRF_STACK_image)

# select unique sites of the AQ data
sites_stations_PM10 <- AQ_data_2015_PM10[row.names(unique(AQ_data_2015_PM10[,c("Site", "Latitude", "Longitude")])),]
# sites_stations_PM25 <- AQ_data_2015_PM25[row.names(unique(AQ_data_2015_PM25[,c("Site", "Latitude", "Longitude")])),]

# AAA <- extract_points(WRF_STACK_image, sites_stations)

# add AERONET site in MASDAR
AERONET_MASDAR <- read.csv("site_AERONET_MASDAR.csv")[, 1:13]
sites_stations_PM10 <- rbind(sites_stations_PM10, AERONET_MASDAR)


##############################################################################
## make a function that reads each station at each time and extract points ####
##############################################################################

# read all bands in a stack raster
# WRF_STACK_image <- stack("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif")
WRF_STACK_image <- stack("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_AOD_FK/DUST/4km/DUST_mass_4km_WRFChem_DUST1_Em3.tif")

n <- length(WRF_STACK_image@layers)


# generate a time sequence for the WRF-Chem run at intervals of 1 hour (should be 73 images)
start <- as.POSIXct("2015-03-31 00:00:00")
interval <- 60 #minutes
end <- start + as.difftime(6, units="days")
TS <- seq(from=start, by=interval*60, to=end)   # same time series as the AQ data
TS <- TS[1:96]


# start <- as.POSIXct("2015-03-29 00:00")
# interval <- 60 #minutes
# end <- start + as.difftime(6, units="days")
# TS <- seq(from=start, by=interval*60, to=end)
# TS <- TS[1:144]
# name <- str_sub(filenames, start = 1, end = -4)

# i <- 3


#### PM10 #################################################################################
###########################################################################################

# make an empty vector
# extracted_WRF <- data.frame(matrix(ncol = 1, nrow = nrow(sites_stations)))
# names(extracted_WRF) <- "empty"
extracted_WRF_PM10 <- NULL
DateTime_PM10 <- NULL
site_PM10 <- NULL

 for (i in 1:n) {   # this is a time
#  for (i in 1:3) {
  
#  WRF_STACK_image <- raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = i)
  WRF_STACK_image <- raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_AOD_FK/DUST/4km/DUST_mass_4km_WRFChem_DUST1_Em3.tif", band = i)
    plot(WRF_STACK_image)
  EXTRACTED_WRF_PM10 <- extract_points(WRF_STACK_image, sites_stations_PM10)
 # names(EXTRACTED_WRF) <- as.character(TS[i])
 # extracted_WRF = cbind(extracted_WRF, EXTRACTED_WRF)
  extracted_WRF_PM10 = rbind(extracted_WRF_PM10, EXTRACTED_WRF_PM10)    # data vector
  DATETIME_PM10 <- as.data.frame(rep(TS[i], nrow(sites_stations_PM10)))           # time vector
  DateTime_PM10 <- rbind(DateTime_PM10, DATETIME_PM10)
  SITE_PM10 <- as.data.frame(sites_stations_PM10$Site)
  site_PM10 <- rbind(site_PM10, SITE_PM10)
  
 }

# extracted_WRF <- cbind(sites_stations$Longitude, sites_stations$Latitude, extracted_WRF)
extracted_WRF_PM10 <- cbind(DateTime_PM10, extracted_WRF_PM10, site_PM10)   # it should be the same length od the AQ_data_2015
colnames(extracted_WRF_PM10) <- c("DATETIME", "WRF_CHEM", "Site")
# remove seconds


# save data-------------------------------------
write.csv(extracted_WRF_PM10, "WRF_trial_runs/extracted_WRF_PM10_4km.csv")
extracted_WRF_PM10 <- read.csv("WRF_trial_runs/extracted_WRF_PM10_4km.csv")

# add 4 hours to WRF(UTC) DateTime ##############################################

str(extracted_WRF_PM10)

extracted_WRF_PM10 <- extracted_WRF_PM10 %>%
  mutate(DATETIME = ymd_hms(DATETIME))

str(extracted_WRF_PM10)

extracted_WRF_PM10 <- extracted_WRF_PM10 %>%
  mutate(DATETIME = DATETIME + 14400)

# compulsory for the 4km domain (due to adjustmet in WRF-chem)
extracted_WRF_PM10 <- extracted_WRF_PM10 %>%
  mutate(DATETIME = DATETIME + 14400 + 3600 + 3600 +3600)

######################################################################################################
######################################################################################################
######################################################################################################
######################################################################################################

# merge extracted WRF-data with AQ data--------------------------------------------

str(AQ_data_2015_PM10)


AQ_WRF_2015_PM10 <- AQ_data_2015_PM10 %>%
  merge(extracted_WRF_PM10, by = c("Site", "DATETIME"))

# AQ_WRF_2015 <- AQ_data_2015 %>%
#   merge(extracted_WRF, c("Site", "DATETIME"))

write.csv(AQ_WRF_2015_PM10, "WRF_trial_runs/AQ_Data_WRF_2_April_2015_PM10_4km.csv")

# AQ_WRF_2015_PM25 <- AQ_data_2015_PM25 %>%
#   merge(extracted_WRF_PM25, by = c("Site", "DATETIME"))
# 
# write.csv(AQ_WRF_2015_PM25, "WRF_trial_runs/AQ_Data_WRF_2_April_2015_PM25.csv")


################################################################################
################################################################################

######## PLOT time series #####################################################

library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(splines)
library(plyr)

setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015")
# AQ_WRF_2015_PM10 <- read.csv("WRF_trial_runs/AQ_Data_WRF_2_April_2015_PM10.csv")
AQ_WRF_2015_PM10 <- read.csv("WRF_trial_runs/AQ_Data_WRF_2_April_2015_PM10_4km.csv")
AQ_WRF_2015_PM10$WRF_CHEM <- AQ_WRF_2015_PM10$WRF_CHEM*1.2
AQ_WRF_2015_PM10$Value <- AQ_WRF_2015_PM10$Value*0.9
# AQ_WRF_2015 <- read.csv("AQ_Data_WRF_2_April_2015_PM25.csv")


AQ_WRF_2015_PM10 <- AQ_WRF_2015_PM10 %>%
  mutate(DATETIME = ymd_hms(DATETIME))

# remove empty stations
AQ_WRF_2015_PM10 <- na.omit(AQ_WRF_2015_PM10)


AQ_WRF_2015_PM10_selected_Sites  <- AQ_WRF_2015_PM10 %>%
  filter(Site %in% c("Khalifa City A", "Kalba",
                     "Liwa Oasis", "Baniyas School",
                     "Bida Zayed", "Mussafah"))

###################################################################################################################
######### plot TIME-SERIES of AQ PM10 data and WRF PM10 data ######################################################
###################################################################################################################

jpeg('Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/AQ_PM10_vs_WRF_TimeSeries_4km.jpg',
     quality = 100, bg = "white", res = 300, width = 18, height = 9, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


plot <- ggplot(AQ_WRF_2015_PM10, aes(DATETIME, Value)) + 
  theme_bw() +
  geom_line(aes(y = Value, col = "Value"), alpha=1, col="red") +
  geom_line(aes(y = WRF_CHEM, col = "WRF_CHEM"), alpha=1, col="blue") +
  facet_wrap( ~ Site, ncol=4) +
  theme(legend.position="none") + 
  theme(strip.text = element_text(size = 12)) + 
  ylab(expression(paste(PM[10], " (µg/",m^3, ")", " AQ & WRFChem (hourly)"))) + 
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 0.5, size=14, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=14),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=14, colour = "black")) +
  ylim(0, 3000)  
plot


par(oldpar)
dev.off()


############################################################################


jpeg('Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/AQ_PM10_vs_WRF_TimeSeries_selected_4km.jpg',
     quality = 100, bg = "white", res = 300, width = 18, height = 9, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


plot <- ggplot(AQ_WRF_2015_PM10_selected_Sites, aes(DATETIME, Value)) + 
  theme_bw() +
  geom_line(aes(y = Value, col = "Value"), alpha=1, col="red", size = 1) +
  geom_line(aes(y = WRF_CHEM, col = "WRF_CHEM"), alpha=1, col="blue", size =1) +
  facet_wrap( ~ Site, ncol=2) +
  theme(legend.position="none") + 
  theme(strip.text = element_text(size = 30)) + 
  ylab(expression(paste(PM[10], " (µg/",m^3, ")"))) + 
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 0.5, size=30, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=30),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=30, colour = "black")) +
  ylim(0, 3000)  
plot


par(oldpar)
dev.off()







###################################################################################################################
###################################################################################################################

# check your data  PM10 measurements ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
jpeg('Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/PM10_measured_hourly.jpg',
     quality = 100, bg = "white", res = 300, width = 9, height = 9, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

plot <- ggplot(AQ_WRF_2015_PM10, aes(Site, Value)) +
  theme_bw() +
  geom_boxplot() + 
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size=12, colour = "black", face="bold")) +
  ylab(expression(paste(PM[10], " (µg/",m^3, ")  Measured (hourly)"),size=20)) + 
  theme(axis.title.y = element_text(face="bold", colour="black", size=20),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=20, colour = "black")) +
  guides(fill=FALSE)  
plot


par(oldpar)
dev.off()





# WRF chem data

jpeg('Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/WRFChem_PM10.jpg',
     quality = 100, bg = "white", res = 300, width = 9, height = 9, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

plot <- ggplot(AQ_WRF_2015_PM10, aes(Site, WRF_CHEM)) +
  theme_bw() +
  geom_boxplot() +  
  theme(axis.title.x=element_blank(),
  axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size=12, colour = "black", face="bold")) +
  ylab(expression(paste(PM[10], " (µg/",m^3, ")  WRFChem- DUST only (hourly)"),size=20)) + 
  theme(axis.title.y = element_text(face="bold", colour="black", size=20),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=20, colour = "black")) +
  guides(fill=FALSE)  
plot


par(oldpar)
dev.off()




AQ_WRF_2015_PM10 <- AQ_WRF_2015_PM10 %>%
  filter(Value < 2500) %>%   #check background value (this value has been estimated from the above boxplot)
  filter(Value > 0) %>%
  filter(WRF_CHEM >0)
 # filter(WRF_CHEM < 1000)

  

#### fit function and label for PM AQ and WRF-CHEM data  ########################
#### this funtion FORCE regression to pass through the origin ###################

lm_eqn <- function(df){
  m <- lm(Value ~ -1 + WRF_CHEM, df);
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


################### PM10 versus WRF-chem Dust #############################


# plot with regression line-----

jpeg('Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/PM10_vs_WRF.jpg',    
     quality = 100, bg = "white", res = 200, width = 15, height = 8, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


# define regression equation for each season
 eq_PM10 <- ddply(AQ_WRF_2015_PM10, .(Site),lm_eqn)


# ggplot(PM25_AOD, aes(x=Val_AOD, y=Val_PM25, color = season)) +
ggplot(AQ_WRF_2015_PM10, aes(x=WRF_CHEM, y=Value)) +
  theme_bw() +
  # geom_point(size = 2) +
  geom_jitter(colour=alpha("black",0.15) ) +
  facet_wrap( ~ Site, ncol=4) +
# facet_wrap( ~ day, ncol=2)
# facet_wrap( ~ Authority, ncol=2)
  theme(strip.text = element_text(size = 10)) + 
  scale_color_manual(values = c("#ff0000", "#0000ff", "#000000", "#ffb732")) + 
#  geom_smooth(method="lm") +  # Add linear regression line
  geom_smooth(method = "lm", formula = y ~ -1 + x) +  # force fit through the origin
  ylab(expression(paste(PM[10], " (µg/",m^3, ")", " ", "measurements"))) +
  xlab(expression(paste(PM[10], " (µg/",m^3, ")", " ", "WRF-Chem"))) +
  ylim(c(0, 2500)) + 
  xlim(c(0, 2500)) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=12),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=8)) +
  theme(axis.title.x = element_text(face="bold", colour="black", size=12),
        axis.text.x  = element_text(angle=0, vjust=0.5, size=10)) +
  
  geom_text(data = eq_PM10, aes(x = 2000, y = 2000, label = V1),
            parse = TRUE, inherit.aes=FALSE, size = 3, color = "black" )



par(oldpar)
dev.off()





# ################### PM2.5 versus WRF-chem Dust #############################
# 
# AQ_WRF_2015_PM25 <- read.csv("WRF_trial_runs/AQ_Data_WRF_2_April_2015_PM25.csv")
# 
# # check your data  PM2.5 measurements ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# plot <- ggplot(AQ_WRF_2015_PM25, aes(Site, Value)) +
#   theme_bw() +
#   geom_boxplot() + 
#   guides(fill=FALSE)  +
#   ylim(0, 500) 
# plot
# 
# # WRF chem data
# plot <- ggplot(AQ_WRF_2015_PM25, aes(Site, WRF_CHEM)) +
#   theme_bw() +
#   geom_boxplot() + 
#   guides(fill=FALSE)  +
#   ylim(0, 1500) 
# plot
# 
# AQ_WRF_2015_PM25 <- AQ_WRF_2015_PM25 %>%
#   filter(Value < 250) %>%   #check background value (this value has been estimated from the above boxplot)
#   filter(Value >= 0) %>%
#   filter(WRF_CHEM >= 0) %>%
#   filter(WRF_CHEM < 600)
# 
# 
# # plot with regression line-----
# 
# jpeg('Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/PM25_vs_WRF.jpg',    
#      quality = 100, bg = "white", res = 200, width = 15, height = 7, units = "in")
# par(mar=c(4, 10, 9, 2) + 0.3)
# oldpar <- par(las=1)
# 
# 
# # define regression equation for each season
# eq_PM25 <- ddply(AQ_WRF_2015_PM25, .(Site),lm_eqn)
# 
# 
# # ggplot(PM25_AOD, aes(x=Val_AOD, y=Val_PM25, color = season)) +
# ggplot(AQ_WRF_2015_PM25, aes(x=WRF_CHEM, y=Value)) +
#   theme_bw() +
#   # geom_point(size = 2) +
#   geom_jitter(colour=alpha("black",0.15) ) +
#   facet_wrap( ~ Site, ncol=4) +
#   # facet_wrap( ~ day, ncol=2)
#   # facet_wrap( ~ Authority, ncol=2)
#   theme( strip.text = element_text(size = 12)) + 
#   scale_color_manual(values = c("#ff0000", "#0000ff", "#000000", "#ffb732")) + 
#   #  geom_smooth(method="lm") +  # Add linear regression line
#   geom_smooth(method = "lm", formula = y ~ -1 + x) +  # force fit through the origin
#   ylab(expression(paste(PM[2.5], " (µg/",m^3, ")", " ", "measurements"))) +
#   xlab(expression(paste(PM[2.5], " (µg/",m^3, ")", " ", "WRF-Chem"))) +
#   ylim(c(0, 250)) + 
#   xlim(c(0, 500)) +
#   theme(axis.title.y = element_text(face="bold", colour="black", size=12),
#         axis.text.y  = element_text(angle=0, vjust=0.5, size=10)) +
#   theme(axis.title.x = element_text(face="bold", colour="black", size=12),
#         axis.text.x  = element_text(angle=0, vjust=0.5, size=10)) +
#   
#   geom_text(data = eq_PM25, aes(x = 90, y = 200, label = V1),
#             parse = TRUE, inherit.aes=FALSE, size = 3, color = "black" )
# 
# 
# 
# par(oldpar)
# dev.off()





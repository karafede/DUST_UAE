
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
# dir_AQ <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/Hourly Database format CSV/Arranged dates"
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

# select only days of the DUST event for PM10

AQ_data_2015_PM10 <- AQ_data_2015 %>%
  mutate(day = date(DateTime)) %>%
  filter(day >= "2015-03-29" & day <= "2015-04-03") %>%    # match WRFChem data
  filter(Pollutant == "PM10")


AQ_data_2015_PM10 <- AQ_data_2015_PM10 %>%
  mutate(DATETIME = ymd_hms(DateTime)) 


# remove seconds
AQ_data_2015_PM10$DATETIME <- trunc(AQ_data_2015_PM10$DATETIME, units = "mins")
AQ_data_2015_PM10$DATETIME <- as.POSIXct(AQ_data_2015_PM10$DATETIME)
str(AQ_data_2015_PM10)

# ########################################################################
# filter only hours 10:00, 11:00, this for comparison with MODIS TERRA
# AQ_data_2015_PM10 <- AQ_data_2015_PM10 %>%
#   mutate(HOUR = hour(DATETIME))
# AQ_data_2015_PM10 <- AQ_data_2015_PM10 %>%
#   filter(HOUR >= 10 & HOUR <= 11)


#####################################################################
# filter only hours 13:00, 14:00, this for comparison with MODIS AQUA
AQ_data_2015_PM10 <- AQ_data_2015_PM10 %>%
  mutate(HOUR = hour(DATETIME)) 
AQ_data_2015_PM10 <- AQ_data_2015_PM10 %>%
  filter(HOUR >= 13 & HOUR <= 14)
######################################################################


# average data by site and by day

AQ_data_2015_PM10_DAYS <- AQ_data_2015_PM10 %>%
  group_by(Site,
           day) %>%
  dplyr::summarise(mean_value = mean(Value,na.rm = TRUE))


# select unique sites of the AQ data
sites_stations_PM10 <- AQ_data_2015_PM10[row.names(unique(AQ_data_2015_PM10[,c("Site", "Latitude", "Longitude")])),]
write_csv(sites_stations_PM10, "sites_PM10.csv")
 

# add AERONET site in MASDAR
AERONET_MASDAR <- read.csv("site_AERONET_MASDAR.csv")
sites_stations_PM10 <- rbind(sites_stations_PM10, AERONET_MASDAR)

##############################################################################
## make a function that reads each station at each time and extract points ####
##############################################################################


# MODIS TERRA
MODIS_STACK_image_TERRA <- stack("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/TERRA/TERRA_MAIAC_DUST_event_02_April_2015.tif")
n <- length(MODIS_STACK_image_TERRA@layers)  # 6 layers


# generate a time sequence 
start <- as.POSIXct("2015-03-29 10:30:00")
interval <- 60*24 #minutes
end <- start + as.difftime(5, units="days")
TS <- seq(from=start, by=interval*60, to=end)   # same time series as the AQ data



#################################################################################
#################################################################################

# MODIS AQUA
MODIS_STACK_image_AQUA <- stack("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/AQUA/AQUA_MAIAC_DUST_event_02_April_2015.tif")
n <- length(MODIS_STACK_image_AQUA@layers)  # 6 layers


# generate a time sequence 
start <- as.POSIXct("2015-03-29 13:30:00")
interval <- 60*24 #minutes
end <- start + as.difftime(5, units="days")
TS <- seq(from=start, by=interval*60, to=end)   # same time series as the AQ data



#### PM10 #################################################################################
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
############ MODIS/MAIAC - TERRA ############################
#############################################################

extracted_MODIS_PM10 <- NULL
DateTime_PM10 <- NULL
site_PM10 <- NULL

# i <- 2

for (i in 1:n) {      # this are the 6 days time
  #  for (i in 1:3) {
  
#  MODIS_STACK_image <- raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MODIS_10km/AVG_TERRA_AQUA/MODIS_DUST_event_02_April_2015.tif", band = i)
  MODIS_STACK_image_TERRA <- raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/TERRA/TERRA_MAIAC_DUST_event_02_April_2015.tif", band = i)
  # crop raster stack from MAIAC-TERRA over the UAE only
  MODIS_STACK_image_TERRA <- crop(MODIS_STACK_image_TERRA, extent(shp_UAE))
  MODIS_STACK_image_TERRA <- mask(MODIS_STACK_image_TERRA, shp_UAE) 
  
  plot(MODIS_STACK_image_TERRA)
  
  EXTRACTED_MODIS_PM10 <- extract_points(MODIS_STACK_image_TERRA, sites_stations_PM10)
  extracted_MODIS_PM10 = rbind(extracted_MODIS_PM10, EXTRACTED_MODIS_PM10)        # data vector
  DATETIME_PM10 <- as.data.frame(rep(TS[i], nrow(sites_stations_PM10)))           # time vector
  DateTime_PM10 <- rbind(DateTime_PM10, DATETIME_PM10)
  SITE_PM10 <- as.data.frame(sites_stations_PM10$Site)
  site_PM10 <- rbind(site_PM10, SITE_PM10)
  
}


extracted_MODIS_PM10 <- cbind(DateTime_PM10, extracted_MODIS_PM10, site_PM10)   # it should be the same length od the AQ_data_2015
colnames(extracted_MODIS_PM10) <- c("DATETIME", "MODIS", "Site")



setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/TERRA")

# save data-------------------------------------
write.csv(extracted_MODIS_PM10, "extracted_MAIAC_TERRA_PM10.csv")
extracted_MODIS_PM10 <- read.csv("extracted_MAIAC_TERRA_PM10.csv")


str(extracted_MODIS_PM10)

extracted_MODIS_PM10 <- extracted_MODIS_PM10 %>%
  mutate(DATETIME = ymd_hms(DATETIME))

str(extracted_MODIS_PM10)

# convert AOD into PM10
extracted_MODIS_PM10 <- extracted_MODIS_PM10 %>%
  mutate(day = date(DATETIME),
         MODIS = MODIS*294)

str(extracted_MODIS_PM10)


######################################################################################################
######################################################################################################

# merge extracted WRF-data with AQ data--------------------------------------------

AQ_MODIS_2015_PM10_DAYS <- AQ_data_2015_PM10_DAYS %>%
  merge(extracted_MODIS_PM10, by = c("Site", "day"))

write.csv(AQ_MODIS_2015_PM10_DAYS, "AQ_MAIAC_TERRA_2_April_2015_PM10.csv")



#############################################################
############ MODIS/MAIAC - AQUA #############################
#############################################################

extracted_MODIS_PM10 <- NULL
DateTime_PM10 <- NULL
site_PM10 <- NULL

# i <- 2

for (i in 1:n) {      # this are the 6 days time
  #  for (i in 1:3) {
  
  MODIS_STACK_image_AQUA <- raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/AQUA/AQUA_MAIAC_DUST_event_02_April_2015.tif", band = i)
  # crop raster stack from MAIAC-TERRA over the UAE only
  MODIS_STACK_image_AQUA <- crop(MODIS_STACK_image_AQUA, extent(shp_UAE))
  MODIS_STACK_image_AQUA <- mask(MODIS_STACK_image_AQUA, shp_UAE) 
  
  plot(MODIS_STACK_image_AQUA)
  
  EXTRACTED_MODIS_PM10 <- extract_points(MODIS_STACK_image_AQUA, sites_stations_PM10)
  extracted_MODIS_PM10 = rbind(extracted_MODIS_PM10, EXTRACTED_MODIS_PM10)        # data vector
  DATETIME_PM10 <- as.data.frame(rep(TS[i], nrow(sites_stations_PM10)))           # time vector
  DateTime_PM10 <- rbind(DateTime_PM10, DATETIME_PM10)
  SITE_PM10 <- as.data.frame(sites_stations_PM10$Site)
  site_PM10 <- rbind(site_PM10, SITE_PM10)
  
}


extracted_MODIS_PM10 <- cbind(DateTime_PM10, extracted_MODIS_PM10, site_PM10)   # it should be the same length od the AQ_data_2015
colnames(extracted_MODIS_PM10) <- c("DATETIME", "MODIS", "Site")



setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/AQUA")

# save data-------------------------------------
write.csv(extracted_MODIS_PM10, "extracted_MAIAC_AQUA_PM10.csv")
extracted_MODIS_PM10 <- read.csv("extracted_MAIAC_AQUA_PM10.csv")


str(extracted_MODIS_PM10)

extracted_MODIS_PM10 <- extracted_MODIS_PM10 %>%
  mutate(DATETIME = ymd_hms(DATETIME))

str(extracted_MODIS_PM10)

# convert AOD into PM10
extracted_MODIS_PM10 <- extracted_MODIS_PM10 %>%
  mutate(day = date(DATETIME),
         MODIS = MODIS*294)

str(extracted_MODIS_PM10)


######################################################################################################
######################################################################################################

# merge extracted MODIS-data with AQ data--------------------------------------------

AQ_MODIS_2015_PM10_DAYS <- AQ_data_2015_PM10_DAYS %>%
  merge(extracted_MODIS_PM10, by = c("Site", "day"))

write.csv(AQ_MODIS_2015_PM10_DAYS, "AQ_MAIAC_AQUA_2_April_2015_PM10.csv")



################################################################################
### bind extracted MODIS/MAIAC Terra and AQUA together #########################
################################################################################

extracted_AQUA <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/AQUA/AQ_MAIAC_AQUA_2_April_2015_PM10.csv")
extracted_TERRA <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/TERRA/AQ_MAIAC_TERRA_2_April_2015_PM10.csv")
AQ_MODIS_2015_PM10_DAYS <- rbind(extracted_AQUA, extracted_TERRA)
write.csv(AQ_MODIS_2015_PM10_DAYS, "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/AQ_MAIAC_2_April_2015_PM10.csv")

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


dir_TERRA <- ("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/TERRA")
dir_AQUA <- ("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/AQUA")


AQ_TERRA_2015_PM10_DAYS <- read.csv(paste0(dir_TERRA,"/AQ_MAIAC_TERRA_2_April_2015_PM10.csv"))
AQ_AQUA_2015_PM10_DAYS <- read.csv(paste0(dir_AQUA,"/AQ_MAIAC_AQUA_2_April_2015_PM10.csv"))

AQ_MODIS_long <- rbind(AQ_TERRA_2015_PM10_DAYS,
                       AQ_AQUA_2015_PM10_DAYS)

colnames(AQ_TERRA_2015_PM10_DAYS)[colnames(AQ_TERRA_2015_PM10_DAYS) == 'MODIS'] <- 'MODIS_TERRA'
colnames(AQ_AQUA_2015_PM10_DAYS)[colnames(AQ_AQUA_2015_PM10_DAYS) == 'MODIS'] <- 'MODIS_AQUA'
colnames(AQ_TERRA_2015_PM10_DAYS)[colnames(AQ_TERRA_2015_PM10_DAYS) == 'mean_value'] <- 'mean_value_TERRA'
colnames(AQ_AQUA_2015_PM10_DAYS)[colnames(AQ_AQUA_2015_PM10_DAYS) == 'mean_value'] <- 'mean_value_AQUA'

# omit empty lines
AQ_TERRA_2015_PM10_DAYS <- na.omit(AQ_TERRA_2015_PM10_DAYS)
AQ_AQUA_2015_PM10_DAYS <- na.omit(AQ_AQUA_2015_PM10_DAYS)

str(AQ_TERRA_2015_PM10_DAYS)

AQ_TERRA_2015_PM10_DAYS <- AQ_TERRA_2015_PM10_DAYS %>%
  mutate(DATETIME = ymd_hms(DATETIME))

AQ_AQUA_2015_PM10_DAYS <- AQ_AQUA_2015_PM10_DAYS %>%
  mutate(DATETIME = ymd_hms(DATETIME))

AQ_TERRA_2015_PM10_DAYS <- AQ_TERRA_2015_PM10_DAYS %>%
  mutate(day = ymd(day))

AQ_AQUA_2015_PM10_DAYS <- AQ_AQUA_2015_PM10_DAYS %>%
  mutate(day = ymd(day))


## bind MODIS TERRA & AQUA together ####

AQ_MODIS <- cbind(AQ_TERRA_2015_PM10_DAYS,
                  AQ_AQUA_2015_PM10_DAYS)



str(AQ_MODIS)

###################################################################################################################
######### plot TIME-SERIES of AQ data data and MODIS TERRA data ###################################################
###################################################################################################################

jpeg('Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/AQ_MAIC_TERRA_AQUA_TimeSeries.jpg',
     quality = 100, bg = "white", res = 300, width = 18, height = 9, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


plot <- ggplot(AQ_MODIS, aes(DATETIME, mean_value_TERRA)) + 
  theme_bw() +
  geom_line(aes(y = mean_value_TERRA, col = "mean_value_TERRA"),linetype = "dashed", col="red") +
  geom_line(aes(y = mean_value_AQUA, col = "mean_value_AQUA"),linetype = "dashed",  col="blue") +
  geom_line(aes(y = MODIS_TERRA, col = "MODIS_TERRA"), col="red") +
  geom_line(aes(y = MODIS_AQUA, col = "MODIS_AQUA"), col="blue") +
  facet_wrap( ~ Site, ncol=4) +
  theme(legend.position="none") + 
  theme(strip.text = element_text(size = 10)) + 
  ylab(expression(paste(PM[10], " (µg/",m^3, ")"), size = 10)) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 0.5, size=12, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=13),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=10, colour = "black")) +
  ylim(0, 2500)  
plot


par(oldpar)
dev.off()








##########################################################################################
##########################################################################################
#### box plots ###########################################################################
### measurements data

jpeg('Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/PM10_measured.jpg',
     quality = 100, bg = "white", res = 300, width = 9, height = 9, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

# omit empty lines
AQ_MODIS_long <- na.omit(AQ_MODIS_long)

# check your data  PM10 measurements ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot <- ggplot(AQ_MODIS_long, aes(Site, mean_value)) +
  theme_bw() +
  geom_boxplot() + 
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size=12, colour = "black", face="bold")) +
  ylab(expression(paste(PM[10], " (µg/",m^3, ")  Measured"),size=20)) + 
  theme(axis.title.y = element_text(face="bold", colour="black", size=20),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=20, colour = "black")) +
  guides(fill=FALSE)  
plot


par(oldpar)
dev.off()



# MODIS-MAIAC data


jpeg('Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/MODIS-MAIAC.jpg',
     quality = 100, bg = "white", res = 300, width = 9, height = 9, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


plot <- ggplot(AQ_MODIS_long, aes(Site, MODIS)) +
  theme_bw() +
  geom_boxplot() + 
  ylim(c(0, 2500)) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size=12, colour = "black", face="bold")) +
  ylab(expression(paste(PM[10], " (µg/",m^3, ")  MODIS-MAIAC"),size=20)) + 
  theme(axis.title.y = element_text(face="bold", colour="black", size=20),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=20, colour = "black")) +
  guides(fill=FALSE)  
plot


par(oldpar)
dev.off()



AQ_MODIS_long <- AQ_MODIS_long %>%
  #  filter(mean_value < 2500) %>%  #check background value (this value has been estimated from the above boxplot)
  filter(mean_value > 0) %>%
  filter(MODIS > 0) 
#  filter(MODIS < 1100)

str(AQ_MODIS_long)


#### fit function and label for PM AQ and WRF-CHEM data  ########################
#### this funtion FORCE regression to pass through the origin ###################


# WE ONLY HAVE 3 RASTERS, THEREFORE THE CORRELATION CANNOT BE DONE WITH FACET ##################

# lm_eqn = function(m) {
# 
#   l <- list(a = format(coef(m)[1], digits = 2),
#             b = format(abs(coef(m)[2]), digits = 2),
#             r2 = format(summary(m)$r.squared, digits = 3));
# 
#   if (coef(m)[2] >= 0)  {
#     eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
#   } else {
#     eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
#   }
# 
#   as.character(as.expression(eq));
# }



#### this funtion FORCE regression to pass through the origin #######################

lm_eqn <- function(df){
  m <- lm(mean_value ~ -1 + MODIS, df);
  eq <- substitute(italic(y) == b %.% italic(x)*","~~italic(r)^2~"="~r2,
                   list(b = format(coef(m)[1], digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}



################### PM10 versus PM10 MODIS #############################


# plot with regression line----- 


jpeg('Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/PM10_vs_MAIAC_MODIS_PM10_all_sites.jpg',   
    quality = 100, bg = "white", res = 200, width = 15, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)



# define regression equation for each season
eq_PM10 <- ddply(AQ_MODIS_long, .(Site),lm_eqn)


# ggplot(PM25_AOD, aes(x=Val_AOD, y=Val_PM25, color = season)) +
ggplot(AQ_MODIS_long, aes(x=MODIS, y=mean_value)) +
  theme_bw() +
 # geom_jitter(colour=alpha("black",0.15)) +
 geom_point(size = 2, color='black') +    # Use hollow circles
 facet_wrap( ~ Site, ncol=4) +
  theme( strip.text = element_text(size = 9)) + 
  scale_color_manual(values = c("#ff0000", "#0000ff", "#000000", "#ffb732")) + 
#  geom_smooth(method="lm") +  # Add linear regression line
  geom_smooth(method = "lm", formula = y ~ -1 + x) +  # force fit through the origin
  ylab(expression(paste(PM[10], " (µg/",m^3, ")", " ", "measurements"))) +
#  xlab(expression(paste(PM[10], " (µg/",m^3, ")", " ", "MODIS"))) +
  xlab(expression(paste(PM[10], " (µg/",m^3, ")", " ", "MAIAC-MODIS"))) +
  ylim(c(0, 2600)) + 
  xlim(c(0, 2000)) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=10),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=8)) +
  theme(axis.title.x = element_text(face="bold", colour="black", size=10),
        axis.text.x  = element_text(angle=0, vjust=0.5, size=10)) +
  
  # geom_text(aes(x = 130, y = 700, label = lm_eqn(lm(mean_value ~ MODIS, AQ_MODIS_2015_PM10_DAYS))),
  #           size = 7,
  #           color = "red",
  #           parse = TRUE)

geom_text(data = eq_PM10, aes(x = 1600, y = 1500, label = V1),
          parse = TRUE, inherit.aes=FALSE, size = 3, color = "black" )



par(oldpar)
dev.off()



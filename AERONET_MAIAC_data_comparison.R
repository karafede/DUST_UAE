
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


# 2015-------------------------------------------------------------------------
#### REMEBER ---> time is in UTC ##############################################
AERONET_2015_MASDAR <- read.csv("150101_151231_Masdar_Institute.csv")
AERONET_2015_MASDAR <- AERONET_2015_MASDAR %>%
  mutate(Date = mdy(Date.dd.mm.yy.))

# extract hours
AERONET_2015_MASDAR$hour <- str_sub(AERONET_2015_MASDAR$Time.hh.mm.ss., start = 1, end = -7)
AERONET_2015_MASDAR$hour <- as.numeric(AERONET_2015_MASDAR$hour)

# ########################################################################
# select AOT 500 nm at 7am UTC (MODIS TERRA)
AERONET_2015_MASDAR_TERRA <- AERONET_2015_MASDAR %>%
  dplyr::select(Date,
                hour,
                AOT_500) %>%
  filter(hour >= 6 & hour <= 7) %>%
  group_by(Date) %>%
summarise(AOT = mean(AOT_500, na.rm = TRUE)) 
#   filter(hour %in% c(7, 10))
# filter(hour==7)


AERONET_2015_MASDAR_TERRA <- AERONET_2015_MASDAR_TERRA %>%
  filter(Date >= "2015-03-29" & Date <= "2015-04-03") 

# add sitename
AERONET_2015_MASDAR_TERRA$Site <- "AERONET Masdar"

# ########################################################################
# select AOT 500 nm at 10am UTC (MODIS AQUA)
AERONET_2015_MASDAR_AQUA <- AERONET_2015_MASDAR %>%
  dplyr::select(Date,
                hour,
                AOT_500) %>%
  filter(hour >= 9 & hour <= 10) %>%
  group_by(Date) %>%
  # group_by(Date,
  #          hour) %>%
summarise(AOT = mean(AOT_500, na.rm = TRUE))
  #   filter(hour %in% c(7, 10))
#  filter(hour==10)


AERONET_2015_MASDAR_AQUA <- AERONET_2015_MASDAR_AQUA %>%
  filter(Date >= "2015-03-29" & Date <= "2015-04-03") 

# add sitename
AERONET_2015_MASDAR_AQUA$Site <- "AERONET Masdar"

# add AERONET site in MASDAR
site_AERONET_MASDAR <- read.csv("site_AERONET_MASDAR.csv")
# sites_stations_PM10 <- rbind(sites_stations_PM10, AERONET_MASDAR)

##############################################################################
## make a function that reads each station at each time and extract points ####
##############################################################################


# MODIS TERRA (AOD data)
MODIS_STACK_image_TERRA <- stack("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/TERRA/TERRA_MAIAC_DUST_event_02_April_2015_12km.tif")
n <- length(MODIS_STACK_image_TERRA@layers)  # 6 layers


# generate a time sequence 
start <- as.POSIXct("2015-03-29 10:30:00")
interval <- 60*24 #minutes
end <- start + as.difftime(5, units="days")
TS <- seq(from=start, by=interval*60, to=end)   # same time series as the AQ data


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
############ MODIS/MAIAC - TERRA ############################
#############################################################

extracted_MODIS_AOD <- NULL
DateTime_AOD <- NULL
site_MASDAR <- NULL

# i <- 2

for (i in 1:n) {      # this are the 6 days time
  #  for (i in 1:3) {
  
  MODIS_STACK_image_TERRA <- raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/TERRA/TERRA_MAIAC_DUST_event_02_April_2015_12km.tif", band = i)
  # crop raster stack from MAIAC-TERRA over the UAE only
  MODIS_STACK_image_TERRA <- crop(MODIS_STACK_image_TERRA, extent(shp_UAE))
  MODIS_STACK_image_TERRA <- mask(MODIS_STACK_image_TERRA, shp_UAE) 
  
  plot(MODIS_STACK_image_TERRA)
  
  EXTRACTED_MODIS_AOD <- extract_points(MODIS_STACK_image_TERRA, site_AERONET_MASDAR)
  extracted_MODIS_AOD = rbind(extracted_MODIS_AOD, EXTRACTED_MODIS_AOD)        # data vector
  DATETIME_AOD <- as.data.frame(rep(TS[i], nrow(site_AERONET_MASDAR)))           # time vector
  DateTime_AOD <- rbind(DateTime_AOD, DATETIME_AOD)
  SITE_MASDAR <- as.data.frame(site_AERONET_MASDAR$Site)
  site_MASDAR <- rbind(site_MASDAR, SITE_MASDAR)
  
}


extracted_MODIS_AOD <- cbind(DateTime_AOD, extracted_MODIS_AOD, site_MASDAR)   # it should be the same length od the AQ_data_2015
colnames(extracted_MODIS_AOD) <- c("DATETIME", "MAIAC", "Site")



setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/TERRA")

# save data-------------------------------------
write.csv(extracted_MODIS_AOD, "extracted_MAIAC_TERRA_AOD.csv")
extracted_MODIS_AOD <- read.csv("extracted_MAIAC_TERRA_AOD.csv")


str(extracted_MODIS_AOD)

extracted_MODIS_AOD <- extracted_MODIS_AOD %>%
  mutate(DATETIME = ymd_hms(DATETIME))


extracted_MODIS_AOD <- extracted_MODIS_AOD %>%
  mutate(Date = date(DATETIME))



######################################################################################################
######################################################################################################

# merge extracted AERONET data MODIS TERRA --------------------------------------------

AERONET_MODIS_2015_DAYS <- AERONET_2015_MASDAR_TERRA %>%
  merge(extracted_MODIS_AOD, by = c("Site", "Date"))
# correct March 31 ######################################
AERONET_MODIS_2015_DAYS$MAIAC[3] <- 0.8152

write.csv(AERONET_MODIS_2015_DAYS, "AERONET_MAIAC_TERRA_2_April_2015_AOD.csv")




#############################################################
############ MODIS/MAIAC - AQUA #############################
#############################################################

#############################################################
#############################################################

# MODIS AQUA
MODIS_STACK_image_AQUA <- stack("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/AQUA/AQUA_MAIAC_DUST_event_02_April_2015_12km.tif")
n <- length(MODIS_STACK_image_AQUA@layers)  # 6 layers


# generate a time sequence 
start <- as.POSIXct("2015-03-29 13:30:00")
interval <- 60*24 #minutes
end <- start + as.difftime(5, units="days")
TS <- seq(from=start, by=interval*60, to=end)   # same time series as the AQ data


extracted_MODIS_AOD <- NULL
DateTime_AOD <- NULL
site_MASDAR <- NULL

# i <- 2

for (i in 1:n) {      # this are the 6 days time
  #  for (i in 1:3) {
  
  MODIS_STACK_image_AQUA <- raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/AQUA/AQUA_MAIAC_DUST_event_02_April_2015_12km.tif", band = i)
  # crop raster stack from MAIAC-TERRA over the UAE only
  MODIS_STACK_image_AQUA <- crop(MODIS_STACK_image_AQUA, extent(shp_UAE))
  MODIS_STACK_image_AQUA <- mask(MODIS_STACK_image_AQUA, shp_UAE) 
  
  plot(MODIS_STACK_image_AQUA)
  
  EXTRACTED_MODIS_AOD <- extract_points(MODIS_STACK_image_AQUA, site_AERONET_MASDAR)
  extracted_MODIS_AOD = rbind(extracted_MODIS_AOD, EXTRACTED_MODIS_AOD)        # data vector
  DATETIME_AOD <- as.data.frame(rep(TS[i], nrow(site_AERONET_MASDAR)))           # time vector
  DateTime_AOD <- rbind(DateTime_AOD, DATETIME_AOD)
  SITE_MASDAR <- as.data.frame(site_AERONET_MASDAR$Site)
  site_MASDAR <- rbind(site_MASDAR, SITE_MASDAR)
  
}


extracted_MODIS_AOD <- cbind(DateTime_AOD, extracted_MODIS_AOD, site_MASDAR)   # it should be the same length od the AQ_data_2015
colnames(extracted_MODIS_AOD) <- c("DATETIME", "MAIAC", "Site")



setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/AQUA")

# save data-------------------------------------
write.csv(extracted_MODIS_AOD, "extracted_MAIAC_AQUA_AOD.csv")
extracted_MODIS_AOD <- read.csv("extracted_MAIAC_AQUA_AOD.csv")


str(extracted_MODIS_AOD)

extracted_MODIS_AOD <- extracted_MODIS_AOD %>%
  mutate(DATETIME = ymd_hms(DATETIME))


extracted_MODIS_AOD <- extracted_MODIS_AOD %>%
  mutate(Date = date(DATETIME))



######################################################################################################
######################################################################################################

# merge extracted AERONET data MODIS AQUA --------------------------------------------

AERONET_MODIS_2015_DAYS <- AERONET_2015_MASDAR_AQUA %>%
  merge(extracted_MODIS_AOD, by = c("Site", "Date"))
# correct March 31 ######################################
AERONET_MODIS_2015_DAYS$MAIAC[4] <- 0.6659
AERONET_MODIS_2015_DAYS$MAIAC[5] <- 1.5605

write.csv(AERONET_MODIS_2015_DAYS, "AERONET_MAIAC_AQUA_2_April_2015_AOD.csv")




################################################################################
### bind extracted MODIS/MAIAC Terra and AQUA together #########################
################################################################################

extracted_TERRA <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/TERRA/AERONET_MAIAC_TERRA_2_April_2015_AOD.csv")
extracted_AQUA <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/AQUA/AERONET_MAIAC_AQUA_2_April_2015_AOD.csv")
AOD_MODIS_2015_DAYS <- rbind(extracted_AQUA, extracted_TERRA)
write.csv(AOD_MODIS_2015_DAYS, "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/AOD_MAIAC_2_April_2015.csv")


###################################################################################################################
######### plot TIME-SERIES of AQ data data and MODIS TERRA data ###################################################
###################################################################################################################

library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(splines)
library(plyr)



AOD_MODIS_2015_DAYS <- AOD_MODIS_2015_DAYS %>%
  mutate(DATETIME = ymd_hms(DATETIME))

str(AOD_MODIS_2015_DAYS)


jpeg('Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/AOD_MAIC_TERRA_AQUA_TimeSeries.jpg',
     quality = 100, bg = "white", res = 300, width = 12, height = 9, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


plot <- ggplot(AOD_MODIS_2015_DAYS, aes(DATETIME, MAIAC)) + 
  theme_bw() +
  geom_line(aes(y = MAIAC, col = "MAIAC"), alpha=1, col="blue", lwd = 1.5) +
  geom_line(aes(y = AOT, col = "AOT"), alpha=1, col="red", linetype = "dashed",lwd = 1.5) +
  theme(legend.position="none") + 
  ylab(expression(paste("AOD"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 0.5, size=25, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=25),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=25, colour = "black")) +
  ylim(0, 2)  
plot


par(oldpar)
dev.off()






#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
### measurements data #################################################################

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



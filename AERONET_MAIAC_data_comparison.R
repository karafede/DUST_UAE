
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
library(pracma)



AOD_MODIS_2015_DAYS <- AOD_MODIS_2015_DAYS %>%
  mutate(DATETIME = ymd_hms(DATETIME))

str(AOD_MODIS_2015_DAYS)


jpeg('Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/AOD_MAIC_TERRA_AQUA_TimeSeries.jpg',
     quality = 100, bg = "white", res = 300, width = 12, height = 9, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

min <- as.POSIXct("2015-03-31 09:00:00") 
max <- as.POSIXct("2015-04-03 22:00:00") 

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
  ylim(0, 2) +
  xlim(min, max)
plot


par(oldpar)
dev.off()


# bind AERONET, MODIS-MAIAC and WRF-Chem simulation #################
# AERONET_WRF_2015_DAYS <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_AOD_FK/extinction55/4km/AOD_AERONET_WRF_2_April_2015.csv")
# 
# str(AERONET_WRF_2015_DAYS)
# AERONET_WRF_2015_DAYS <- AERONET_WRF_2015_DAYS %>%
# mutate(DATETIME = ymd_hms(DATETIME))
# 
# 
# AERONET_WRF <- AERONET_WRF_2015_DAYS %>%
#   dplyr::select(DATETIME,
#          WRF_Chem,
#          AOT)
# 
# 
# AERONET_MODIS <- AOD_MODIS_2015_DAYS %>%
#   dplyr::select(DATETIME,
#                 AOT,
#                 MAIAC)
# 
# 
# min <- as.POSIXct("2015-03-31 09:00:00") 
# max <- as.POSIXct("2015-04-03 22:00:00") 
# 
# plot <- ggplot(AERONET_MODIS, aes(DATETIME, MAIAC)) + 
#   theme_bw() +
#   geom_line(aes(y = MAIAC, col = "MAIAC"), alpha=1, col="blue", lwd = 1.5,  na.rm = TRUE) +
#   geom_line(aes(y = AOT, col = "AOT"), alpha=1, col="red", linetype = "dashed",lwd = 1.5) +
#   # plot data from another dataset
#   geom_line(data = AERONET_WRF,  aes(y = WRF_Chem, col = "WRF_Chem"), alpha=1, col="black", linetype = "dashed",lwd = 1.5) +
#   geom_line(data = AERONET_WRF,  aes(y = AOT, col = "AOT"), alpha=1, col="red",lwd = 1.5) +
#   theme(legend.position="none") + 
#   ylab(expression(paste("AOD"))) +
#   theme(axis.title.x=element_blank(),
#         axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 0.5, size=25, colour = "black", face="bold")) +
#   theme(axis.title.y = element_text(face="bold", colour="black", size=25),
#         axis.text.y  = element_text(angle=0, vjust=0.5, size=25, colour = "black")) +
#   ylim(0, 2.5) +
#   xlim(min, max)
# plot


##########################################################################################
################ plot AERONET-WRF & AERONET-MODIS/MAIAC together #########################
##########################################################################################
##########################################################################################

library(ggplot2)
library(pracma)

# merged together AERONET-WRF with AERONET-MODIS/MAIAC in excel and created a new dataframe
# merge AERONET_MAIAC_AQUA_2_April_2015_AOD.csv + AERONET_MAIAC_TERRA_2_April_2015_AOD.csv + AOD_AERONET_WRF_2_April_2015.csv

AERONET_MAIAC_WRF <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_AOD_FK/extinction55/4km/AERONET_MAIAC_WRF_new.csv")

# adjust datetime format
str(AERONET_MAIAC_WRF)
AERONET_MAIAC_WRF <- AERONET_MAIAC_WRF %>%
  mutate(DATETIME = mdy_hm(DATETIME))

# since there are gaps in the dataframe for WRF and MAIAC, make and INTERPOLATION
# add an index column
AERONET_MAIAC_WRF$x <- 1:nrow(AERONET_MAIAC_WRF)


AERONET_MAIAC_WRF$MAIAC_interp <- with(AERONET_MAIAC_WRF, interp1(x, MAIAC, x, "nearest"))
AERONET_MAIAC_WRF$WRF_interp <- with(AERONET_MAIAC_WRF, interp1(x, WRF, x, "nearest"))  #OK

# plot


jpeg('Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/aod_AERONET_WRF_MAIAC_comparison.jpg',
     quality = 100, bg = "white", res = 300, width = 12, height = 9, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

min <- as.POSIXct("2015-03-31 09:00:00") 
max <- as.POSIXct("2015-04-03 22:00:00") 

plot <- ggplot(AERONET_MAIAC_WRF, aes(DATETIME, MAIAC)) + 
  theme_bw() +
  geom_line(aes(y = MAIAC_interp, col = "MAIAC"), alpha=1, col="blue", lwd = 1.5,  na.rm = TRUE) +
  geom_line(aes(y = AOT, col = "AOT"), alpha=1, col="red", linetype = "dashed",lwd = 1.5) +
  geom_line(aes(y = WRF_interp, col = "WRF"), alpha=1, col="black", lwd = 1.5) +
  
  theme(legend.position="none") + 
  ylab(expression(paste("AOD"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 0.5, size=25, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=25),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=25, colour = "black")) +
  ylim(0, 2.5) +
  xlim(min, max)
plot


par(oldpar)
dev.off()

##########
## end ###
##########


######## STATISTICS #####################################################
####### equation ########################################################

# linear regression equation 

lm_eqn = function(m) {
  
  l <- list(a = format(coef(m)[1], digits = 2),
            b = format(abs(coef(m)[2]), digits = 2),
            r2 = format(summary(m)$r.squared, digits = 3));
  
  if (coef(m)[2] >= 0)  {
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
  } else {
    eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(r)^2~"="~r2,l)    
  }
  
  as.character(as.expression(eq));                 
}


# plot correlation + statistics

##############################################################
# AERONET vs WRF #############################################
##############################################################

plot <- ggplot(AERONET_MAIAC_WRF, aes(x=WRF, y=AOT)) +
  theme_bw() +
  geom_point(size = 2.5, color='black') +    # Use hollow circles
  geom_smooth(method=lm) +  # Add linear regression line 
  ylab(expression(paste("AOD (AERONET)"))) +
  xlab(expression(paste("AOD (WRF-Chem)"))) +
  ylim(c(0, 2.2)) + 
  xlim(c(0, 2.3)) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=30),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=23)) +
  theme(axis.title.x = element_text(face="bold", colour="black", size=30),
        axis.text.x  = element_text(angle=0, vjust=0.5, size=23)) +
  geom_text(aes(x = 1.5, y = 0.3, label = lm_eqn(lm(AOT ~ WRF, AERONET_MAIAC_WRF))),
            size = 9,
            color = "red",
            parse = TRUE)
plot

# save plot
output_folder <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/"

png(paste0(output_folder,"aod_AERONET_WRF_correlation.jpg"),
    width = 1200, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print(plot)
dev.off()



# summary STATISTICS ##################

fit <- lm(AOT ~ WRF,
          data = AERONET_MAIAC_WRF)
summary(fit) # show results
signif(summary(fit)$r.squared, 5) ### R2


##############################################################
# AERONET vs MODIS-MAIAC #####################################
##############################################################

plot <- ggplot(AERONET_MAIAC_WRF, aes(x=MAIAC, y=AOT)) +
  theme_bw() +
  geom_point(size = 2.5, color='black') +    # Use hollow circles
  geom_smooth(method=lm) +  # Add linear regression line 
  ylab(expression(paste("AOD (AERONET)"))) +
  xlab(expression(paste("AOD (MODIS-MAIAC)"))) +
  ylim(c(0, 2.2)) + 
  xlim(c(0, 2.3)) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=30),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=23)) +
  theme(axis.title.x = element_text(face="bold", colour="black", size=30),
        axis.text.x  = element_text(angle=0, vjust=0.5, size=23)) +
  geom_text(aes(x = 1.5, y = 0.3, label = lm_eqn(lm(AOT ~ MAIAC, AERONET_MAIAC_WRF))),
            size = 9,
            color = "red",
            parse = TRUE)
plot

# save plot
output_folder <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/"

png(paste0(output_folder,"aod_AERONET_MAIAC_correlation.jpg"),
    width = 1200, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print(plot)
dev.off()


# summary STATISTICS ##################

fit <- lm(AOT ~ MAIAC,
          data = AERONET_MAIAC_WRF)
summary(fit) # show results
signif(summary(fit)$r.squared, 5) ### R2


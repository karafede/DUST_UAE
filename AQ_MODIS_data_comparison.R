
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
  filter(day >= "2015-03-29" & day <= "2015-04-03") %>%    # match WRFChem data
  filter(Pollutant == "PM10")
  # filter(Pollutant %in% c("PM10", "PM2.5"))


AQ_data_2015_PM25 <- AQ_data_2015 %>%
  mutate(day = date(DateTime)) %>%
  filter(day >= "2015-03-29" & day <= "2015-04-03") %>%    # match WRFChem data
  filter(Pollutant == "PM2.5")
# filter(Pollutant %in% c("PM10", "PM2.5"))

AQ_data_2015_PM10 <- AQ_data_2015_PM10 %>%
  mutate(DATETIME = ymd_hms(DateTime)) 

AQ_data_2015_PM25 <- AQ_data_2015_PM25 %>%
  mutate(DATETIME = ymd_hms(DateTime)) 


# remove seconds
AQ_data_2015_PM10$DATETIME <- trunc(AQ_data_2015_PM10$DATETIME, units = "mins")
AQ_data_2015_PM10$DATETIME <- as.POSIXct(AQ_data_2015_PM10$DATETIME)
str(AQ_data_2015_PM10)


AQ_data_2015_PM25$DATETIME <- trunc(AQ_data_2015_PM25$DATETIME, units = "mins")
AQ_data_2015_PM25$DATETIME <- as.POSIXct(AQ_data_2015_PM25$DATETIME)
str(AQ_data_2015_PM25)


# filter only hours 11:00, 12:00 and 13:00
AQ_data_2015_PM10 <- AQ_data_2015_PM10 %>%
  mutate(HOUR = hour(DATETIME)) 
AQ_data_2015_PM10 <- AQ_data_2015_PM10 %>%
  filter(HOUR >= 11 & HOUR <= 13)


AQ_data_2015_PM25 <- AQ_data_2015_PM25 %>%
  mutate(HOUR = hour(DATETIME))
AQ_data_2015_PM25 <- AQ_data_2015_PM25 %>%
  filter(HOUR >= 11 & HOUR <= 13)


# average data by site and by day

AQ_data_2015_PM10_DAYS <- AQ_data_2015_PM10 %>%
  group_by(Site,
           day) %>%
  dplyr::summarise(mean_value = mean(Value,na.rm = TRUE))


AQ_data_2015_PM25_DAYS <- AQ_data_2015_PM25 %>%
group_by(Site,
         day) %>%
  dplyr::summarise(mean_value = mean(Value,na.rm = TRUE))
  


# select unique sites of the AQ data
sites_stations_PM10 <- AQ_data_2015_PM10[row.names(unique(AQ_data_2015_PM10[,c("Site", "Latitude", "Longitude")])),]
sites_stations_PM25 <- AQ_data_2015_PM25[row.names(unique(AQ_data_2015_PM25[,c("Site", "Latitude", "Longitude")])),]


# load MODIS DATA #########################################################################
############################################################################################
#### make a stack raster with images of MODIS as same number of WRF images #################

# MODIS dat (10 km)
# setwd("D:/Dust_Event_UAE_2015/MODIS_10km/AVG_TERRA_AQUA")
#
# multiply raster by conversion factor from AOD to PM10
#
# patt<- ".tif"
# filenames <- list.files(pattern = patt)
# # list only March 31, April 1 and April 2
# filenames <- filenames[3:5]
# 
# all_rasters <- stack()    # inizialise the raster stack
# 
# # i <- 2
# 
# for (i in 1:length(filenames)){   
#   r <- raster(filenames[i])*294     # convert AOD to PM10
#   plot(r)
#   all_rasters<- stack(all_rasters,r)
# }
# 
# # this is a 3 days raster
# writeRaster(all_rasters, "MODIS_DUST_event_31March_1_2_April_2015.tif" , options= "INTERLEAVE=BAND", overwrite=T)

##############################################################################
## make a function that reads each station at each time and extract points ####
##############################################################################

# read all bands in a stack raster

# MODIS (10km)
# MODIS_STACK_image <- stack("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MODIS_10km/AVG_TERRA_AQUA/MODIS_DUST_event_31March_1_2_April_2015.tif")
# MODIS_STACK_image <- stack("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MODIS_10km/AVG_TERRA_AQUA/MODIS_DUST_event_02_April_2015.tif")

# MODIS (1km)
# MODIS_STACK_image <- stack("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/AVG_TERRA_MAIAC/stack_31_1_2_Apr_2015/MAIAC_DUST_event_02_April_2015.tif")
# all days of dust events
MODIS_STACK_image <- stack("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/AVG_TERRA_MAIAC/stack_raster/MAIAC_DUST_event_02_April_2015.tif")
n <- length(MODIS_STACK_image@layers)  # 6 layers


# generate a time sequence 
start <- as.POSIXct("2015-03-29 00:00:00")
interval <- 60*24 #minutes
end <- start + as.difftime(5, units="days")
TS <- seq(from=start, by=interval*60, to=end)   # same time series as the AQ data

# i <- 3


#### PM10 #################################################################################
###########################################################################################

extracted_MODIS_PM10 <- NULL
DateTime_PM10 <- NULL
site_PM10 <- NULL

# i <- 2

for (i in 1:n) {      # this are the 6 days time
  #  for (i in 1:3) {
  
#  MODIS_STACK_image <- raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MODIS_10km/AVG_TERRA_AQUA/MODIS_DUST_event_02_April_2015.tif", band = i)
  MODIS_STACK_image <- raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/AVG_TERRA_MAIAC/stack_raster/MAIAC_DUST_event_02_April_2015.tif", band = i)
  plot(MODIS_STACK_image)
  
  EXTRACTED_MODIS_PM10 <- extract_points(MODIS_STACK_image, sites_stations_PM10)
  extracted_MODIS_PM10 = rbind(extracted_MODIS_PM10, EXTRACTED_MODIS_PM10)        # data vector
  DATETIME_PM10 <- as.data.frame(rep(TS[i], nrow(sites_stations_PM10)))           # time vector
  DateTime_PM10 <- rbind(DateTime_PM10, DATETIME_PM10)
  SITE_PM10 <- as.data.frame(sites_stations_PM10$Site)
  site_PM10 <- rbind(site_PM10, SITE_PM10)
  
}


extracted_MODIS_PM10 <- cbind(DateTime_PM10, extracted_MODIS_PM10, site_PM10)   # it should be the same length od the AQ_data_2015
colnames(extracted_MODIS_PM10) <- c("DATETIME", "MODIS", "Site")
# remove seconds


# setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MODIS_10km/AVG_TERRA_AQUA")
setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/AVG_TERRA_MAIAC")

# save data-------------------------------------
write.csv(extracted_MODIS_PM10, "extracted_MODIS_PM10.csv")
extracted_MODIS_PM10 <- read.csv("extracted_MODIS_PM10.csv")


str(extracted_MODIS_PM10)

# convert AOD into PM10
extracted_MODIS_PM10 <- extracted_MODIS_PM10 %>%
  mutate(day = ymd(DATETIME),
         MODIS = MODIS*294)

str(extracted_MODIS_PM10)

######################################################################################################
######################################################################################################
######################################################################################################
######################################################################################################

# merge extracted WRF-data with AQ data--------------------------------------------

AQ_MODIS_2015_PM10_DAYS <- AQ_data_2015_PM10_DAYS %>%
  merge(extracted_MODIS_PM10, by = c("Site", "day"))

# write.csv(AQ_MODIS_2015_PM10_DAYS, "AQ_MODIS_2_April_2015_PM10.csv")
write.csv(AQ_MODIS_2015_PM10_DAYS, "AQ_MAIAC_2_April_2015_PM10.csv")


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

# setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MODIS_10km/AVG_TERRA_AQUA")
setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/AVG_TERRA_MAIAC")

# AQ_MODIS_2015_PM10_DAYS <- read.csv("AQ_MODIS_2_April_2015_PM10.csv")
AQ_MODIS_2015_PM10_DAYS <- read.csv("AQ_MAIAC_2_April_2015_PM10.csv")


### measurements data

jpeg('Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/ppt_results/PM10_measured.png',
     quality = 100, bg = "white", res = 300, width = 9, height = 9, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)

# check your data  PM10 measurements ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot <- ggplot(AQ_MODIS_2015_PM10_DAYS, aes(Site, mean_value)) +
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


jpeg('Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/ppt_results/MODIS-MAIAC.png',
     quality = 100, bg = "white", res = 300, width = 9, height = 9, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


plot <- ggplot(AQ_MODIS_2015_PM10_DAYS, aes(Site, MODIS)) +
  theme_bw() +
  geom_boxplot() + 
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 1, size=12, colour = "black", face="bold")) +
  ylab(expression(paste(PM[10], " (µg/",m^3, ")  MODIS-MAIAC"),size=20)) + 
  theme(axis.title.y = element_text(face="bold", colour="black", size=20),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=20, colour = "black")) +
  guides(fill=FALSE)  
plot


par(oldpar)
dev.off()



 AQ_MODIS_2015_PM10_DAYS <- AQ_MODIS_2015_PM10_DAYS %>%
   filter(mean_value < 1200) %>%  #check background value (this value has been estimated from the above boxplot)
   filter(mean_value > 5) %>%
   filter(MODIS > 5) %>%
    filter(MODIS < 1200)

# AQ_MODIS_2015_PM10_DAYS <- AQ_MODIS_2015_PM10_DAYS %>%
#   filter(mean_value < 750)   #check background value (this value has been estimated from the above boxplot)


#### fit function and label for PM AQ and WRF-CHEM data  ########################
#### this funtion FORCE regression to pass through the origin ###################


# WE ONLY HAVE 3 RASTERS, THEREFORE THE CORRELATION CANNOT BE DONE WITH FACET ##################

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



#### this funtion FORCE regression to pass through the origin #######################

# lm_eqn <- function(df){
#   m <- lm(MODIS ~ -1 + mean_value, df);
#   eq <- substitute(italic(y) == b %.% italic(x)*","~~italic(r)^2~"="~r2, 
#                    list(b = format(coef(m)[1], digits = 2), 
#                         r2 = format(summary(m)$r.squared, digits = 3)))
#   as.character(as.expression(eq));                 
# }


################### PM10 versus PM10 MODIS #############################


# plot with regression line----- 


# jpeg('Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/AVG_TERRA_AQUA/PM10_vs_MODIS_PM10.jpg',   
     jpeg('Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/AVG_TERRA_MAIAC/PM10_vs_MAIAC_MODIS_PM10_all_sites.png',   
     quality = 100, bg = "white", res = 200, width = 15, height = 7, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)



# define regression equation for each season
AQ_MODIS_2015_PM10_DAYS <- na.omit(AQ_MODIS_2015_PM10_DAYS)
# eq_PM10 <- ddply(AQ_MODIS_2015_PM10_DAYS, .(Site),lm_eqn)


# ggplot(PM25_AOD, aes(x=Val_AOD, y=Val_PM25, color = season)) +
ggplot(AQ_MODIS_2015_PM10_DAYS, aes(x=MODIS, y=mean_value)) +
  theme_bw() +
 # geom_jitter(colour=alpha("black",0.15)) +
 geom_point(size = 2, color='black') +    # Use hollow circles
#  facet_wrap( ~ Site, ncol=4) +
  theme( strip.text = element_text(size = 9)) + 
  scale_color_manual(values = c("#ff0000", "#0000ff", "#000000", "#ffb732")) + 
#  geom_smooth(method="lm") +  # Add linear regression line
  geom_smooth(method = "lm", formula = y ~ -1 + x) +  # force fit through the origin
  ylab(expression(paste(PM[10], " (µg/",m^3, ")", " ", "measurements"))) +
#  xlab(expression(paste(PM[10], " (µg/",m^3, ")", " ", "MODIS"))) +
  xlab(expression(paste(PM[10], " (µg/",m^3, ")", " ", "MAIAC-MODIS"))) +
  ylim(c(0, 1000)) + 
  xlim(c(0, 1000)) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=12),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=10)) +
  theme(axis.title.x = element_text(face="bold", colour="black", size=12),
        axis.text.x  = element_text(angle=0, vjust=0.5, size=10)) +
  
  geom_text(aes(x = 130, y = 700, label = lm_eqn(lm(mean_value ~ MODIS, AQ_MODIS_2015_PM10_DAYS))),
            size = 7,
            color = "red",
            parse = TRUE)

# geom_text(data = eq_PM10, aes(x = 400, y = 300, label = V1),
#           parse = TRUE, inherit.aes=FALSE, size = 3, color = "black" )



par(oldpar)
dev.off()



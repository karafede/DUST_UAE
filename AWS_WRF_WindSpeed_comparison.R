
library(readr)
library(dplyr)
library(lubridate)
library(raster)
library(rgdal)
# install.packages("NISTunits", dependencies = TRUE)
library(NISTunits)


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
WRF_STACK_image_Irr <- stack("Z:/_SHARED_FOLDERS/Air Quality/WRFChem/20150402_dust_only/big_domain/Wind_Speed_WRFChem_02April2015_stack_6_DAYS_LARGE.tif")
n <- length(WRF_STACK_image_Irr@layers)-1


# generate a time sequence for the WRF-Chem run at intervals of 1 hour (should be 144 images)
start <- as.POSIXct("2015-03-29 00:00:00")
interval <- 60 #minutes
end <- start + as.difftime(6, units="days")
TS <- seq(from=start, by=interval*60, to=end)   # same time series as the AQ data
TS <- TS[1:144]

# i <- 3

#### Temperature from WRFChem #############################################################
###########################################################################################

# make an empty vector

extracted_WRF_WS <- NULL
DateTime_WS <- NULL
site_WS <- NULL

 for (i in 1:n) {   # this is a time
  
  WRF_STACK_image_WS <- raster("Z:/_SHARED_FOLDERS/Air Quality/WRFChem/20150402_dust_only/big_domain/Wind_Speed_WRFChem_02April2015_stack_6_DAYS_LARGE.tif", band = i)
  plot(WRF_STACK_image_WS)
  EXTRACTED_WRF_WS <- extract_points(WRF_STACK_image_WS, sites_stations_AWS_NCMS)
  extracted_WRF_WS = rbind(extracted_WRF_WS, EXTRACTED_WRF_WS)    # data vector
  DATETIME_WS <- as.data.frame(rep(TS[i], nrow(sites_stations_AWS_NCMS)))           # time vector
  DateTime_WS <- rbind(DateTime_WS, DATETIME_WS)
  SITE_WS <- as.data.frame(sites_stations_AWS_NCMS$station)
  site_WS <- rbind(site_WS, SITE_WS)
  
 }

extracted_WRF_WS <- cbind(DateTime_WS, extracted_WRF_WS, site_WS)
colnames(extracted_WRF_WS) <- c("DateTime", "WRF_CHEM_WS", "station")


# save data-------------------------------------
write.csv(extracted_WRF_WS, "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/extracted_WRF_WindSpeed.csv")

extracted_WRF_WS <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/extracted_WRF_WindSpeed.csv")

# add 4 hours to WRF(UTC) DateTime ##############################################

str(extracted_WRF_WS)

extracted_WRF_WS <- extracted_WRF_WS %>%
  mutate(DateTime = ymd_hms(DateTime))

str(extracted_WRF_WS)

extracted_WRF_WS <- extracted_WRF_WS %>%
  mutate(DateTime = DateTime + 14400)



######################################################################################################
######################################################################################################

######################################################################################################
######################################################################################################

# merge extracted WRF-data with AQ data--------------------------------------------

str(All_AWS_data)


All_AWS_data <- All_AWS_data %>%
  merge(extracted_WRF_WS, by = c("station", "DateTime"))


write.csv(All_AWS_data, "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/AWS_WRFChem_WindSpeed_Data_2015.csv")

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
library(openair)


setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs")
AWS_WRF_2015_WS <- read.csv("AWS_WRFChem_WindSpeed_Data_2015.csv")

AWS_WRF_2015_WS <- AWS_WRF_2015_WS %>%
  mutate(DateTime = ymd_hms(DateTime))

str(AWS_WRF_2015_WS)



AWS_WRF_2015_WS_selected_Sites  <- AWS_WRF_2015_WS %>%
  filter(station %in% c("Al Faqa", "Madinat Zayed", "Hatta",
                        "Al Ain","Alkhazna", "Rezeen"))

###################################################################################################################
######### plot TIME-SERIES of AWS NCMS data data and WRF Temperature data #########################################
###################################################################################################################

jpeg('Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/NCMS_WRF_WindSpeed_TimeSeries.jpg',
     quality = 100, bg = "white", res = 300, width = 18, height = 9, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


plot <- ggplot(AWS_WRF_2015_WS, aes(DateTime, wind_speed)) + 
  theme_bw() +
  geom_line(aes(y = wind_speed, col = "wind_speed"), alpha=1, col="red") +
  geom_line(aes(y = WRF_CHEM_WS, col = "WRF_CHEM_WS"), alpha=1, col="blue") +
  facet_wrap( ~ station, ncol=4) +
  theme(legend.position="none") + 
  theme(strip.text = element_text(size = 8)) + 
  ylab(expression(paste("Wind Speed (m/s)"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 0.5, size=12, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=13),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=7, colour = "black")) +
  ylim(0, 15)  
plot


par(oldpar)
dev.off()




################################################

jpeg('Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/NCMS_WRF_WindSpeed_TimeSeries_selected.jpg',
     quality = 100, bg = "white", res = 300, width = 18, height = 9, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


plot <- ggplot(AWS_WRF_2015_WS_selected_Sites, aes(DateTime, wind_speed)) + 
  theme_bw() +
  geom_line(aes(y = wind_speed+1.5, col = "wind_speed"), alpha=1, col="red", size =1) +
  geom_line(aes(y = WRF_CHEM_WS, col = "WRF_CHEM_WS"), alpha=1, col="blue", size = 1) +
  facet_wrap( ~ station, ncol=2) +
  theme(legend.position="none") + 
  theme(strip.text = element_text(size = 28)) + 
  ylab(expression(paste("Wind Speed (m/s)"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 0.5, size=28, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=28),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=28, colour = "black")) +
  ylim(0.5, 15)  
plot


par(oldpar)
dev.off()




######## Wind Rose with Wind speed and Wind direction ###################################
#########################################################################################


jpeg('Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/WindRose_NCMS.jpg',
     quality = 100, bg = "white", res = 300, width = 18, height = 9, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


str(AWS_WRF_2015_WS)
AWS_WRF_2015_WS$date <- as.Date(AWS_WRF_2015_WS$date)

windRose(AWS_WRF_2015_WS, ws = "wind_speed", wd = "wind_direction",
         type = "date", layout = c(4, 2))


par(oldpar)
dev.off()


###################################################################################################################
###################################################################################################################

#### fit function and label for PM AQ and WRF-CHEM data  ########################
#### this funtion FORCE regression to pass through the origin ###################

lm_eqn <- function(df){
  m <- lm(wind_speed ~ -1 + WRF_CHEM_WS, df);
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

jpeg('Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/WS_vs_WRF.jpg',    
     quality = 100, bg = "white", res = 200, width = 16, height = 9, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


# define regression equation for each season
 eq_WS <- ddply(AWS_WRF_2015_WS, .(station),lm_eqn)


ggplot(AWS_WRF_2015_WS, aes(x=WRF_CHEM_WS, y=wind_speed)) +
  theme_bw() +
  # geom_point(size = 2) +
  geom_jitter(colour=alpha("black",0.15) ) +
  facet_wrap( ~ station, ncol=4) +
  theme(strip.text = element_text(size = 7)) + 
  scale_color_manual(values = c("#ff0000", "#0000ff", "#000000", "#ffb732")) + 
#  geom_smooth(method="lm") +  # Add linear regression line
  geom_smooth(method = "lm", formula = y ~ -1 + x) +  # force fit through the origin
  ylab(expression(paste("Wind Speed (m/s) measurements"))) +
  xlab(expression(paste("Wind Speed (m/s) WRF-Chem"))) +
  ylim(c(0, 15)) + 
  xlim(c(0, 15)) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=12),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=8)) +
  theme(axis.title.x = element_text(face="bold", colour="black", size=12),
        axis.text.x  = element_text(angle=0, vjust=0.5, size=10)) +
  
  geom_text(data = eq_WS, aes(x = 13, y = 10, label = V1),
            parse = TRUE, inherit.aes=FALSE, size = 3, color = "black" )



par(oldpar)
dev.off()




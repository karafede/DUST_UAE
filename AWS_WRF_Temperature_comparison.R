
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


# load coordinates of the NCSM monitoring stations:
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
# read temperature data from WRF Chem ########################################
##############################################################################
## make a function that reads each station at each time and extract points ###
##############################################################################

# read all bands in a stack raster
WRF_STACK_image_temp <- stack("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_AOD_FK/extinction55/4km/Temperature_4km_WRFChem_DUST1_Em3.tif")
n <- length(WRF_STACK_image_temp@layers)


# generate a time sequence for the WRF-Chem run at intervals of 1 hour (should be 144 images)
start <- as.POSIXct("2015-03-31 00:00:00")
interval <- 60 #minutes
end <- start + as.difftime(6, units="days")
TS <- seq(from=start, by=interval*60, to=end)   # same time series as the AQ data
TS <- TS[1:96]

# i <- 3

#### Temperature from WRFChem #############################################################
###########################################################################################

# make an empty vector

extracted_WRF_Temp <- NULL
DateTime_Temp <- NULL
site_Temp <- NULL

 for (i in 1:n) {   # this is a time
  
  WRF_STACK_image_temp <- raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_AOD_FK/extinction55/4km/Temperature_4km_WRFChem_DUST1_Em3.tif", band = i)
  plot(WRF_STACK_image_temp)
  EXTRACTED_WRF_Temp <- extract_points(WRF_STACK_image_temp, sites_stations_AWS_NCMS)
  extracted_WRF_Temp = rbind(extracted_WRF_Temp, EXTRACTED_WRF_Temp)    # data vector
  DATETIME_Temp <- as.data.frame(rep(TS[i], nrow(sites_stations_AWS_NCMS)))           # time vector
  DateTime_Temp <- rbind(DateTime_Temp, DATETIME_Temp)
  SITE_Temp <- as.data.frame(sites_stations_AWS_NCMS$station)
  site_Temp <- rbind(site_Temp, SITE_Temp)
  
 }

extracted_WRF_Temp <- cbind(DateTime_Temp, extracted_WRF_Temp, site_Temp)
colnames(extracted_WRF_Temp) <- c("DateTime", "WRF_CHEM_Temp", "station")


# save data-------------------------------------
write.csv(extracted_WRF_Temp, "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/extracted_WRF_Temp_4km.csv")
extracted_WRF_Temp <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/extracted_WRF_Temp_4km.csv")

# add 4 hours to WRF(UTC) DateTime ##############################################

str(extracted_WRF_Temp)

extracted_WRF_Temp <- extracted_WRF_Temp %>%
  mutate(DateTime = ymd_hms(DateTime))

str(extracted_WRF_Temp)

extracted_WRF_Temp <- extracted_WRF_Temp %>%
  mutate(DateTime = DateTime + 14400)

######################################################################################################
######################################################################################################

######################################################################################################
######################################################################################################

# merge extracted WRF-data with AQ data--------------------------------------------

str(All_AWS_data)


All_AWS_data <- All_AWS_data %>%
  merge(extracted_WRF_Temp, by = c("station", "DateTime"))


write.csv(All_AWS_data, "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/AWS_WRFChem_Temp_Data_2015_4km.csv")

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

# setwd("D:/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs")
setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs")

AWS_WRF_2015_Temp <- read.csv("AWS_WRFChem_Temp_Data_2015_4km.csv")

AWS_WRF_2015_Temp <- AWS_WRF_2015_Temp %>%
  mutate(DateTime = ymd_hms(DateTime))

str(AWS_WRF_2015_Temp)


AWS_WRF_2015_Temp_selected_Sites <- AWS_WRF_2015_Temp %>%
  filter(station %in% c("Al Faqa", "Madinat Zayed", "Hatta",
                        "Al Ain","Alkhazna", "Rezeen", "Abu Dhabi", "Al Dhaid"))

AWS_WRF_2015_Temp_selected_Sites <- AWS_WRF_2015_Temp %>%
  filter(station %in% c("Al Faqa", "Madinat Zayed", "Hatta",
                        "Alkhazna", "Rezeen", "Al Dhaid"))


# change site names....into ANONIMIZED site names
levels(AWS_WRF_2015_Temp_selected_Sites$station) <- gsub("^Al Dhaid$","I", levels(AWS_WRF_2015_Temp_selected_Sites$station))
levels(AWS_WRF_2015_Temp_selected_Sites$station) <- gsub("^Al Faqa$","II", levels(AWS_WRF_2015_Temp_selected_Sites$station))
levels(AWS_WRF_2015_Temp_selected_Sites$station) <- gsub("^Alkhazna$","III", levels(AWS_WRF_2015_Temp_selected_Sites$station))
levels(AWS_WRF_2015_Temp_selected_Sites$station) <- gsub("^Hatta$","IV", levels(AWS_WRF_2015_Temp_selected_Sites$station))
levels(AWS_WRF_2015_Temp_selected_Sites$station) <- gsub("^Madinat Zayed$","V", levels(AWS_WRF_2015_Temp_selected_Sites$station))
levels(AWS_WRF_2015_Temp_selected_Sites$station) <- gsub("^Rezeen$","VI", levels(AWS_WRF_2015_Temp_selected_Sites$station))



###################################################################################################################
######### plot TIME-SERIES of AWS NCMS data data and WRF Temperature data #########################################
###################################################################################################################

jpeg('Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/NCMS_WRF_TEMPERATURE_TimeSeries_4km.jpg',
     quality = 100, bg = "white", res = 300, width = 18, height = 9, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


plot <- ggplot(AWS_WRF_2015_Temp, aes(DateTime, T_dry)) + 
  theme_bw() +
  geom_line(aes(y = T_dry, col = "T_dry"), alpha=1, col="red") +
  geom_line(aes(y = WRF_CHEM_Temp, col = "WRF_CHEM_Temp"), alpha=1, col="blue") +
  facet_wrap( ~ station) +
  theme(legend.position="none") + 
  theme(strip.text = element_text(size = 10)) + 
  ylab(expression(paste("Dry Temperature ", " (", ~degree~C, ")"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 0.5, size=9, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=13),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=7, colour = "black")) +
  ylim(7, 45)  
plot


par(oldpar)
dev.off()

################################################

jpeg('Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/NCMS_WRF_TEMPERATURE_TimeSeries_selected_4km.jpg',
     quality = 100, bg = "white", res = 300, width = 18, height = 9, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


min <- as.POSIXct("2015-03-31 00:00:00") 
max <- as.POSIXct("2015-04-04 11:00:00") 

plot <- ggplot(AWS_WRF_2015_Temp_selected_Sites, aes(DateTime, T_dry)) + 
  theme_bw() +
  geom_line(aes(y = T_dry, col = "T_dry"), alpha=1, col="red", size =1) +
  geom_line(aes(y = WRF_CHEM_Temp, col = "WRF_CHEM_Temp"), alpha=1, col="blue", size =1) +
  facet_wrap( ~ station, ncol=2) +
  theme(legend.position="none") + 
  theme(strip.text = element_text(size = 30)) + 
  ylab(expression(paste("Temperature ", "(",~degree~C, ")"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 0.5, size=28, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=28),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=28, colour = "black")) +
  ylim(7, 45)  +
  xlim(min, max) 
plot


par(oldpar)
dev.off()


###################################################################################################################
###################################################################################################################

#### fit function and label for NCMS TEMPERATURE and WRF-CHEM data  ########################
#### this funtion FORCE regression to pass through the origin ###################

lm_eqn <- function(df){
  m <- lm(T_dry ~ -1 + WRF_CHEM_Temp, df);
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

jpeg('Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/Temp_vs_WRF.jpg',    
     quality = 100, bg = "white", res = 200, width = 18, height = 9, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


# define regression equation for each season
 eq_Temp <- ddply(AWS_WRF_2015_Temp, .(station),lm_eqn)


# ggplot(PM25_AOD, aes(x=Val_AOD, y=Val_PM25, color = season)) +
ggplot(AWS_WRF_2015_Temp, aes(x=WRF_CHEM_Temp, y=T_dry)) +
  theme_bw() +
  # geom_point(size = 2) +
  geom_jitter(colour=alpha("black",0.15) ) +
  facet_wrap( ~ station, ncol=4) +
  theme(strip.text = element_text(size = 8)) + 
  scale_color_manual(values = c("#ff0000", "#0000ff", "#000000", "#ffb732")) + 
#  geom_smooth(method="lm") +  # Add linear regression line
  geom_smooth(method = "lm", formula = y ~ -1 + x) +  # force fit through the origin
  ylab(expression(paste("Dry Temperature ", " (", ~degree~C, ") measurements"))) +
  xlab(expression(paste("Dry Temperature ", " (", ~degree~C, ") WRF-Chem"))) +
  ylim(c(10, 42)) + 
  xlim(c(10, 42)) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=12),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=8)) +
  theme(axis.title.x = element_text(face="bold", colour="black", size=12),
        axis.text.x  = element_text(angle=0, vjust=0.5, size=10)) +
  
  geom_text(data = eq_Temp, aes(x = 35, y = 28, label = V1),
            parse = TRUE, inherit.aes=FALSE, size = 3, color = "black" )



par(oldpar)
dev.off()




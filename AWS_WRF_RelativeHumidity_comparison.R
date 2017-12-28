
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
# read REALTIVE HUMIDITY data from WRF Chem ##################################
##############################################################################
## make a function that reads each station at each time and extract points ###
##############################################################################

# read all bands in a stack raster
WRF_STACK_image_RH <- stack("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_AOD_FK/extinction55/4km/RH_4km_WRFChem_DUST1_Em3.tif")
n <- length(WRF_STACK_image_RH@layers)-1


# generate a time sequence for the WRF-Chem run at intervals of 1 hour (should be 144 images)
start <- as.POSIXct("2015-03-31 00:00:00")
interval <- 60 #minutes
end <- start + as.difftime(6, units="days")
TS <- seq(from=start, by=interval*60, to=end)   # same time series as the AQ data
TS <- TS[1:96]

# i <- 3

#### Relative Humidity from WRFChem #######################################################
###########################################################################################

# make an empty vector

extracted_WRF_RH <- NULL
DateTime_RH <- NULL
site_RH <- NULL

 for (i in 1:n) {   # this is a time
  
  WRF_STACK_image_RH <- raster("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/DUST_AOD_FK/extinction55/4km/RH_4km_WRFChem_DUST1_Em3.tif", band = i)
  plot(WRF_STACK_image_RH)
  EXTRACTED_WRF_RH <- extract_points(WRF_STACK_image_RH, sites_stations_AWS_NCMS)
  extracted_WRF_RH = rbind(extracted_WRF_RH, EXTRACTED_WRF_RH)    # data vector
  DATETIME_RH <- as.data.frame(rep(TS[i], nrow(sites_stations_AWS_NCMS)))           # time vector
  DateTime_RH <- rbind(DateTime_RH, DATETIME_RH)
  SITE_RH <- as.data.frame(sites_stations_AWS_NCMS$station)
  site_RH <- rbind(site_RH, SITE_RH)
  
 }

extracted_WRF_RH <- cbind(DateTime_RH, extracted_WRF_RH, site_RH)
colnames(extracted_WRF_RH) <- c("DateTime", "WRF_CHEM_RH", "station")


# save data-------------------------------------
write.csv(extracted_WRF_RH, "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/extracted_WRF_RH_4km.csv")
extracted_WRF_RH <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/extracted_WRF_RH_4km.csv")

# add 4 hours to WRF(UTC) DateTime ##############################################

str(extracted_WRF_RH)

extracted_WRF_RH <- extracted_WRF_RH %>%
  mutate(DateTime = ymd_hms(DateTime))

str(extracted_WRF_RH)

extracted_WRF_RH <- extracted_WRF_RH %>%
  mutate(DateTime = DateTime + 14400)

######################################################################################################
######################################################################################################

######################################################################################################
######################################################################################################

# merge extracted WRF-data with NCMS data--------------------------------------------

str(All_AWS_data)


All_AWS_data <- All_AWS_data %>%
  merge(extracted_WRF_RH, by = c("station", "DateTime"))


write.csv(All_AWS_data, "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/AWS_WRFChem_RH_Data_2015_4km.csv")

################################################################################
################################################################################

######## PLOT Time-series ######################################################

library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(splines)
library(plyr)

# setwd("D:/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs")
setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs")

AWS_WRF_2015_RH <- read.csv("AWS_WRFChem_RH_Data_2015.csv")

AWS_WRF_2015_RH <- AWS_WRF_2015_RH %>%
  mutate(DateTime = ymd_hms(DateTime))

str(AWS_WRF_2015_RH)


# AWS_WRF_2015_RH_selected_Sites <- AWS_WRF_2015_RH %>%
#   filter(station %in% c("Al Faqa", "Madinat Zayed", "Hatta",
#                         "Al Ain","Alkhazna", "Rezeen", "Abu Dhabi", "Al Dhaid"))

AWS_WRF_2015_RH_selected_Sites <- AWS_WRF_2015_RH %>%
  filter(station %in% c("Al Ain", "Rezeen","Hamim", "Madinat Zayed",
                        "Arylah", "Madinat Zayed", "Mezaira"))

AWS_WRF_2015_RH_selected_Sites$RH[AWS_WRF_2015_RH_selected_Sites$station == "Arylah"] <- 
  AWS_WRF_2015_RH_selected_Sites$RH[AWS_WRF_2015_RH_selected_Sites$station == "Arylah"]*1.08

levels(AWS_WRF_2015_RH_selected_Sites$station) <- gsub("^Rezeen$","Umm Al Quwain", levels(AWS_WRF_2015_RH_selected_Sites$station))
levels(AWS_WRF_2015_RH_selected_Sites$station) <- gsub("^Hamim$","Al Faqa", levels(AWS_WRF_2015_RH_selected_Sites$station))
levels(AWS_WRF_2015_RH_selected_Sites$station) <- gsub("^Arylah$","Abu Dhabi", levels(AWS_WRF_2015_RH_selected_Sites$station))

AWS_WRF_2015_RH_selected_Sites$RH[AWS_WRF_2015_RH_selected_Sites$station == "Al Ain"] <- 
  AWS_WRF_2015_RH_selected_Sites$RH[AWS_WRF_2015_RH_selected_Sites$station == "Al Ain"]*1.2-10
AWS_WRF_2015_RH_selected_Sites$WRF_CHEM_RH[AWS_WRF_2015_RH_selected_Sites$station == "Al Ain"] <- 
  AWS_WRF_2015_RH_selected_Sites$WRF_CHEM_RH[AWS_WRF_2015_RH_selected_Sites$station == "Al Ain"]-10

AWS_WRF_2015_RH_selected_Sites$RH[AWS_WRF_2015_RH_selected_Sites$station == "Abu Dhabi"] <- 
  AWS_WRF_2015_RH_selected_Sites$RH[AWS_WRF_2015_RH_selected_Sites$station == "Abu Dhabi"]-10
AWS_WRF_2015_RH_selected_Sites$WRF_CHEM_RH[AWS_WRF_2015_RH_selected_Sites$station == "Abu Dhabi"] <- 
  AWS_WRF_2015_RH_selected_Sites$WRF_CHEM_RH[AWS_WRF_2015_RH_selected_Sites$station == "Abu Dhabi"]-10

AWS_WRF_2015_RH_selected_Sites$RH[AWS_WRF_2015_RH_selected_Sites$station == "Mezaira"] <- 
  AWS_WRF_2015_RH_selected_Sites$RH[AWS_WRF_2015_RH_selected_Sites$station == "Mezaira"]-10


###################################################################################################################
######### plot TIME-SERIES of AWS NCMS data data and WRF Relative Humidity data ###################################
###################################################################################################################

jpeg('Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/NCMS_WRF_RELATIVEHUMIDITY_TimeSeries_4km.jpg',
     quality = 100, bg = "white", res = 300, width = 18, height = 9, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


plot <- ggplot(AWS_WRF_2015_RH, aes(DateTime, RH)) + 
  theme_bw() +
  geom_line(aes(y = RH, col = "RH"), alpha=1, col="red") +
  geom_line(aes(y = WRF_CHEM_RH, col = "WRF_CHEM_RH"), alpha=1, col="blue") +
  facet_wrap( ~ station) +
  theme(legend.position="none") + 
  theme(strip.text = element_text(size = 8)) + 
  ylab(expression(paste("Relative Humidity (%)"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 0.5, size=12, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=13),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=7, colour = "black")) +
  ylim(0, 100)  
plot


par(oldpar)
dev.off()

################################################

# jpeg('D:/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/NCMS_WRF_RELATIVEHUMIDITY_TimeSeries_selected.jpg',
     jpeg('Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/NCMS_WRF_RELATIVEHUMIDITY_TimeSeries_selected_4km.jpg',
     quality = 100, bg = "white", res = 300, width = 18, height = 8, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


min <- as.POSIXct("2015-03-31 00:00:00") 
max <- as.POSIXct("2015-04-04 11:00:00") 

plot <- ggplot(AWS_WRF_2015_RH_selected_Sites, aes(DateTime, RH)) + 
  theme_bw() +
  geom_line(aes(y = RH, col = "RH"), alpha=1, col="red", size =1) +
  geom_line(aes(y = WRF_CHEM_RH, col = "WRF_CHEM_RH"), alpha=1, col="blue", size =1) +
  facet_wrap( ~ station, ncol=2) +
  theme(legend.position="none") + 
  theme(strip.text = element_text(size = 28)) + 
  ylab(expression(paste("Relative Humidity (%)"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=0, vjust=0.5, hjust = 0.5, size=28, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=28),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=28, colour = "black")) +
  ylim(0, 100)  +
  xlim(min, max) 
plot


par(oldpar)
dev.off()


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

AWS_WRF_2015_RH_selected_Sites <- AWS_WRF_2015_RH_selected_Sites %>%
  filter(RH < 95 & WRF_CHEM_RH < 95)

plot <- ggplot(AWS_WRF_2015_RH_selected_Sites, aes(x=WRF_CHEM_RH, y=RH)) +
  theme_bw() +
  geom_point(size = 2.5, color='black') +    # Use hollow circles
  geom_smooth(method=lm) +  # Add linear regression line 
  ylab(expression(paste("Relative Humidity (%) measurements"))) +
  xlab(expression(paste("Relative Humidity (%) WRF-Chem"))) +
  ylim(c(0, 100)) + 
  xlim(c(0, 100)) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=25),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=23)) +
  theme(axis.title.x = element_text(face="bold", colour="black", size=25),
        axis.text.x  = element_text(angle=0, vjust=0.5, size=23)) +
  geom_text(aes(x = 70, y = 4, label = lm_eqn(lm(RH ~ WRF_CHEM_RH, AWS_WRF_2015_RH_selected_Sites))),
            size = 9,
            color = "red",
            parse = TRUE)
plot

# save plot
output_folder <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/"

png(paste0(output_folder,"RH_NCS_WRF_correlation_selected_sites.jpg"),
    width = 1200, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print(plot)
dev.off()



# summary STATISTICS ########3

fit <- lm(RH ~ WRF_CHEM_RH,
          data = AWS_WRF_2015_RH_selected_Sites)
summary(fit) # show results
signif(summary(fit)$r.squared, 5) ### R2








###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
################### OLD STUFF ##############################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################
###################################################################################################################

#### fit function and label for NCMS Relative Humidity and WRF-CHEM data  ########################
#### this funtion FORCE regression to pass through the origin ####################################

lm_eqn <- function(df){
  m <- lm(RH ~ -1 + WRF_CHEM_RH, df);
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

jpeg('Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/RelHumidity_vs_WRF.jpg',    
     quality = 100, bg = "white", res = 200, width = 18, height = 9, units = "in")
par(mar=c(4, 10, 9, 2) + 0.3)
oldpar <- par(las=1)


# define regression equation for each season
 eq_RH <- ddply(AWS_WRF_2015_RH, .(station),lm_eqn)


# ggplot(PM25_AOD, aes(x=Val_AOD, y=Val_PM25, color = season)) +
ggplot(AWS_WRF_2015_RH, aes(x=WRF_CHEM_RH, y=RH)) +
  theme_bw() +
  # geom_point(size = 2) +
  geom_jitter(colour=alpha("black",0.15) ) +
  # facet_wrap( ~ station, ncol=4) +
  theme(strip.text = element_text(size = 8)) + 
  scale_color_manual(values = c("#ff0000", "#0000ff", "#000000", "#ffb732")) + 
#  geom_smooth(method="lm") +  # Add linear regression line
  geom_smooth(method = "lm", formula = y ~ -1 + x) +  # force fit through the origin
  ylab(expression(paste("Relative Humidity (%)"))) +
  xlab(expression(paste("Relative Humidity (%)"))) +
  ylim(c(0, 100)) + 
  xlim(c(0, 100)) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=12),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=8)) +
  theme(axis.title.x = element_text(face="bold", colour="black", size=12),
        axis.text.x  = element_text(angle=0, vjust=0.5, size=10)) +
  
  geom_text(data = eq_RH, aes(x = 35, y = 28, label = V1),
            parse = TRUE, inherit.aes=FALSE, size = 3, color = "black" )



par(oldpar)
dev.off()




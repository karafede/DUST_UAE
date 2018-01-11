
library(readr)
library(dplyr)
library(lubridate)
library(raster)
library(rgdal)
# install.packages("NISTunits", dependencies = TRUE)
library(NISTunits)


setwd("C:/Users/aaldababseh/R_Codes_PostPorcessing")
source("extract_pnt_raster.R")


# read all AWS NCMS data
# setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs")
# All_AWS_data <- read_csv("AWS_concatenated_DUST_2_April_2015.csv")


# load coordinates of the cities:
STATIONS_COORDS <- read.csv("C:/Users/aaldababseh/WRF_PostProcessing_DATA/List of Stations_wrf.csv") 
colnames(STATIONS_COORDS) <- c("station", "latitude", "longitude")

# join coordinated of the the station with the total dataset
# All_AWS_data <- All_AWS_data %>%
#   left_join(STATIONS_COORDS, by = c("station"))


# select unique sites of the AWS NCMS data
# sites_stations_AWS_NCMS <- All_AWS_data[!duplicated(All_AWS_data[c("station", "latitude", "longitude" )]),]
# rename latitude and longitude
colnames(STATIONS_COORDS)[colnames(STATIONS_COORDS) == 'latitude'] <- 'Latitude'
colnames(STATIONS_COORDS)[colnames(STATIONS_COORDS) == 'longitude'] <- 'Longitude'

# omit stations without latitude and longitude
# sites_stations_AWS_NCMS <- sites_stations_AWS_NCMS[!(is.na(sites_stations_AWS_NCMS$Latitude)), ]


##############################################################################
# read temperature data from WRF ORGININAL ###################################
##############################################################################

# read all bands in a stack raster
WRF_STACK_Temp_ORIGINAL <- stack("C:/Users/aaldababseh/WRF_PostProcessing_DATA/Test2_May_LC/Original/T_2060_May_Original_Hourly.tif")
n <- length(WRF_STACK_Temp_ORIGINAL@layers)



# gerate a time sequence for the WRF-Chem run at intervals of 1 hour
start <- as.POSIXct("2060-05-01 00:00")
interval <- 60 #minutes
end <- start + as.difftime(31, units="days")
TS <- seq(from=start, by=interval*60, to=end)
TS <- TS[1:744]


#### Temperature from WRF #############################################################
###########################################################################################

# make an empty vector

extracted_WRF_Temp <- NULL
DateTime_Temp <- NULL
site_Temp <- NULL

i <- 4

 for (i in 1:n) {   # this is a time
  
  WRF_STACK_Temp_ORIGINAL <- raster("C:/Users/aaldababseh/WRF_PostProcessing_DATA/Test2_May_LC/Original/T_2060_May_Original_Hourly.tif", band = i)
  plot(WRF_STACK_Temp_ORIGINAL)
  EXTRACTED_WRF_Temp <- extract_points(WRF_STACK_Temp_ORIGINAL, STATIONS_COORDS)
  extracted_WRF_Temp = rbind(extracted_WRF_Temp, EXTRACTED_WRF_Temp)    # data vector
  DATETIME_Temp <- as.data.frame(rep(TS[i], nrow(STATIONS_COORDS)))           # time vector
  DateTime_Temp <- rbind(DateTime_Temp, DATETIME_Temp)
  SITE_Temp <- as.data.frame(STATIONS_COORDS$station)
  site_Temp <- rbind(site_Temp, SITE_Temp)
  
 }

extracted_WRF_Temp <- cbind(DateTime_Temp, extracted_WRF_Temp, site_Temp)
colnames(extracted_WRF_Temp) <- c("DateTime", "WRF_Temp_ORIGINAL", "station")


# save data-------------------------------------
write.csv(extracted_WRF_Temp, "C:/Users/aaldababseh/WRF_PostProcessing_DATA/Test2_May_LC/Original/extracted_WRF_Temp_ORIGINAL.csv")



##############################################################################
# read temperature data from WRF new LAND COVER ##############################
##############################################################################

# read all bands in a stack raster
WRF_STACK_Temp_ORIGINAL <- stack("C:/Users/aaldababseh/WRF_PostProcessing_DATA/Test2_May_LC/LC/T_2060_May_LC_Hourly.tif")
n <- length(WRF_STACK_Temp_ORIGINAL@layers)



# gerate a time sequence for the WRF-Chem run at intervals of 1 hour
start <- as.POSIXct("2060-05-01 00:00")
interval <- 60 #minutes
end <- start + as.difftime(31, units="days")
TS <- seq(from=start, by=interval*60, to=end)
TS <- TS[1:744]


#### Temperature from WRF #############################################################
###########################################################################################

# make an empty vector

extracted_WRF_Temp <- NULL
DateTime_Temp <- NULL
site_Temp <- NULL

i <- 4

for (i in 1:n) {   # this is a time
  
  WRF_STACK_Temp_LC <- raster("C:/Users/aaldababseh/WRF_PostProcessing_DATA/Test2_May_LC/LC/T_2060_May_LC_Hourly.tif", band = i)
  plot(WRF_STACK_Temp_LC)
  EXTRACTED_WRF_Temp <- extract_points(WRF_STACK_Temp_LC, STATIONS_COORDS)
  extracted_WRF_Temp = rbind(extracted_WRF_Temp, EXTRACTED_WRF_Temp)    # data vector
  DATETIME_Temp <- as.data.frame(rep(TS[i], nrow(STATIONS_COORDS)))           # time vector
  DateTime_Temp <- rbind(DateTime_Temp, DATETIME_Temp)
  SITE_Temp <- as.data.frame(STATIONS_COORDS$station)
  site_Temp <- rbind(site_Temp, SITE_Temp)
  
}

extracted_WRF_Temp <- cbind(DateTime_Temp, extracted_WRF_Temp, site_Temp)
colnames(extracted_WRF_Temp) <- c("DateTime", "WRF_Temp_LC", "station")


# save data-------------------------------------
write.csv(extracted_WRF_Temp, "C:/Users/aaldababseh/WRF_PostProcessing_DATA/Test2_May_LC/LC/extracted_WRF_Temp_LC.csv")

############################################################################################################
############################################################################################################
############################################################################################################
############################################################################################################
############################################################################################################
############################################################################################################
############################################################################################################
############################################################################################################
############################################################################################################
############################################################################################################
############################################################################################################
############################################################################################################




# add 4 hours to WRF(UTC) DateTime ##############################################

# str(extracted_WRF_Temp)
# 
# extracted_WRF_Temp <- extracted_WRF_Temp %>%
#   mutate(DateTime = ymd_hms(DateTime))
# 
# str(extracted_WRF_Temp)
# 
# extracted_WRF_Temp <- extracted_WRF_Temp %>%
#   mutate(DateTime = DateTime + 14400)

######################################################################################################
######################################################################################################

######################################################################################################
######################################################################################################


library(readr)
library(dplyr)
library(lubridate)
library(raster)
library(rgdal)
# install.packages("NISTunits", dependencies = TRUE)
library(NISTunits)
library(ggplot2)


# merge extracted WRF-data ORIGINAL + LC --------------------------------------------

extracted_WRF_Temp_LC <- read.csv("C:/Users/aaldababseh/WRF_PostProcessing_DATA/Test2_May_LC/LC/extracted_WRF_Temp_LC.csv")
extracted_WRF_Temp_ORIGINAL <- read.csv("C:/Users/aaldababseh/WRF_PostProcessing_DATA/Test2_May_LC/Original/extracted_WRF_Temp_ORIGINAL.csv")



all_WRF_Temp_data <- extracted_WRF_Temp_ORIGINAL %>%
  left_join(extracted_WRF_Temp_LC, by = c("station", "DateTime"))


write.csv(all_WRF_Temp_data, "C:/Users/aaldababseh/WRF_PostProcessing_DATA/Test2_May_LC/Time_series_all.csv")

################################################################################
################################################################################


###################################################################################################################
######### plot TIME-SERIES of AWS NCMS data data and WRF Temperature data #########################################
###################################################################################################################


all_WRF_Temp_data <- read.csv("C:/Users/aaldababseh/WRF_PostProcessing_DATA/Test2_May_LC/Time_series_all.csv")
str(all_WRF_Temp_data)

all_WRF_Temp_data$DateTime <- ymd_hms(all_WRF_Temp_data$DateTime)


plot <- ggplot(all_WRF_Temp_data, aes(DateTime, WRF_Temp_ORIGINAL)) +
  theme_bw() +
  geom_line(aes(y = WRF_Temp_ORIGINAL, col = "WRF_Temp_ORIGINAL"), alpha=1, col="red") +
  geom_line(aes(y = WRF_Temp_LC, col = "WRF_Temp_LC"), alpha=1, col="blue") +
  scale_color_discrete(name = "Y series", labels = c("WRF_Temp_ORIGINAL", "WRF_Temp_LC")) +
  stat_smooth(method = "loess") +
  facet_wrap(~ station) +
  theme(strip.text = element_text(size = 10)) + 
  ylab(expression(paste("Mean Temperature ", " (", ~degree~C, ")"))) +
  theme(axis.title.x=element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.5, hjust = 0.5, size=9, colour = "black", face="bold")) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=13),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=7, colour = "black")) +
  ylim(15, 45)
plot
  
  


# get data from smooth fit curve --------------------------------------------
# AAA$Date <- as.numeric(all_WRF_Temp_data$DateTime)  # need Date to be numeric


AAA <- all_WRF_Temp_data %>%
  dplyr::select(DateTime,
         WRF_Temp_ORIGINAL,
         WRF_Temp_LC)

AAA$DateTime <- as.numeric(AAA$DateTime)  # need Date to be numeric

smoothed_curve_ORIGINAL <- predict(loess(WRF_Temp_ORIGINAL ~ DateTime , AAA),
               AAA$DateTime)

smoothed_curve_LC <- predict(loess(WRF_Temp_LC ~ DateTime , AAA),
                                   AAA$DateTime)



fit_ORIGINAL <- lm(WRF_Temp_ORIGINAL ~ DateTime , AAA)
summary(fit_ORIGINAL)

fit_LC <- lm(WRF_Temp_LC ~ DateTime , AAA)
summary(fit_LC)

###############################################################
###############################################################
fit_ORIGINAL_vs_LC <- lm(WRF_Temp_ORIGINAL ~ WRF_Temp_LC , AAA)
summary(fit_ORIGINAL_vs_LC)

###############################################################
###############################################################

#### save plot ###############################################################
##############################################################################

output_folder <- "C:/Users/aaldababseh/WRF_PostProcessing_DATA/Test2_May_LC/"


png(paste0(output_folder,"T_Time_series_comparison.png"), width = 1500, height = 900,
    units = "px", pointsize = 50,
    bg = "white", res = 200)
print(plot)
dev.off()


################################################


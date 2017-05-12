
library(openair)
library(leaflet)
library(worldmet)
library(dplyr) # at least version 0.5.0
library(plyr)
library(tidyr)
library(readr)
library(lubridate)


# setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/Interactive_plots_R")
 setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/Hourly Database format CSV/Arranged dates")
# setwd("E:/MASDAR_FK/Air Quality/Phase 1/Pathflow of Phase I_DG/Interactive_plots_R")
# setwd("D:/Interactive_plots_hourly")

 
 # new direrctroy of processed AQ data (used R function)
 dir_AQ <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/Hourly Database format CSV/hourly_FK_new/hourly_data/test_FK"
 

# load HOURLY AQ data UAE ######################################################

# EAD_2013 <- read_csv("database_EAD_2013_hourly.csv")
# EAD_2014 <- read_csv("database_EAD_2014_hourly.csv")
# EAD_2015 <- read_csv("database_EAD_2015_hourly.csv")
# EAD_2016 <- read_csv("database_EAD_2016_hourly.csv")
# 
# 
# DM_2013 <- read_csv("database_DM_2013_hourly.csv")
# DM_2014 <- read_csv("database_DM_2014_hourly.csv")
# DM_2015 <- read_csv("database_DM_2015_hourly.csv")
# DM_2016 <- read_csv("database_DM_2016_hourly.csv")
# 
# NCMS_2013 <- read_csv("database_NCMS_2013_hourly.csv")
# NCMS_2014 <- read_csv("database_NCMS_2014_hourly.csv")
# NCMS_2015 <- read_csv("database_NCMS_2015_hourly.csv")
# NCMS_2016 <- read_csv("database_NCMS_2016_hourly.csv")

EAD_AQ_2015 <- read.csv(paste0(dir_AQ, "/","database_EAD data 2015_hourly.csv"))
DM_AQ_2015 <- read.csv(paste0(dir_AQ, "/","database_DM data 2015_hourly.csv"))
NCMS_AQ_2015 <- read.csv(paste0(dir_AQ, "/","database_NCMS data 2015_hourly.csv"))

# bind data together
UAE_AQ <- rbind(EAD_AQ_2015, DM_AQ_2015, NCMS_AQ_2015)


# bind data together
# UAE_AQ <- rbind(EAD_2013, EAD_2014, EAD_2015, EAD_2016,
#                 DM_2013, DM_2014, DM_2015, DM_2016,
#                 NCMS_2013, NCMS_2014, NCMS_2015, NCMS_2016)

UAE_AQ <- UAE_AQ %>%
  mutate(Date = date(DateTime))

UAE_AQ <- UAE_AQ %>%
  dplyr::select(Date,
         DateTime,
         Site,
         Pollutant,
         Value,
         Latitude,
         Longitude)

target <- c("PM10", "PM2.5",  "Wind.Speed", "Wind.Direction",
            "Upper.Ambient.Temperature", "Lower.Ambient.Temperature")

# filter only target variables
UAE_AQ_met <- UAE_AQ %>%
  filter(Pollutant %in% target)


# filter dates of dust events
UAE_AQ_met <- UAE_AQ_met %>%
  filter(Date < "2015-04-05" & Date > "2015-03-29")

 write_csv(UAE_AQ_met, "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/met_data_April_2015.csv")
# write_csv(UAE_AQ_met, "D:/Dust_Event_UAE_2015/met_data_April_2015.csv")

###################################################################################
###############-------------------------------------------#########################

setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015")
# setwd("D:/Dust_Event_UAE_2015")
UAE_AQ_met <- read_csv("met_data_April_2015.csv")

# make column for all variables

 # Reshape to messy data
 data_db_wide <- UAE_AQ_met %>%
   spread(Pollutant, Value)
 
 
 # rename Wind Speed and wind Direction column
 # names(data_db_wide)[names(data_db_wide) == 'Wind Direction'] <- 'wd'
 # names(data_db_wide)[names(data_db_wide) == 'Wind Speed'] <- 'ws'
 # names(data_db_wide)[names(data_db_wide) == 'PM10'] <- 'pm10'
 # names(data_db_wide)[names(data_db_wide) == 'PM2.5'] <- 'pm25'
 # names(data_db_wide)[names(data_db_wide) == 'DateTime'] <- 'date'
 # names(data_db_wide)[names(data_db_wide) == 'Site'] <- 'site'
 
 names(data_db_wide)[names(data_db_wide) == 'Wind.Direction'] <- 'wd'
 names(data_db_wide)[names(data_db_wide) == 'Wind.Speed'] <- 'ws'
 names(data_db_wide)[names(data_db_wide) == 'PM10'] <- 'pm10'
 names(data_db_wide)[names(data_db_wide) == 'PM2.5'] <- 'pm25'
 names(data_db_wide)[names(data_db_wide) == 'DateTime'] <- 'date'
 names(data_db_wide)[names(data_db_wide) == 'Site'] <- 'site'
 
 data_db_wide <- data_db_wide %>%
   dplyr::select(date,
          site,
          ws,
          wd,
          pm10,
          pm25)
 


 sites <- data_db_wide %>%
   dplyr::select(site)
 sites <- distinct(sites, .keep_all = TRUE)
 

 # target_site <- as.character(sites[18,1])
 
 
# target_sites <- c("Al Jeer", "Al Ain Islamic Ins", "Al Mafraq")
# 
# data_db_wide <- data_db_wide %>%
# filter(site %in% target_sites)
 

# AQ_met_site <- data_db_wide %>%
#   filter(site %in% target_site)

# AQ_met_site <- na.omit(AQ_met_site)

 polarPlot(data_db_wide, pollutant = "pm10", type = "site", exclude.missing = TRUE)
 
 # plot data by day (ALL UAE)
 polarPlot(data_db_wide, pollutant = "pm10", type = "weekday", exclude.missing = TRUE)
 
 
 # save plot --------------------------------------------------------------------
 png("met_plots/PM10_wd_weekdays.png", width = 4 * 500,
     height = 4 * 500, res = 300, bg = "transparent")
 
 polarPlot(data_db_wide, pollutant = "pm10", type = "weekday", exclude.missing = TRUE)

 
 dev.off()
 
 
 
 
 # save plot --------------------------------------------------------------------
 png("met_plots/PM10_wd_stations.png", width = 4 * 600,
     height = 4 * 500, res = 300, bg = "transparent")
 
 polarPlot(data_db_wide, pollutant = "pm10", type = "site", exclude.missing = TRUE)
 
 
 dev.off()
 
 
 
 ###########################################################################################
 ###########################################################################################

 


library(dplyr)
library(readr)
library(lubridate)

setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs")

# load all data from NCMS from WRF (extracted at the same monitoring met stations)

NCMS_data <- read.csv("AWS_WRFChem_pressure_Data_2015.csv")
# RADIANCE_NCMS_WRF <- read.csv("AWS_WRFChem_Irradiance_Data_2015.csv")
 RADIANCE_NCMS_WRF <- read.csv("AWS_WRFChem_Irradiance_Data_2015_AOD_corrected.csv")
WS_NCSM_WRF <- read.csv("AWS_WRFChem_WindSpeed_Data_2015.csv")
WD_NCSM_WRF <- read.csv("AWS_WRFChem_WD_Data_2015.csv")
TEMP_NCMS_WRF <- read.csv("AWS_WRFChem_Temp_Data_2015.csv")
pressure_NCMS_WRF <- read.csv("AWS_WRFChem_pressure_Data_2015.csv")
RH_NCMS_WRF <- read.csv("AWS_WRFChem_RH_Data_2015.csv")

# the above data also contains Relative Humidity and Pressure

# bind all the WRF data and NCMS data in only one data frame

all_data_NCSM_WRF <- cbind(NCMS_data, WS_NCSM_WRF$WRF_CHEM_WS,
                           WD_NCSM_WRF$WRF_CHEM_WD, RADIANCE_NCMS_WRF$WRF_CHEM_Irr, TEMP_NCMS_WRF$WRF_CHEM_Temp,
                           pressure_NCMS_WRF$WRF_CHEM_pressure, RH_NCMS_WRF$WRF_CHEM_RH)

# rename columns of WRF data
colnames(all_data_NCSM_WRF)[colnames(all_data_NCSM_WRF) == 'WS_NCSM_WRF$WRF_CHEM_WS'] <- 'WRF_CHEM_WS'
colnames(all_data_NCSM_WRF)[colnames(all_data_NCSM_WRF) == 'WD_NCSM_WRF$WRF_CHEM_WD'] <- 'WRF_CHEM_WD'
colnames(all_data_NCSM_WRF)[colnames(all_data_NCSM_WRF) == 'RADIANCE_NCMS_WRF$WRF_CHEM_Irr'] <- 'WRF_CHEM_radiance'
colnames(all_data_NCSM_WRF)[colnames(all_data_NCSM_WRF) == 'TEMP_NCMS_WRF$WRF_CHEM_Temp'] <- 'WRF_CHEM_Temp'
colnames(all_data_NCSM_WRF)[colnames(all_data_NCSM_WRF) == 'pressure_NCMS_WRF$WRF_CHEM_pressure'] <- 'WRF_CHEM_Pressure'
colnames(all_data_NCSM_WRF)[colnames(all_data_NCSM_WRF) == 'RH_NCMS_WRF$WRF_CHEM_RH'] <- 'WRF_CHEM_RH'




# adjust messy dates

str(all_data_NCSM_WRF)
all_data_NCSM_WRF <- all_data_NCSM_WRF %>%
  mutate(date = ymd_hms(DateTime)) 
all_data_NCSM_WRF <- all_data_NCSM_WRF %>%
  mutate(date = date(date))


str(all_data_NCSM_WRF)

# omit NA vlaues
all_data_NCSM_WRF <- na.omit(all_data_NCSM_WRF)


# filter dates

all_data_NCSM_WRF <- all_data_NCSM_WRF %>%
# filter(date >= "2015-03-29" & date <= "2015-03-29") %>%
  filter(wind_direction > 0) %>%
  filter(wind_speed > 0) %>%
  filter(Radiation >0) %>%
  filter(T_dry >0) %>%
  filter(RH > 0) %>%
  filter(WRF_CHEM_radiance > 0) %>%
  filter(WRF_CHEM_Temp > 0) %>%
  filter(WRF_CHEM_WD >0) %>%
  filter(WRF_CHEM_WS > 0) %>%
  filter(WRF_CHEM_Pressure > 0) %>%
  filter(WRF_CHEM_RH > 0)




stats <- all_data_NCSM_WRF %>%
  dplyr::group_by(date) %>%
  dplyr::summarize(AVG_WD_NCMS = mean(wind_direction),
            AVG_WD_WRF = mean(WRF_CHEM_WD),
            
            MAX_WS_NCMS = max(wind_speed),
            MAX_WS_WRF = max(WRF_CHEM_WS),
            
            AVG_Temp_NCMS = mean(T_dry),
            AVG_Temp_WRF = mean(WRF_CHEM_Temp),
            
            AVG_pressure_NCMS = mean(pressure),
            AVG_Pressure_WRF = mean(WRF_CHEM_Pressure),
            
            AVG_RH_NCMS = mean(RH),
            AVG_RH_WRF = mean(WRF_CHEM_RH),
            
            AVG_RAD_NCMS = mean(Radiation),
            AVG_RAD_WRF = mean(WRF_CHEM_radiance))
  
write.csv(stats, "SUMMARY_STATS_NCMS_WRF_MET_DATA.csv")  


##################################################
##### selected hours  (good for RH and pressure)

all_data_NCSM_WRF_selected_hours  <- all_data_NCSM_WRF %>%
  # filter(station %in% c("Al Faqa", "Madinat Zayed", "Hatta",
  #                       "Al Ain","Alkhazna", "Rezeen")) %>%
  filter(hour >= 6 & hour <= 8 )
  

stats_selected <- all_data_NCSM_WRF_selected_hours %>%
  dplyr::group_by(date) %>%
  dplyr::summarize(AVG_WD_NCMS = mean(wind_direction),
                   AVG_WD_WRF = mean(WRF_CHEM_WD),
                   
                   MAX_WS_NCMS = max(wind_speed),
                   MAX_WS_WRF = max(WRF_CHEM_WS),
                   
                   AVG_Temp_NCMS = mean(T_dry),
                   AVG_Temp_WRF = mean(WRF_CHEM_Temp),
                   
                   AVG_pressure_NCMS = mean(pressure),
                   AVG_Pressure_WRF = mean(WRF_CHEM_Pressure),
                   
                   AVG_RH_NCMS = mean(RH),
                   AVG_RH_WRF = mean(WRF_CHEM_RH),
                   
                   AVG_RAD_NCMS = mean(Radiation),
                   AVG_RAD_WRF = mean(WRF_CHEM_radiance))


write.csv(stats_selected, "SUMMARY_STATS_NCMS_WRF_MET_DATA_selected.csv")  



################################################################################
### summary stats for AQ data from WRF, MAIAC and measurements #################


# AQ_MAIAC_TERRA <- read.csv("D:/Dust_Event_UAE_2015/MAIAC_1km/TERRA/AQ_MAIAC_TERRA_2_April_2015_PM10.csv")
# AQ_MAIAC_AQUA <- read.csv("D:/Dust_Event_UAE_2015/MAIAC_1km/AQUA/AQ_MAIAC_AQUA_2_April_2015_PM10.csv")
# AQ_WRF <- read.csv("D:/Dust_Event_UAE_2015/WRF_trial_runs/AQ_Data_WRF_2_April_2015_PM10.csv")

AQ_MAIAC_TERRA <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/TERRA/AQ_MAIAC_TERRA_2_April_2015_PM10.csv")
AQ_MAIAC_AQUA <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/MAIAC_1km/AQUA/AQ_MAIAC_AQUA_2_April_2015_PM10.csv")
AQ_WRF <- read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/WRF_trial_runs/AQ_Data_WRF_2_April_2015_PM10.csv")



AQ_MAIAC <- rbind(AQ_MAIAC_TERRA,
                 AQ_MAIAC_AQUA)


# remove outlier from AQ_MAIAC

AQ_MAIAC <- AQ_MAIAC %>%
  filter(mean_value < 2500)

str(AQ_MAIAC)
AQ_MAIAC <- AQ_MAIAC %>%
  mutate(date = ymd_hms(DATETIME)) 
AQ_MAIAC <- AQ_MAIAC %>%
  mutate(date = date(DATETIME))


str(AQ_WRF)
AQ_WRF <- AQ_WRF %>%
  mutate(date = ymd_hms(DATETIME)) 
AQ_WRF <- AQ_WRF %>%
  mutate(date = date(DATETIME))



# omit NA vlaues
AQ_MAIAC <- na.omit(AQ_MAIAC)
AQ_WRF <- na.omit(AQ_WRF)

AQ_WRF <- AQ_WRF %>%
  filter(Value < 2500)

AQ_MAIAC <- AQ_MAIAC %>%
  group_by(date) %>%
  summarise(AVG_PM10_meas = mean(mean_value, na.rm = T),
            AVG_PM10_MAIAC = mean(MODIS, nam.rm = T))


AQ_WRF <- AQ_WRF %>%
  group_by(date) %>%
  summarise(AVG_PM10_meas = mean(Value, na.rm = T),
            AVG_PM10_WRFCHEM = mean(WRF_CHEM, nam.rm = T))



AQ_WRF_MAIAC <- cbind(AQ_MAIAC,
                      AQ_WRF[1:6,])

write.csv(AQ_WRF_MAIAC, "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/summary_stats_PM10.csv")





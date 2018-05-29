
# Load packages
library(hyr)
library(openair)
library(sp)
library(maptools)
library(dplyr)
library(plyr)
library(threadr)


##############################################################################################################
##############################################################################################################

#run HYSPLIT
# something general about hysplit: http://www.arl.noaa.gov/HYSPLIT_info.php
# link to NOAA to install hyspliy on any laptop
# http://ready.arl.noaa.gov/HYSPLIT.php
# http://www.arl.noaa.gov/documents/workshop/Spring2007/HTML_Docs/index.html


# met data
# https://ready.arl.noaa.gov/documents/Tutorial/html/meteo_ftp.html
# ftp://arlftp.arlhq.noaa.gov/archives/reanalysis/

# need to download RP201609.gbl first  from  ftp://arlftp.arlhq.noaa.gov/archives/reanalysis/
# use wet command in linux

# # Calculate back trajectories for Auchencorth Moss for 2015
# data_moss <- run_hysplit(
#   latitude = 55.79216, 
#   longitude = -3.2429, 
#   runtime = -96, 
#   start_height = 10, 
#   model_height = 10000, 
#   start = "2016-11-01",
#   end = "2016-11-31",
#   # hysplit_exec = "/AQprojects/hysplit_package/exec", 
#   # hysplit_input = "/AQprojects/hysplit_package/hysplit_met_data", 
#   # hysplit_output = "/AQprojects/hysplit_package/hysplit_output",
#   hysplit_exec = "C:/hysplit4/exec", # run "hyts_std"
#   hysplit_input = "C:/hysplit4/hysplit_met_data", 
#   hysplit_output = "C:/hysplit4/hysplit_output",
# site = "acth")
# 
# # plot backtrajectories
# trajPlot(data_moss)


#################################################################################

#################################################################################
#################################################################################

# Abu Dhabi, Dubai, Ryhad
lat <- as.list(c(24.466667, 25.2048, 24.7136, 15))
lon <- as.list(c(54.366669, 55.2708, 46.6753, 47))

# Abu Dabi Airport
lat <- as.list(c(24.4419))
lon <- as.list(c(54.6501))

# Dawit
# lon= as.list(c(52.566943999999999, 54.748610999999997, 54.514443999999997, 54.366667000000000, 54.430664000000000, 52.631110999999997, 54.229444000000001, 53.494722000000003, 54.576600999999997, 53.439388000000001, 52.730556000000000, 51.783332999999999))
# lat= as.list(c(24.165278000000001, 24.815833000000001, 24.438611000000002, 24.466667000000001, 24.006326000000001, 24.191666999999999, 24.307777999999999, 24.091111000000001, 24.421554000000000, 24.120435000000001, 24.110278000000001, 24.050000000000001))

# met data are the GFS data


# ID= [0 , 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]


# UAE_all <- NULL
# 
# for (i in 1:length(lat)) {
#   data_UAE <- run_hysplit(
#     lat[i], lon[i],
#     interval = "1 hour",
#     runtime = -120,
#     start_height = 10,
#     model_height = 1000,
#     start = "2015-03-29",     # always load the month before (RP201502.gbl)
#     end = "2015-04-03",
#     # start = "15/03/28 01:00:00",
#     # end = "15/03/28 03:00:00",
#     hysplit_exec = "C:/hysplit4/exec", # run "hyts_std"
#     hysplit_input = "C:/hysplit4/hysplit_met_data",
#     hysplit_output = "C:/hysplit4/hysplit_output",
#     site = "UAE")
#   # plot backtrajectories
# trajPlot(data_UAE, orientation = c(0, 0, 0))
# data_UAE$ID <- i-1
# UAE_all <- rbind(UAE_all, data_UAE)
# }

# plot backtrajectories
# trajPlot(data_abudhabi, orientation = c(0, 0, 0))

# write.csv(UAE_all, "Z:/_SHARED_FOLDERS/Air Quality/hysplit_December_20_2016.csv")

###########################################################################
######### remember to empty the hysplit_output folder #####################
###########################################################################

# Abu Dhabi
data_abudhabi <- run_hysplit(
  latitude = 24.4419,
  longitude = 54.6501,
  interval = "1 hour",
  runtime = -72,
  start_height = 500,
  model_height = 10000, #10000,
  start = "2015-04-02",   # always load the month before (RP201502.gbl)
  end = "2015-04-03",
  hysplit_exec = "C:/hysplit4/exec", # run "hyts_std"
  hysplit_input = "C:/hysplit4/hysplit_met_data",
  hysplit_output = "C:/hysplit4/hysplit_output",
  site = "abudhabi")



# EXPO 2020 site (DUBAI)
data_EXPO2020 <- run_hysplit(
  latitude = 24.9666223,
  longitude = 55.1460984,
  interval = "1 hour",
  runtime = -72,
  start_height = 500,
  model_height = 10000, #10000,
  start = "2018-02-13",   # always load the month before (RP201502.gbl)
  end = "2018-02-14",
  hysplit_exec = "C:/hysplit4/exec", # run "hyts_std"
  hysplit_input = "C:/hysplit4/hysplit_met_data",
  hysplit_output = "C:/hysplit4/hysplit_output",
  site = "EXPO2020")


# save(file = "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/hysplit/Abu_Dhabi_HYPSLIT.RData", data_abudhabi)

# plot backtrajectories

# trajPlot(data_abudhabi, orientation = c(0, 0, 0),projection = "lambert",
#          col = "blue", lwd = 2, key.pos = "left", key.col = 1, grid.col = "transparent",
#          xlim= c(48, 57), ylim = c(10,40), map.cols = "white", map.alpha = 0.4)

trajPlot(data_EXPO2020, orientation = c(0, 0, 0),projection = "lambert",
         col = "blue", lwd = 2, key.pos = "left", key.col = 1, grid.col = "transparent",
         xlim= c(48, 57), ylim = c(10,40), map.cols = "white", map.alpha = 0.4)

# 500, 1000, 1500, 2000


# # yemen
# data_yemen <- run_hysplit(
#   latitude = 15,
#   longitude = 47,
#   interval = "1 hour",
#   runtime = -96,
#   start_height = 10,
#   model_height = 1000, #10000,
#   start = "2015-03-29",   # always load the month before (RP201502.gbl)
#   end = "2015-04-04",
#   hysplit_exec = "C:/hysplit4/exec", # run "hyts_std"
#   hysplit_input = "C:/hysplit4/hysplit_met_data",
#   hysplit_output = "C:/hysplit4/hysplit_output",
#   site = "abudhabi")
# 
# 
# # plot backtrajectories
# trajPlot(data_yemen, orientation = c(0, 0, 0))
# 
# 
# 
# # saudi_desert
# data_saudi_desert <- run_hysplit(
#   latitude = 19,
#   longitude = 49,
#   interval = "1 hour",
#   runtime = -96,
#   start_height = 10,
#   model_height = 1000, #10000,
#   start = "2015-03-29",   # always load the month before (RP201502.gbl)
#   end = "2015-04-04",
#   hysplit_exec = "C:/hysplit4/exec", # run "hyts_std"
#   hysplit_input = "C:/hysplit4/hysplit_met_data",
#   hysplit_output = "C:/hysplit4/hysplit_output",
#   site = "abudhabi")
# 
# 
# # plot backtrajectories
# trajPlot(data_saudi_desert, orientation = c(0, 0, 0))
# 
# 
# 
# 
# 
# # saudi_desert
# data_rhyad <- run_hysplit(
#   latitude = 24.7136,
#   longitude = 46.6753,
#   interval = "1 hour",
#   runtime = -96,
#   start_height = 10,
#   model_height = 1000, #10000,
#   start = "2015-03-29",   # always load the month before (RP201502.gbl)
#   end = "2015-04-04",
#   hysplit_exec = "C:/hysplit4/exec", # run "hyts_std"
#   hysplit_input = "C:/hysplit4/hysplit_met_data",
#   hysplit_output = "C:/hysplit4/hysplit_output",
#   site = "abudhabi")
# 
# 
# # plot backtrajectories
# trajPlot(data_rhyad, orientation = c(0, 0, 0))




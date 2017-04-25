
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

# Calculate back trajectories for Auchencorth Moss for 2015
data_moss <- run_hysplit(
  latitude = 55.79216, 
  longitude = -3.2429, 
  runtime = -96, 
  start_height = 10, 
  model_height = 10000, 
  start = "2016-11-01",
  end = "2016-11-31",
  # hysplit_exec = "/AQprojects/hysplit_package/exec", 
  # hysplit_input = "/AQprojects/hysplit_package/hysplit_met_data", 
  # hysplit_output = "/AQprojects/hysplit_package/hysplit_output",
  hysplit_exec = "C:/hysplit4/exec", # run "hyts_std"
  hysplit_input = "C:/hysplit4/hysplit_met_data", 
  hysplit_output = "C:/hysplit4/hysplit_output",
site = "acth")

# plot backtrajectories
trajPlot(data_moss)


###########################################################################

#################################################################################
#################################################################################


lat <- as.list(c(24.466667, 25.2048, 24.7136))
lon <- as.list(c(54.366669, 55.2708, 46.6753))


lat <- as.list(c(24.466667))
lon <- as.list(c(54.366669))


for (i in 1:length(lat)) {
  data_UAE <- run_hysplit(
    lat[i], lon[i],
    interval = "1 hour",
    runtime = -96,
    start_height = 10,
    model_height = 10000,
    start = "2015-03-31",
    end = "2015-04-02",
    # start = "15/03/28 01:00:00",
    # end = "15/03/28 03:00:00",
    hysplit_exec = "C:/hysplit4/exec", # run "hyts_std"
    hysplit_input = "C:/hysplit4/hysplit_met_data",
    hysplit_output = "C:/hysplit4/hysplit_output",
    site = "UAE")
  # plot backtrajectories
  trajPlot(data_UAE, orientation = c(0, 0, 0))
}




# # Abu Dhabi
# data_abudhabi <- run_hysplit(
#   latitude = 24.466667,
#   longitude = 54.366669,
#   interval = "1 hour",
#   runtime = -96,
#   start_height = 10,
#   model_height = 10000,
#   start = "2015-03-28",   # always load the month before (RP201502.gbl)
#   end = "2015-03-28",
#   hysplit_exec = "C:/hysplit4/exec", # run "hyts_std"
#   hysplit_input = "C:/hysplit4/hysplit_met_data",
#   hysplit_output = "C:/hysplit4/hysplit_output",
#   site = "abudhabi")
# 
# 
# # plot backtrajectories
# trajPlot(data_abudhabi, orientation = c(0, 0, 0))
# 
# 




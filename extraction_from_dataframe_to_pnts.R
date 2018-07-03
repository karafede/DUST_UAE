

### functions needed for the extraction (extract points from a a dataframe according to a list of coordinates x & y)
extra_dataframe <- function(x=x, y=y, mydata=mydata, lon_lat= c("x", "y")){
  # x is the longitude of the point to be extracted
  # y is the latitude of the point to be extracted
  # mydata is the dataframe to be extracted from. mydata should contain the x y columns or lon and lat names provided
  # as lon_lat i=2
  
  library(NISTunits)
  all_point<- NULL
  for (i in 1:length(x)){
    a = (sin(NISTdegTOradian(mydata[[lon_lat[2]]]-y[i])/2))^2  + (cos(NISTdegTOradian(mydata[[lon_lat[2]]]))) * (cos (NISTdegTOradian(y[i]))) * (sin(NISTdegTOradian(mydata[[lon_lat[1]]]-x[i])/2))^2
    c = 2*atan2( sqrt(a), sqrt((1-a)))
    d = 6371*c # radius of the earth in km
    min_y <- which(d == min(d))
    close_point<- mydata[min_y,]
    all_point<-rbind(all_point,close_point)
  }
  return(all_point)
}

source("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR/new_analysis_2013_2015/Validation_2016/extract_pnt_raster.r")
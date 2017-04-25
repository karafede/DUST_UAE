extract_points<- function(raster=new_res, input_stations=AQ_data_PM25){
  ##### this function extracts point values from a raster. the points are assigned a value of a pixel closest to the point.
  ##### the value of the outcome only contains values without NA of the masked area
  
  value_point_all<-data.frame()
  site_name_all<-data.frame()
  for (i in 1:nrow(input_stations)){
    raster_data<- rasterToPoints(raster)
    a = (sin(NISTdegTOradian(input_stations$Latitude[i]-raster_data[,2])/2))^2  + (cos(NISTdegTOradian(input_stations$Latitude[i]))) * (cos (NISTdegTOradian(raster_data[,2]))) * (sin(NISTdegTOradian(input_stations$Longitude[i]-raster_data[,1])/2))^2
    c = 2*atan2( sqrt(a), sqrt((1-a)))
    d = 6371*c # radius of the earth in km
    min_y <- which(d == min(d))
    value_point<- raster_data[min_y,3]
    
    value_point_all<- rbind(value_point_all,value_point)
    names(value_point_all)<- "values_extr"
    # site_name <-  as.character(input_stations$Site[2])
    # site_name_all<-rbind(site_name_all, site_name)
    # names(site_name_all)<- "Site"
  }
  #value_point_all<-cbind(value_point_all, site_name_all)
  return(value_point_all)
}

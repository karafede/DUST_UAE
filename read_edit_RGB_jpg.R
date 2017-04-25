
library(jpeg)

setwd("D:/Dust_Event_UAE_2015/SEVIRI_20150402_outputs/I_method/RGB_images/20150401")


img <- readJPEG("RGB_20150401_201.jpg", native = TRUE)

#this will display your image to test you read it correctly
if(exists("rasterImage")){
  plot(1:2, type='n')
  rasterImage(img,1,1,2,2)
}




### correlation 
z <- stack(raste_AVW_chlo_arr,raste_AOD_stack_arr)

r <- calc(z, fun=function(x) cor(x[1:42], x[55:96], method='spearman', use= "pairwise.complete.obs" ))




#### second method

dawit_cho<- values(raste_AVW_chlo_arr[[1:48]])
dawit_AOD<- values(raste_AOD_stack)

data_cor<- data.frame()
for (i in 1:nrow(dawit_cho)){
  
  dada<-cor(dawit_cho[i,],dawit_AOD[i,], use= "pairwise.complete.obs")
  data_cor<- rbind(data_cor,dada)
  
}
check_spa<-rasterToPoints(raste_AOD_stack[[1]])
colnames(check_spa)[3] <- "val"
check_spa<- cbind(check_spa[,-3],data_cor)
rr<- rasterFromXYZ(check_spa )
plot(rr)
hist((rr),breaks= 100)
plot(rr-r)


### 

season_in<- da



plot(raste_AVW_chlo_arr[[1]]) # One exemplary layer for orientation


values <- click(raste_AVW_chlo_arr, n=1)
plot(raste_AOD_stack_arr[[1]]) # One exemplary layer for orientation


values_2 <- click(raste_AOD_stack_arr, n=1)



# Compose and plot dataframe
timeseries <- data.frame(year = c(2000, 2005, 2010, 2015),
                         values = values[1, ])
plot(timeseries, type="l")






plot(raste_AOD_stack$X2011.02.)

plot(raste_AVW_chlo_arr$X20110101.20110131)

as_data<- as.data.frame(r@data)
dd<-as.vector(as_data$layer)
ind<- which(is.na(dd))
mean(as_data$layer,na.rm = T)



hist((rr-r), breaks=100)




dada<-cor(dawit_cho[,1:48],dawit_AOD, use= "complete.obs")


library(raster)

s <- stack(NDVI, rainfall)
fun=function(x) {
  if (is.na(x[1])){ NA 
  } else { 
    lm(x[1:120] ~ x[121:240])$coefficients[2] 
  }
}
slope <- calc(s, fun)





files_year<- str_sub(files_chlor, start=1, end=4)
files_mon<- str_sub(files_chlor, start=5, end=6)
raste_AVW_chlo<-stack()
for (ii in 1:(length(files_year)-2)){
  ii=1
  raster_avw<- raster(paste0("D:/PhD Masdar/COURSE PROJECT/DATA/Chlorophl/data/AVW/",files_chlor[ii],sep=""))
  raste_AVW_chlo<- stack(raste_AVW_chlo,raster_avw)
}
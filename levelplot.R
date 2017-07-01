

####### color pallet

cool = rainbow(100, start=rgb2hsv(col2rgb('green'))[1], end=rgb2hsv(col2rgb('blue'))[1])
warm = rainbow(100, start=rgb2hsv(col2rgb('red'))[1], end=rgb2hsv(col2rgb('green'))[1])
#middle = rainbow(215, start=rgb2hsv(col2rgb('#FF8600FF'))[1], end=rgb2hsv(col2rgb('green'))[1])
cols = c(rev(cool),  rev(warm))




##### Intercept coefficents ####


data_frame_intercept<- data.frame()
Intercept_ras_stack<- stack()
for (kk in 1:12){
  #kk=1
  nam <- paste("gwrG_pnt_", month.name[kk] , sep = "")
  model_value <- loadOneName( eval(nam), file=paste0(load_folder, "GWR_file.RData"))
  dawi<- data.frame(model_value$SDF)
  ras_r2<- rasterFromXYZ( dawi[, c("coord.x","coord.y", "X.Intercept.")])
  r2_data<- dawi[, c( "X.Intercept.")]
  names(ras_r2)<- month.name[kk]
  month_ind<- cbind(r2_data, rep (kk, length(r2_data)))
  colnames(month_ind)<-c("Intercept", "month_ind")
  data_frame_intercept<-rbind(data_frame_intercept,month_ind)
  Intercept_ras_stack<- stack(Intercept_ras_stack,ras_r2)
}

# output_folder<- "D:/Air Quality/GWR_with_met/Result/Images/Results of 70_30 rm RH/"


data_frame_intercept<- cbind(data_frame_intercept, month.name[data_frame_intercept$month_ind])
colnames(data_frame_intercept)[3]<- "Month"

#### Ploting the WS maps

library(viridis)
library(lattice)

dir <- "D:/Air Quality/GWR/UAE_boundary"
shp_UAE <- readOGR(dsn = dir, layer = "uae_emirates")
shp_UAE <- spTransform(shp_UAE, CRS("+init=epsg:4326"))
plot(shp_UAE)


max_val<-ceiling(max(maxValue(Intercept_ras_stack)))
min_val<-floor(min(minValue(Intercept_ras_stack)))

stat_dat<- summary(as.vector(Intercept_ras_stack))
IQR<- floor(as.numeric((stat_dat[5]-stat_dat[2])* 2))# n is the space after IQR


low_IQR<- if(floor(min_val) > floor(as.numeric((stat_dat[2]- IQR)))) floor(min_val) else floor(as.numeric((stat_dat[2]- IQR)))
high_IQR<- if(ceiling(max_val) < ceiling(as.numeric((stat_dat[5]+IQR)))) floor(max_val) else ceiling(as.numeric((stat_dat[5]+IQR)))


vec_all<- as.vector(Intercept_ras_stack)
# vec_all<-vec_all[ vec_all >= low_IQR & vec_all <= high_IQR & !is.na(vec_all) ]

xxx<- pretty( vec_all, n=10)
xxx<- (c(min_val, xxx, max_val))

AOD_plot <-Intercept_ras_stack
# AOD_plot[AOD_plot < low_IQR ]<- low_IQR
# AOD_plot[ AOD_plot >  high_IQR]<- high_IQR

## 


### plots of maps 

h <- rasterVis::levelplot(AOD_plot, 
                          margin=FALSE, main= "Intercept" ,
                          ## about colorbar
                          colorkey=list(
                            space='right',                   
                            labels= list(at= floor(as.numeric( seq(low_IQR, high_IQR, length.out=7))),
                                         font=3),
                            axis.line=list(col='black'),
                            width=0.75,
                            title=expression(paste("     ", mu,"g ",m^-3) )
                          ),   
                          ## about the axis
                          par.settings=list(
                            strip.border=list(col='transparent'),
                            strip.background=list(col='transparent'),
                            axis.line=list(col='black')
                          ),
                          scales=list(draw=T, alternating= F),            
                          #col.regions = colorRampPalette(c("blue", "white","red"))(1e3),
                          col.regions = cols,
                          at=unique(c(seq(low_IQR, high_IQR, length.out=200))),
                          # at=c(seq(stat_dist[1], ceiling(stat_dist[6]), length.out=256)),
                          names.attr=rep(names(AOD_plot))) +
  latticeExtra::layer(sp.polygons(shp_UAE))
#h


png(paste0(output_folder,"Coefficients_intercept.png"),
    width = 1680, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print(h)
dev.off()


### plots of histograms

#vec_all<- as.vector(AOD_plot)
png(paste0(output_folder,"hist_Intercept.png"),
    width = 1680, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print({
  histogram(vec_all,  breaks=500 , main = paste("Histogram of Intercepts"), type="percent",
            xlab= list(expression(beta),cex = 1.5) , ylab=list("%",cex = 1.5 ), col="black", scales=list(cex = 1.5))
})
dev.off()




rm(list = ls()[!ls() %in% c( "output_folder", "cols", "shp_UAE","loadOneName","load_folder")])







#####


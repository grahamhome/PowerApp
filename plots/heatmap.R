library(ggplot2)
library(ggmap)
library(rgdal)
library(raster)
library(akima)
library(sp)

source("data/import_tenn.R")
import_data()
fnames <- function(){
  n <- list(Heatmap="heatmap",
            Voltage="plot_heatmapvolt",
            Frequency="plot_heatmapfreq")
  n
}

#update covariance matrix of the frequency values, starting from whatever the previous time (curr_sf)
# was to whatever "time" is. 
update_covbus_freq <- function(time) {
  if (curr_sf<time&&curr_sf>2) {
    cf <- curr_sf
  }
  else{
    cf <- 3
    xfbar <- colMeans(Xf[1:2,])
    Sf <- cov(Xf[1:2,])
  }
  for (t in cf:time) {
    xn1 <- Xf[t,]
    Sf <- (((t-1)/t)*Sf)+
      ((1/t+1)*(xn1-xfbar)%*%(t((xn1-xfbar))))
    xfbar <- xfbar + (1/(t+1))*(xn1-xfbar)
  }
  assign("xfbar",xfbar,envir = .GlobalEnv)
  assign("curr_sf",time,envir = .GlobalEnv)
  assign("Sf",Sf,envir = .GlobalEnv)
}
#update covariance matrix of the voltage values, starting from whatever the previous time (curr_sv)
# was to whatever "time" is. 
update_covbus_volt <- function(time) {
  #This is in case we call this function on a timepoint before the last one we left off at; if that is the case we want to make sure the loop
  # is starting at time=3 and the xbar is only the mean of the first 2 timepoints.
  if (curr_sv<time&&curr_sv>2) {
    cv <- curr_sv
  }
  else{
    cv <- 3
    xvbar <- colMeans(Xv[1:2,])
    Sv <- cov(Xv[1:2,])
  }
  for (t in cv:time) {
    xn1 <- Xv[t,]
    Sv <- (((t-1)/t)*Sv)+
      ((1/t+1)*(xn1-xvbar)%*%(t((xn1-xvbar))))
    xvbar <- xvbar + (1/(t+1))*(xn1-xvbar)
  }
  assign("xvbar",xvbar,envir = .GlobalEnv)
  assign("curr_sv",time,envir = .GlobalEnv)
  assign("Sv",Sv,envir = .GlobalEnv)
}

mincovf <- 1
maxcovf <- 0
for (t in 1:nrow(Freq)) {
  update_covbus_freq(t)
  mincovf <- ifelse(min(Sf[,])<mincovf,min(Sf[,]),mincovf)
  maxcovf <- ifelse(max(Sf[,])>maxcovf,max(Sf[,]),maxcovf)
}
mincovv <- 1
maxcovv <- 0
for (t in 1:nrow(Volt)) {
  update_covbus_volt(t)
  mincovv <- ifelse(min(Sv[,])<mincovv,min(Sv[,]),mincovv)
  maxcovv <- ifelse(max(Sv[,])>maxcovv,max(Sv[,]),maxcovv)
}


get_busline_voltcov <- function(time){
  update_covbus_volt(time)
  for (x in 1:nrow(linesb)) {
    curr_row <- linesb[x,]
    curr_row$Variance <- as.numeric(as.character(Sv[[curr_row$From.Bus.Name,curr_row$To.Bus.Name]]))
    linesb[x,"Variance"] <- curr_row$Variance
  }
  linesb
}
get_busline_freqcov <- function(time){
  update_covbus_freq(time)
  for (x in 1:nrow(linesb)) {
    curr_row <- linesb[x,]
    curr_row$Variance <- as.numeric(as.character(Sf[[curr_row$From.Bus.Name,curr_row$To.Bus.Name]]))
    linesb[x,"Variance"] <- curr_row$Variance
  }
  linesb
}

get_corr_neighbors_volt <- function(time){
  linesb <-get_busline_voltcov(time)
  for (x in 1:nrow(bus_locs)) {
    curr_row <- bus_locs[x,]
    
  }
}

#Change the frequency column of bus_locs with the frequencies for a given time 
update_freq <- function(time){
  tf <- t(Freq[time,-1])
  tf <- cbind(rownames(tf),tf)
  colnames(tf) <- c("Bus.Name","Frequency")
  bus_locs <- merge(subset(bus_locs,select = c("Bus.Num","Bus.Name","Sub.Name","Latitude","Longitude","Voltage")),tf, by="Bus.Name")
  bus_locs$Frequency <- as.numeric(as.character(bus_locs$Frequency))
  assign("bus_locs",bus_locs,envir = .GlobalEnv)
}
#Change the voltage column of bus_locs with the frequencies for a given time
update_volt <- function(time){
  vf <- t(Volt[time,-1])
  vf <- cbind(rownames(vf),vf)
  colnames(vf) <- c("Bus.Name","Voltage")
  bus_locs <- merge(subset(bus_locs,select = c("Bus.Num","Bus.Name","Sub.Name","Latitude","Longitude","Frequency")),vf, by="Bus.Name")
  bus_locs$Voltage <- as.numeric(as.character(bus_locs$Voltage))
  assign("bus_locs",bus_locs,envir = .GlobalEnv)
} 
update_volt(1)
update_freq(1)



plot_heatmapvolt<- function(t){
  update_volt(t)
  xmn <- min(bus_locs$Longitude)
  xmx <- max(bus_locs$Longitude)
  ymn <- min(bus_locs$Latitude)
  ymx <- max(bus_locs$Latitude)
  intp_coords <- interp(bus_locs$Longitude, bus_locs$Latitude, bus_locs$Voltage, duplicate = "mean",
                        xo=seq(xmn,xmx, by=0.1),
                        yo=seq(ymn,ymx, by=0.1))
  r <- raster(intp_coords)
  
  rtp <- rasterToPolygons(r)
  rtp@data$id <- 1:nrow(rtp@data)   # add id column for join
  
  rtpFort <- fortify(rtp, data = rtp@data)
  rtpFortMer <- merge(rtpFort, rtp@data, by.x = 'id', by.y = 'id')  # join data
  g <- g + geom_polygon(data = rtpFortMer, 
                        aes(x = long, y = lat, group = group, fill = layer), 
                        alpha = 0.5, 
                        size = 0) +  ## size = 0 to remove the polygon outlines
    scale_fill_gradientn("Voltage",colours = topo.colors(255),limits=c(min(Volt[,-1]),max(Volt[,-1])))+
    theme(legend.position="right",legend.direction="vertical",legend.box="horizontal") +
    ggtitle(bquote(atop("Voltage at Time",atop(.(Volt[t,1]),""))))
  g
}
plot_heatmapfreq<- function(t){
  update_freq(t)
  xmn <- min(bus_locs$Longitude)
  xmx <- max(bus_locs$Longitude)
  ymn <- min(bus_locs$Latitude)
  ymx <- max(bus_locs$Latitude)
  intp_coords <- interp(bus_locs$Longitude, bus_locs$Latitude, bus_locs$Frequency, duplicate = "mean",
                        xo=seq(xmn,xmx, by=0.1),
                        yo=seq(ymn,ymx, by=0.1))
  r <- raster(intp_coords)
  rtp <- rasterToPolygons(r)
  rtp@data$id <- 1:nrow(rtp@data)   # add id column for join
  rtpFort <- fortify(rtp, data = rtp@data)
  rtpFortMer <- merge(rtpFort, rtp@data, by.x = 'id', by.y = 'id')  # join data
  g <- g + geom_polygon(data = rtpFortMer, 
                        aes(x = long, y = lat, group = group, fill = layer), 
                        alpha = 0.5, 
                        size = 0) +  ## size = 0 to remove the polygon outlines
    #scale_fill_gradientn("Frequency",colours = topo.colors(255))+
    scale_fill_gradientn("Frequency",colours = c("green","blue","orange","yellow"),limits=c(min(Freq[,-1]),max(Freq[,-1])))+
    theme(legend.position="right",legend.direction="vertical",legend.box="horizontal") +
    ggtitle(bquote(atop("Frequency at Time",atop(.(Freq[t,1]),""))))
  g
}


#rtp <- rasterToPolygons(x)
#rtp@data$id <- 1:nrow(rtp@data)   # add id column for join
#rtpFort <- fortify(rtp, data = rtp@data)
#rtpFortMer <- merge(rtpFort, rtp@data, by.x = 'id', by.y = 'id')
#g <- g+ geom_polygon(data = rtpFortMer, aes(x = long, y = lat, group = group, fill = layer), 
 #                    alpha = 0.5, 
  #                   size = 0) +  ## size = 0 to remove the polygon outlines
  #scale_fill_gradientn(colours = topo.colors(255))
#pts <- subset(bus_locs,select=c("Longitude","Latitude","Voltage"))
#coordinates(pts) = c("Longitude","Latitude")
#rvolt <- rasterFromXYZ(pts)
#colnames(pts) <- c('x','y','z')
#e <- extent(pts[,1:2])
#r <- raster(e,ncol=100,nrow=2)
#x <- rasterize(pts[, 1:2], r, pts[,3], fun=mean)

#r <- rasterize(pts[,pts$Longitude:pts$Latitude],pts$Voltage,fun=mean)
#rst <- vect2rast(obj = pts)

#kr = autoKrige(Voltage~1, meuse, meuse.grid)
#dat = as.data.frame(kr$krige_output)

#ggplot(aes(x = x, y = y, fill = var1.pred), data = dat) + 
#  geom_tile() + 
#  scale_fill_continuous(low = "white", high = muted("blue"))


#loadMeuse()
#bb = bbox(meuse)
#grd = spsample(meuse, type = "regular", n = 4000)
#mn_value = sapply(1:length(grd), function(pt) {
#  d = spDistsN1(meuse, grd[pt,])
#  return(mean(meuse[d < 1000,]$cadmium))
#})



#update_volt(100)
#xmn <- min(bus_locs$Longitude)
#xmx <- max(bus_locs$Longitude)
#ymn <- min(bus_locs$Latitude)
#ymx <- max(bus_locs$Latitude)
#intp_coords <- interp(bus_locs$Longitude, bus_locs$Latitude, bus_locs$Voltage, duplicate = "mean",
#                   xo=seq(xmn,xmx, by=0.1),
#                   yo=seq(ymn,ymx, by=0.1))
#r <- raster(intp_coords)

#rtp <- rasterToPolygons(r)
#rtp@data$id <- 1:nrow(rtp@data)   # add id column for join

#rtpFort <- fortify(rtp, data = rtp@data)
#rtpFortMer <- merge(rtpFort, rtp@data, by.x = 'id', by.y = 'id')  # join data
#g <- ggmap(mapten)+scale_x_continuous(limits = c(-90.6, -81), expand = c(0, 0)) +scale_y_continuous(limits = c(34.5, 37), expand = c(0, 0))
#g <- g + geom_polygon(data = rtpFortMer, 
#                       aes(x = long, y = lat, group = group, fill = layer), 
#                       alpha = 0.5, 
#                       size = 0) +  ## size = 0 to remove the polygon outlines
#  scale_fill_gradientn(colours = topo.colors(255))
#g
#plot(bus_locs$Latitude ~ bus_locs$Longitude, data = bus_locs, pch = 3, cex = 0.5,
 #    xlab = "Longitude", ylab = "Latitude")
#image(intp_coords, add=TRUE, col = rainbow(100, alpha = 1))
#data4 <- data.frame(expand.grid(X=intp_coords$x, Y=intp_coords$y), z=c(intp_coords$z))
#a<-data.frame(cbind(data4$X, data4$Y))
#coordinates(a) = ~X1 + X2
#proj4string(a) <-CRS("+proj=utm +zone=10 +datum=WGS84")
# use spTransform now
#a1 <- spTransform(a,CRS("+proj=longlat"))
# inspect output
#head(coordinates(a1)) 
# insert lat-longs back into data
#data4$long <-  a1$X1
#data4$lat <-   a1$X2
#data4 <- na.omit(data4)
#ggplot(data4) + 
#  geom_tile(aes(X, Y, fill=z)) + 
#  scale_fill_gradient(low="white", high="black", space = "Lab") + 
#  coord_fixed(ratio = 1)




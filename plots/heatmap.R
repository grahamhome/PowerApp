library(ggplot2)
library(ggmap)
library(rgdal)
library(raster)
library(rgeos)
library(akima)
library(sp)
library(doSNOW)
library(animation)
#library(gdata)
#library(spatstat)
#library(data.table)
#library(parallel)
library(doParallel)
fnames <- function(){
  n <- list(Heatmap="heatmap",
            Voltage="plot_heatmapvolt",
            Frequency="plot_heatmapfreq")
  if (exists("Pangle")) {
    n <- c(n,Angle="plot_heatmapangle")
  }
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

#mincovf <- 1
#maxcovf <- 0
#for (t in 1:nrow(Freq)) {
#  update_covbus_freq(t)
#  mincovf <- ifelse(min(Sf[,])<mincovf,min(Sf[,]),mincovf)
#  maxcovf <- ifelse(max(Sf[,])>maxcovf,max(Sf[,]),maxcovf)
#}
#mincovv <- 1
#maxcovv <- 0
#for (t in 1:nrow(Volt)) {
#  update_covbus_volt(t)
#  mincovv <- ifelse(min(Sv[,])<mincovv,min(Sv[,]),mincovv)
#  maxcovv <- ifelse(max(Sv[,])>maxcovv,max(Sv[,]),maxcovv)
#}


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
  #assign("bus_locs",bus_locs,envir = .GlobalEnv)
  bus_locs
}
#Change the voltage column of bus_locs with the frequencies for a given time
update_volt <- function(time){
  vf <- t(Volt[time,-1])
  vf <- cbind(rownames(vf),vf)
  colnames(vf) <- c("Bus.Name","Voltage")
  bus_locs <- merge(subset(bus_locs,select = c("Bus.Num","Bus.Name","Sub.Name","Latitude","Longitude","Frequency")),vf, by="Bus.Name")
  bus_locs$Voltage <- as.numeric(as.character(bus_locs$Voltage))
  #assign("bus_locs",bus_locs,envir = .GlobalEnv)
  bus_locs
} 
update_pangle <- function(time){
  ta <- t(Pangle[time,-1])
  ta <- cbind(rownames(ta),ta)
  colnames(ta) <- c("Bus.Name","Angle")
  bus_locs <- merge(subset(bus_locs,select = c("Bus.Num","Bus.Name","Sub.Name","Latitude","Longitude","Voltage","Frequency")),ta, by="Bus.Name")
  bus_locs$Angle <- as.numeric(as.character(bus_locs$Angle))
  #assign("bus_locs",bus_locs,envir = .GlobalEnv)
  bus_locs
}


plot_heatmapangle<- function(t){
  bus_locs <- update_pangle(t)
  xmn <- min(bus_locs$Longitude)
  xmx <- max(bus_locs$Longitude)
  ymn <- min(bus_locs$Latitude)
  ymx <- max(bus_locs$Latitude)
  intp_coords <- interp(bus_locs$Longitude, bus_locs$Latitude, bus_locs$Angle, duplicate = "mean",
                        xo=seq(xmn,xmx, by=0.04),
                        yo=seq(ymn,ymx, by=0.05))
  r <- raster(intp_coords)
  rtp <- rasterToPolygons(r)
  
  rtp@data$id <- 1:nrow(rtp@data)   # add id column for join
  rtpFort <- fortify(rtp, data = rtp@data)
  rtpFortMer <- merge(rtpFort, rtp@data, by.x = 'id', by.y = 'id')  # join data
  g <- g + geom_polygon(data = rtpFortMer, 
                        aes(x = long, y = lat, group = group, fill = layer), 
                        alpha = 0.5, 
                        size = 0) +  ## size = 0 to remove the polygon outlines
    scale_fill_gradientn("Angle",colours = c("red","yellow","green","blue","black"),limits=c(-90,90))+
    theme(legend.position="right",legend.direction="vertical",legend.box="horizontal") +
    ggtitle(bquote(atop("Phase Angle at Time",atop(.(Pangle[t,1]),""))))
  g
}

plot_heatmapvolt<- function(t){
  bus_locs <- update_volt(t)
  xmn <- min(bus_locs$Longitude)
  xmx <- max(bus_locs$Longitude)
  ymn <- min(bus_locs$Latitude)
  ymx <- max(bus_locs$Latitude)
  # n_cores <- detectCores()-1
  # cl<-makeCluster(n_cores)
  # registerDoParallel(cl)
  # xlen <- (xmx-xmn)/n_cores
  # ylen <- (ymx-ymn)/n_cores
  # plist <- foreach(n=1:n_cores, .packages=c( "ggmap", "rgdal", "raster", "akima", "sp"), 
  #         #.export=c("xmn","xmx","ymn","ymx","bus_locs","xlen")) %dopar% { 
  #         .export=c("xmn","xmx","ymn","ymx","bus_locs","xlen")) %dopar% { 
  #           xmn_curr <- xmn+(xlen*(n-1))
  #           xmx_curr <- (xmn+(xlen*n))
  #           #ymn_curr <- ymn+(ylen*(n-1))
  #          # ymx_curr <- ymn+(ylen*n)
  #           intp_coord_part <- interp(bus_locs$Longitude, bus_locs$Latitude, bus_locs$Voltage, duplicate = "mean",
  #                                     xo=seq(xmn_curr,xmx_curr, by=0.05),
  #                                     yo=seq(ymn,ymx, by=0.05))
  #                                   #  yo=seq(ymn_curr,ymx_curr, by=0.05))
  #           return(rasterToPolygons(raster(intp_coord_part)))
  #         }
  # 
  # rtp <- do.call(bind, plist)
  # stopCluster(cl)
  # 
 intp_coords <- interp(bus_locs$Longitude, bus_locs$Latitude, bus_locs$Voltage, duplicate = "mean",
                      xo=seq(xmn,xmx, by=0.05),
                      yo=seq(ymn,ymx, by=0.05))
  r <- raster(intp_coords)

  rtp <- rasterToPolygons(r)
  rtp@data$id <- 1:nrow(rtp@data)   # add id column for join
  rtpFort <- fortify(rtp, data = rtp@data)
  rtpFortMer <- merge(rtpFort, rtp@data, by.x = 'id', by.y = 'id')  # join data
  if (min(Volt[,-1])<1) {
    v_cols <-c("red","yellow","orange","blue","green")
  } else{
    v_cols <-c("green","blue","red","orange","yellow")
  }
  g <- g + 
    geom_polygon(data = rtpFortMer, 
                        aes(x = long, y = lat, group = group, fill = layer), 
                        alpha = 0.5, 
                        size = 0) +  ## size = 0 to remove the polygon outlines
    scale_fill_gradientn("Voltage",colours = v_cols,limits=c(min(Volt[,-1]),max(Volt[,-1])))+#TODO: Change limits to between 0.8-1.2
    theme(legend.position="right",legend.direction="vertical",legend.box="horizontal") +
    ggtitle(bquote(atop("Voltage at Time",atop(.(Volt[t,1]),""))))
  g
}

plot_heatmapfreq<- function(t){
  bus_locs <- update_freq(t)
  xmn <- min(bus_locs$Longitude)
  xmx <- max(bus_locs$Longitude)
  ymn <- min(bus_locs$Latitude)
  ymx <- max(bus_locs$Latitude)
  intp_coords <- interp(bus_locs$Longitude, bus_locs$Latitude, bus_locs$Frequency, duplicate = "mean",
                        xo=seq(xmn,xmx, by=0.04),
                        yo=seq(ymn,ymx, by=0.05))
  r <- raster(intp_coords)
  rtp <- rasterToPolygons(r)
  
  rtp@data$id <- 1:nrow(rtp@data)   # add id column for join
  rtpFort <- fortify(rtp, data = rtp@data)
  rtpFortMer <- merge(rtpFort, rtp@data, by.x = 'id', by.y = 'id')  # join data
  if (min(Freq[,-1])<60) {
    f_cols <-c("red","yellow","orange","blue","green")
  } else{
    f_cols <-c("green","blue","red","orange","yellow")
  }
  g <- g + geom_polygon(data = rtpFortMer, 
                        aes(x = long, y = lat, group = group, fill = layer), 
                        alpha = 0.5, 
                        size = 0) +  ## size = 0 to remove the polygon outlines
    #scale_fill_gradientn("Frequency",colours = topo.colors(255))+
    scale_fill_gradientn("Frequency",colours = f_cols,limits=c(min(Freq[,-1]),max(Freq[,-1])))+
    theme(legend.position="right",legend.direction="vertical",legend.box="horizontal") +
    ggtitle(bquote(atop("Frequency at Time",atop(.(Freq[t,1]),""))))
  g
}

plot_heatmapfreq_parallel<- function(t){
  bus_locs <- update_freq(t)
  xmn <- min(bus_locs$Longitude)
  xmx <- max(bus_locs$Longitude)
  ymn <- min(bus_locs$Latitude)
  ymx <- max(bus_locs$Latitude)
  n_cores <- detectCores()-1
  cl<-makeCluster(n_cores)
  registerDoParallel(cl)
  xlen <- (xmx-xmn)/(n_cores)
  ylen <- (ymx-ymn)/(n_cores)
  rtp <- foreach(n=1:(n_cores), .packages=c( "ggmap", "rgdal", "raster", "akima", "sp"), 
                   #.export=c("xmn","xmx","ymn","ymx","bus_locs","xlen")) %dopar% { 
                   .export=c("xmn","xmx","ymn","ymx","bus_locs","xlen"),
                   .combine=bind,.multicombine=TRUE) %dopar% { 
                     xmn_curr <- xmn+(xlen*(n-1))
                     xmx_curr <- (xmn+(xlen*n))
                     #ymn_curr <- ymn+(ylen*(n-1))
                     # ymx_curr <- ymn+(ylen*n)
                     intp_coord_part <- interp(bus_locs$Longitude, bus_locs$Latitude, bus_locs$Frequency, duplicate = "mean",
                                               xo=seq(xmn_curr,xmx_curr, by=0.035),
                                               yo=seq(ymn,ymx, by=0.05))
                     #  yo=seq(ymn_curr,ymx_curr, by=0.05))
                    # return(rasterToPolygons(raster(intp_coord_part)))
                     rasterToPolygons(raster(intp_coord_part))
                   }
  
  #rtp <- do.call(bind,plist)
  stopCluster(cl)
  #rtp <- rasterToPolygons(r)

  rtp@data$id <- 1:nrow(rtp@data)   # add id column for join
  rtpFort <- fortify(rtp, data = rtp@data)
  rtpFortMer <- merge(rtpFort, rtp@data, by.x = 'id', by.y = 'id')  # join data
  if (min(Freq[,-1])<60) {
    f_cols <-c("red","yellow","orange","blue","green")
  } else{
    f_cols <-c("green","blue","red","orange","yellow")
  }
  g <- g + geom_polygon(data = rtpFortMer, 
                        aes(x = long, y = lat, group = group, fill = layer), 
                        alpha = 0.5, 
                        size = 0) +  ## size = 0 to remove the polygon outlines
    #scale_fill_gradientn("Frequency",colours = topo.colors(255))+
    scale_fill_gradientn("Frequency",colours = f_cols,limits=c(min(Freq[,-1]),max(Freq[,-1])))+
    theme(legend.position="right",legend.direction="vertical",legend.box="horizontal") +
    ggtitle(bquote(atop("Frequency at Time",atop(.(Freq[t,1]),""))))
  g
}

plot_heatmapfreq_cluster<- function(t){
  bus_locs <- update_freq(t)
  xmn <- min(bus_locs$Longitude)
  xmx <- max(bus_locs$Longitude)
  ymn <- min(bus_locs$Latitude)
  ymx <- max(bus_locs$Latitude)
  # intp_coords <- interp(bus_locs$Longitude, bus_locs$Latitude, bus_locs$Frequency, duplicate = "mean",
  #                      xo=seq(xmn,xmx, by=0.07),
  #                     yo=seq(ymn,ymx, by=0.07))
  #  r <- raster(intp_coords)
  n_cores <- detectCores()
  cl<-makeCluster(n_cores, type="MPI")
  registerDoSNOW(cl)
  xlen <- (xmx-xmn)/(n_cores)
  ylen <- (ymx-ymn)/(n_cores)
  plist <- foreach(n=1:(n_cores), .packages=c( "ggmap", "rgdal", "raster", "akima", "sp"), 
                   .export=c("xmn","xmx","ymn","ymx","bus_locs","xlen")) %dopar% { 
                     xmn_curr <- xmn+(xlen*(n-1))
                     xmx_curr <- (xmn+(xlen*n))
                     intp_coord_part <- interp(bus_locs$Longitude, bus_locs$Latitude, bus_locs$Frequency, duplicate = "mean",
                                               xo=seq(xmn_curr,xmx_curr, by=0.035),
                                               yo=seq(ymn,ymx, by=0.05))
                     return(rasterToPolygons(raster(intp_coord_part)))
                   }
  
  rtp <- do.call(bind,plist)
  stopCluster(cl)
  
  rtp@data$id <- 1:nrow(rtp@data)   # add id column for join
  rtpFort <- fortify(rtp, data = rtp@data)
  rtpFortMer <- merge(rtpFort, rtp@data, by.x = 'id', by.y = 'id')  # join data
  if (min(Freq[,-1])<60) {
    f_cols <-c("red","yellow","orange","blue","green")
  } else{
    f_cols <-c("green","blue","red","orange","yellow")
  }
  g <- g + geom_polygon(data = rtpFortMer, 
                        aes(x = long, y = lat, group = group, fill = layer), 
                        alpha = 0.5, 
                        size = 0) +  ## size = 0 to remove the polygon outlines
    #scale_fill_gradientn("Frequency",colours = topo.colors(255))+
    scale_fill_gradientn("Frequency",colours = f_cols,limits=c(min(Freq[,-1]),max(Freq[,-1])))+
    theme(legend.position="right",legend.direction="vertical",legend.box="horizontal") +
    ggtitle(bquote(atop("Frequency at Time",atop(.(Freq[t,1]),""))))
  g
}


test_nonpar <- function(t){
  startTime <- Sys.time()
  plot_heatmapfreq_nonpar(t)
  print(paste("Non-parallel: ", toString((Sys.time()-startTime)), sep=""))
}
test_par <- function(t){
  startTime <- Sys.time()
  print(plot_heatmapfreq(t))
  print(paste("Parallel: ", toString((Sys.time()-startTime)), sep=""))
}
test_cluster <- function(t){
  startTime <- Sys.time()
  plot_heatmapfreq_cluster(t)
  print(paste("Cluster: ", toString((Sys.time()-startTime)), sep=""))
}
test_freqs <- function(){
  test_nonpar(1205)
  test_par(1205)
  test_cluster(1205)
}



#make a ggmap object, and create+save a .mp4 file with a given name for the frequency of the
# buses over the specified time period (from start-stop)
create_freqvideo <- function(start,stop,vidtitle){
  
  saveVideo({
    ani.options(interval = 0.05)
    for (t in start:stop) {
      suppressMessages(print(plot_heatmapfreq(t)))
    }
  },video.name = vidtitle)
}
#make a ggmap object, and create+save a .mp4 file with a given name for the voltage of the
# buses over the specified time period (from start-stop)
create_voltvideo <- function(start,stop,vidtitle){
  g <- ggmap(mapten)+scale_x_continuous(limits = c(-90.6, -81), expand = c(0, 0)) +scale_y_continuous(limits = c(34.5, 37), expand = c(0, 0))
  saveVideo({
    ani.options(interval = 0.05)
    for (t in start:stop) {
      suppressMessages(print(plot_heatmapvolt(t)))
    }
  },video.name = vidtitle)
}



#registerDoParallel(cores = 8)
#tenn <- map_data('USA') %>%data.table()
#xo=seq(xmn,xmx, by=0.05)
#yo=seq(ymn,ymx, by=0.05)
#a <- matrix(xo,ncol = 1)
#b <- matrix(yo,ncol = 1)
#d <- matrix(c(xo,yo),ncol = 2)
#ab <- cbindX(a,b)
#update_volt(1205)
#pts <- subset(bus_locs,select=c("Longitude", "Latitude", "Voltage"))
#colnames(pts) <- c('x','y','z') 
#pppts <- ppp(x = pts$x,y=pts$y,marks = pts$z,window = owin(xrange = c(xmn,xmx),yrange = c(ymn,ymx)))
#smp <- Smooth(pppts)
#r <- raster(smp)
#rr <- as.data.frame(values(r))
#writeRaster(r,filename = "tenn_raster.grd",format="raster")
#system("/usr/bin/gdal_trace_outline tenn_raster.tif -classify -out-cs ll -ogr-out outline_tif.shp",intern = TRUE)
#system("/usr/bin/gdal_trace_outline tenn_raster.tif -classify -out-cs en -ogr-out outline_tif.shp",intern = TRUE)
#polyr <- readShapeP('outline_tif.shp', proj4string=CRS("+proj=longlat"))
#polyr <- readOGR(dsn = "outline_tif.shp",layer = "file")
#rtp <- readGDAL("tenn_raster.tif")
#rtp_data <- sgdf_transform(rtp)

#dfr <- as.data.frame(rtp)
#system("/usr/bin/ogr2ogr outline_tif -nlt MULTILINESTRING")
#cpts <- coordinates(pts)
#image(smp)
#val <- getValues(r)
#xy <- as.data.frame(xyFromCell(r,1:ncell(r)))
#xy <- cbind(xy,val)
#ggplot(dfr, aes(x=x, y=y, fill=band1)) + geom_raster() + coord_equal()
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




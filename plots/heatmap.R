library(ggplot2)
library(ggmap)
library(rgdal)
library(raster)
library(rgeos)
library(akima)
library(sp)
library(doSNOW)
library(animation)
library(geosphere)
#library(gdata)
#library(spatstat)
#library(data.table)
#library(parallel)
library(doParallel)
fnames <- function(){
  n <- list(Heatmap="heatmap",
            Voltage="plot_heatmapvolt",
            Frequency="plot_heatmapfreq",
            "Voltage alarms"="plot_heatmapvolt_alarms",
            "Frequency alarms"="plot_heatmapfreq_alarms")
  if (exists("Pangle")) {
    n <- c(n,Angle="plot_heatmapangle","Angle alarms"="plot_heatmapangle_alarms")
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

#Update voltage covariance matrix, then goes through the lines matrix and returns linesb with the Correlation
# column updated with the current values
get_busline_voltcov <- function(time){
  update_covbus_volt(time)
  for (x in 1:nrow(linesb)) {
    curr_row <- linesb[x,]
    curr_row$Correlation <- as.numeric(as.character(Sv[[curr_row$From.Bus.Name,curr_row$To.Bus.Name]]))
    linesb[x,"Correlation"] <- curr_row$Correlation
  }
  linesb
}
#Update frequency covariance matrix, then goes through the lines matrix and returns linesb with the Correlation
# column updated with the current values
get_busline_freqcov <- function(time){
  update_covbus_freq(time)
  for (x in 1:nrow(linesb)) {
    curr_row <- linesb[x,]
    curr_row$Correlation <- as.numeric(as.character(Sf[[curr_row$From.Bus.Name,curr_row$To.Bus.Name]]))
    linesb[x,"Correlation"] <- curr_row$Correlation
  }
  linesb
}



#Change the frequency column of bus_locs with the frequencies for a given time then returns the new matrix
update_freq <- function(time){
  tf <- t(Freq[time,-1])
  tf <- cbind(rownames(tf),tf)
  colnames(tf) <- c("Bus.Name","Frequency")
 # bus_locs <- merge(subset(bus_locs,select = c("Bus.Num","Bus.Name","Sub.Name","Latitude","Longitude","Voltage")),tf, by="Bus.Name")
  bus_locs <- merge(bus_locs[ , ! colnames(bus_locs) %in% c("Frequency")],tf, by="Bus.Name")
  bus_locs$Frequency <- as.numeric(as.character(bus_locs$Frequency))
  #assign("bus_locs",bus_locs,envir = .GlobalEnv)
  bus_locs
}
#Change the voltage column of bus_locs with the voltages for a given time then returns the new matrix
update_volt <- function(time){
  tv <- t(Volt[time,-1])
  tv <- cbind(rownames(tv),tv)
  colnames(tv) <- c("Bus.Name","Voltage")
  #bus_locs <- merge(subset(bus_locs,select = c("Bus.Num","Bus.Name","Sub.Name","Latitude","Longitude","Frequency")),vf, by="Bus.Name")
  bus_locs <- merge(bus_locs[ , ! colnames(bus_locs) %in% c("Voltage")],tv, by="Bus.Name")
  bus_locs$Voltage <- as.numeric(as.character(bus_locs$Voltage))
  #assign("bus_locs",bus_locs,envir = .GlobalEnv)
  bus_locs
} 
#Change the angle column of bus_locs with the angles for a given time then returns the new matrix
update_pangle <- function(time){
  ta <- t(Pangle[time,-1])
  ta <- cbind(rownames(ta),ta)
  colnames(ta) <- c("Bus.Name","Angle")
  #bus_locs <- merge(subset(bus_locs,select = c("Bus.Num","Bus.Name","Sub.Name","Latitude","Longitude","Voltage","Frequency")),ta, by="Bus.Name")
  bus_locs <- merge(bus_locs[ , ! colnames(bus_locs) %in% c("Angle")],ta, by="Bus.Name")
  bus_locs$Angle <- as.numeric(as.character(bus_locs$Angle))
  #assign("bus_locs",bus_locs,envir = .GlobalEnv)
  bus_locs
}


#Turn autoscaling on or off
autoscale <- function(){
  if(!exists("autosc")){
    autosc <<- FALSE
  }
  if(autosc == TRUE){
    autosc <<-FALSE
  } else{
    autosc <<-TRUE
  }
}

#Unused
get_corr_neighbors_volt <- function(time){
  linesb <-get_busline_voltcov(time)
  for (x in 1:nrow(bus_locs)) {
    curr_row <- bus_locs[x,]
    
  }
}
#Unused
init_volt_heatgrid <- function(){
  bus_locs <- update_volt(1)
  b <- subset(bus_locs,select=c("Latitude","Longitude","Voltage"))
  xmn <- min(bus_locs$Longitude)
  xmx <- max(bus_locs$Longitude)
  ymn <- min(bus_locs$Latitude)
  ymx <- max(bus_locs$Latitude)
  
  longs <- seq(from=xmn,to=xmx,by=0.1)
  lats <- seq(from=ymn,to=ymx,by=0.1)
  #points_df <- data.frame("Longitude","Latitude")
  points_mat <- matrix(data=0,nrow = (length(lats)*length(longs)),ncol = 4)
  colnames(points_mat) <- c("Longitude","Latitude","vvNom","vvDen")
  for (x in 1:length(longs)) {
    for (y in 1:length(lats)) {
      points_mat[(((x-1)*length(lats))+y),1] <- longs[x]
      points_mat[(((x-1)*length(lats))+y),2] <- lats[y]
      points_mat[(((x-1)*length(lats))+y),3] <- 0
      points_mat[(((x-1)*length(lats))+y),4] <- 0
    }
  }
  pm <<- merge(as.data.frame(points_mat),b,by=c("Longitude","Latitude"),all=TRUE)
  pm$pmu <<- ifelse(is.na(pm$Voltage),0,1)
}
#Unused
get_surrounding_points <- function(nbus){
  grid_locs <- matrix(0,nrow = 2592, ncol=2)
  vj <- bus_locs[nbus,"Voltage"]
  x1 <- bus_locs[nbus,"Longitude"]
  y1 <- bus_locs[nbus,"Latitude"]
  dinf <- 4
  curr_point <- 0
  for (x in seq(from = (-dinf+x1),to = (dinf+x1),by=0.1)) {
    for (y in seq(from = (Arg(-sqrt(as.complex((dinf^2)-(x^2))))+y1),to = (Arg((sqrt(as.complex((dinf^2)-(x^2)))))+y1),by=0.1)) {
      grid_locs[curr_point,] <- c(round(x,digits = 2),round(y,digits = 2))
      curr_point <- curr_point+1
    }
  }
  grid_locs
}
#Unused
init_grid_points <- function(){
  allgp_mat <- matrix(ncol = 2,nrow = 0)
  colnames(all_grid_points) <- c("Bus.Name","Surrounding.Points")
  for (n in 1:nrow(bus_locs)) {
    allgp_mat <- rbind(allgp_mat,get_surrounding_points(n))
  }
  allgp_mat <- unique(allgp_mat)
  colnames(allgp_mat) <- c("Longitude","Latitude")
  allgp <- data.frame(allgp_mat)
  allgp <- allgp[!(allgp$Longitude==0 & allgp$Latitude==0),]
  allgp$Sum.Distance <- as.numeric(0)
  allgp <- merge(allgp,bus_locs,by=c("Latitude","Longitude"),all=TRUE)
  allgp$Sum.Distance <- apply(allgp,1,function(x) {ifelse(is.na(x["Sum.Distance"]),as.numeric(0),as.numeric(x["Sum.Distance"]))})
  allgp$Frequency <- apply(allgp,1,function(x) {ifelse(is.na(x["Frequency"]),as.numeric(0),as.numeric(x["Frequency"]))})
  allgp$Angle <- apply(allgp,1,function(x) {ifelse(is.na(x["Angle"]),as.numeric(0),as.numeric(x["Angle"]))})
  allgp$Voltage <- apply(allgp,1,function(x) {ifelse(is.na(x["Voltage"]),as.numeric(0),as.numeric(x["Voltage"]))})
  allgp
}
#Unused
make_sppolys_volt <- function(t){
  library(gputools)
  bus_locs <- update_volt(t)
  
  #bus_locs[with(bus_locs, (Latitude < 50 & Latitude > 45) & (Voltage > 1.05 & Voltage < 0.95) &(Longitude < 80 & Longitude > 75)),]
  
  
  dist_bl <- gpuDist(points = bus_locs[,c("Latitude","Longitude")],method = "euclidean")
  cd_bl <- gpuDistClust(bus_locs[,c("Latitude","Longitude")])
  xmn <- min(bus_locs$Longitude)
  xmx <- max(bus_locs$Longitude)
  ymn <- min(bus_locs$Latitude)
  ymx <- max(bus_locs$Latitude)
  intp_coords <- interp(bus_locs$Longitude, bus_locs$Latitude, bus_locs$Voltage, duplicate = "mean",
                        xo=seq(xmn,xmx, by=0.04),
                        yo=seq(ymn,ymx, by=0.05))
  ic_df <- interp2xyz(intp_coords,data.frame = TRUE)
  ggplot(ic_df)+aes(x=x,y=y,z=z,fill=z)+geom_tile()+coord_equal()
  r <- raster(intp_coords)
  
  rtp <- rasterToPolygons(r)
}
#Update the matrix of values/distances for the contour mapping algorithm for the given point (x,y)
#Unused
update_virtualvalue <- function(x,y,x1,y1,vj){

 # td <- pointDistance(c(x1,y1),c(x,y),lonlat = FALSE) #distance between (tx,ty) and (x1,y1)
  td <- sqrt(((x-x1)^2) + ((y-y1)^2))
  curr_index <- rtpFort$id[which((rtpFort$lat<(y+0.03))&(rtpFort$lat>(y-0.03))&(rtpFort$long<(x+0.03))&(rtpFort$long>(x-0.03)))]
#  curr_index <-  which(((rtpFort$lat<(y+0.03))&(rtpFort$lat>(y-0.03))&(rtpFort$long<(x+0.03))&(rtpFort$long>(x-0.03))),arr.ind = TRUE)
  # print(length(curr_index))
  if (length(curr_index) > 0) {
    #print(curr_index)
    for (n in 1:length(curr_index)) {
      curr_id <- as.numeric(curr_index[n])
      curr_row <- new_voltvals[curr_id,]
      c_valsum <- curr_row$valsum
      c_distsum <- curr_row$distsum
      curr_row$valsum <- c_valsum+ (as.numeric(vj)*(1/((td)^2)))
      curr_row$distsum <- c_distsum+ (1/((td)^2))
      new_voltvals[curr_id,] <- curr_row
      assign("new_voltvals",new_voltvals,envir = .GlobalEnv)
     # c_valsum <-as.numeric(new_voltvals[curr_id,][["valsum"]])
      #c_diststum <- as.numeric(new_voltvals[curr_id,][["distsum"]])
      #new_voltvals[curr_id,][["valsum"]] <<- c_valsum+ as.numeric((vj*(1/((td)^2))))
      #new_voltvals[curr_id,][["distsum"]] <<- c_diststum +as.numeric((1/((td)^2)))
    }
  }
}
  # curr_index <- rtpFortMer_test[((round(rtpFortMer_test$long,digits=2) == tx)&(round(rtpFortMer_test$lat,digits=2)==ty)),]
 # ci <- which((((round(rtpFortMer_test$long,digits=2)==tx)&(round(rtpFortMer_test$lat,digits=2)==ty))))
  # curr_yind <- match(y,intp_coords[["y"]])
 # curr_xind <- match(x,intp_coords[["x"]])
  #c_valsum <- zc_vals[curr_xind,curr_yind] #as.numeric(curr_vals[[1]][2]) #Value sum
#  c_distsum <- zc_dists[curr_xind,curr_yind] #as.numeric(curr_vals[[1]][1]) #Distance sum value
  #Update value sum
#  c_valsum <- c_valsum+(vj*(1/((td)^2)))
 # c_distsum <- c_distsum+(1/((td)^2))
 # new_vals <- paste(c_distsum,c_valsum,sep = ",")
 # coords_z[curr_xind,curr_yind] <<- new_vals
 # zc_vals[curr_xind,curr_yind] <<- I(list(c_valsum,c(td,vj)))#c_valsum+ vj#(vj*(1/((td)^2)))
 # zc_dists[curr_xind,curr_yind] <<- #c_distsum+ td#(1/((td)^2))
 # zc_dists <<- zc_dists
  #zc_vals <<- zc_vals


#Go through every point within a circle for a given bus (with long/lat location) and updates the values there
# nval = name of value to be updating the matrix with ("Voltage","Frequency","Angle")
#Unused
update_neighbor_points <- function(nbus,nval){
  vj <- bus_locs[nbus,nval]
  x1 <- bus_locs[nbus,"Longitude"]
  y1 <- bus_locs[nbus,"Latitude"]
 # ty <- round(y,digits = 2) #Latitude
  #tx <- round(x,digits = 2) #Longitude
  dinf <- 2
  for (x in seq(from = (-dinf+x1),to = (dinf+x1),by=0.01)) {
    tx <- round(x,digits = 2) #Longitude
    if(tx %in% round(rtpFortMer_test$long,digits=2)){
     # print(paste("x:",tx,sep = ""))
      for (y in seq(from = (Arg(-sqrt(as.complex((dinf^2)-(x^2))))+y1),to = (Arg((sqrt(as.complex((dinf^2)-(x^2)))))+y1),by=0.01)) {
        ty <- round(y,digits = 2) #Latitude
       # if ((tx %in% intp_coords[["x"]])&(ty %in% intp_coords[["y"]])) {
        if(ty %in% round(rtpFortMer_test$lat,digits=2)) {
          update_virtualvalue(tx,ty,x1,y1,vj)
         # print(paste("y:",ty,sep = ""))
        }
      }
    }
  }
}
#new_voltvals$new.layer <- ifelse(new_voltvals$distsum!=0,(new_voltvals$valsum/new_voltvals$distsum),0)
  
#  ylist <- xlist <- list()
  # for (x in seq(from = (-dinf+x1),to = (dinf+x1),by=0.1)) {
  #   tx <- round(x,digits = 2) #Longitude
  #     #print(paste("x:",tx,sep = ""))
  #   for (y in seq(from = (Arg(-sqrt(as.complex((dinf^2)-(x^2))))+y1),to = (Arg((sqrt(as.complex((dinf^2)-(x^2)))))+y1),by=0.1)) {
  #     ty <- round(y,digits = 2) #Latitude
  #     
  #     # if ((tx %in% intp_coords[["x"]])&(ty %in% intp_coords[["y"]])) {
  #     #if(ty %in% ulats) {
  #       # update_virtualvalue(tx,ty,x1,y1,vj)
  #       #  print(paste("y:",ty,sep = ""))
  #    # }
  #     
  #   }
  # }

#updated_voltvals <- new_voltvals[(new_voltvals$distsum != 0),]
#updated_voltvals$new.layer <- updated_voltvals$valsum/updated_voltvals$distsum
#Unused
get_containing_poly <- function(x){ 
  p <- rtp@polygons
#  pts <- lapply(p, function(x) )
  pts <- p[with(p,function(i) ((x %in% i@Polygons[[1]]@coords) & (y %in% i@Polygons[[1]]@coords)))]
  cp <-  which.min(abs())
  I(rtp_test@polygons[[x]]@Polygons[[1]]@coords)
}
#Initialize grid using the nval type specified; Unused
initialize_grid <- function(nval){
  bus_locs <- update_volt(1)
  bus_locs <- update_freq(1)
  bus_locs <- update_pangle(1)
  xmn <- min(bus_locs$Longitude)
  xmx <- max(bus_locs$Longitude)
  ymn <- min(bus_locs$Latitude)
  ymx <- max(bus_locs$Latitude)
  if (nval == "Angle") {
    intp_coords <<- interp(bus_locs$Longitude, bus_locs$Latitude, bus_locs$Angle, duplicate = "mean",
                           xo=seq(xmn,xmx, by=0.05),
                           yo=seq(ymn,ymx, by=0.05))
  } else if (nval == "Frequency") {
    intp_coords <<- interp(bus_locs$Longitude, bus_locs$Latitude, bus_locs$Frequency, duplicate = "mean",
                           xo=seq(xmn,xmx, by=0.05),
                           yo=seq(ymn,ymx, by=0.05))
  } else {
    intp_coords <<- interp(bus_locs$Longitude, bus_locs$Latitude, bus_locs$Voltage, duplicate = "mean",
                           xo=seq(xmn,xmx, by=0.05),
                           yo=seq(ymn,ymx, by=0.05))
  }
  r <- raster(intp_coords)
  
  rtp <- rasterToPolygons(r)
  rtp@data$id <- 1:nrow(rtp@data)   # add id column for join
  rtpFort <- fortify(rtp, data = rtp@data)

  #Add the layer column of the rtp@data dataframe to the rtpFort dataframe
  rtpFortMer <- merge(rtpFort, rtp@data, by.x = 'id', by.y = 'id')  # join data
  
  new_voltvals <- rtp@data
  new_voltvals$valsum <- 0 
  new_voltvals$distsum <- 0 
  
  
  rtp_test <- rtp
  rtpFort_test <- rtpFort
  rtpFortMer_test <- rtpFortMer
  #rtp_test@data$locpolys <- I(rtp_test@polygons[[x]]@Polygons[[1]]@coords@Polygons[[1]]@coords)
  #I(rtp_test@polygons[[x]]@Polygons[[1]]@coords@Polygons[[1]]@coords)
 # get_full_coords = function(x) {round(as.vector(seq(min(rtp_test@polygons[[x]]@Polygons[[1]]@coords))),digits = 2)}
  get_full_coords = function(x) {
    c(as.vector(seq(round(min(rtp_test@polygons[[x]]@Polygons[[1]]@coords[,1]),digits = 2),round(max(rtp_test@polygons[[1]]@Polygons[[1]]@coords[,1]),digits = 2),by = 0.01)),
    as.vector(seq(round(min(rtp_test@polygons[[x]]@Polygons[[1]]@coords[,2]),digits = 2),round(max(rtp_test@polygons[[1]]@Polygons[[1]]@coords[,2]),digits = 2),by = 0.01)))
  }
  
  get_long_coords = function(x) {
    as.vector(seq(round(min(rtp_test@polygons[[x]]@Polygons[[1]]@coords[,1]),digits = 2),round(max(rtp_test@polygons[[x]]@Polygons[[1]]@coords[,1]),digits = 2),by = 0.01))
    #  as.vector(seq(round(min(rtp_test@polygons[[1]]@Polygons[[1]]@coords[,2]),digits = 2),round(max(rtp_test@polygons[[1]]@Polygons[[1]]@coords[,2]),digits = 2),by = 0.01)))
  }
  get_lat_coords = function(x) {
    as.vector(seq(round(min(rtp_test@polygons[[x]]@Polygons[[1]]@coords[,2]),digits = 2),round(max(rtp_test@polygons[[x]]@Polygons[[1]]@coords[,2]),digits = 2),by = 0.01))
    #  as.vector(seq(round(min(rtp_test@polygons[[1]]@Polygons[[1]]@coords[,2]),digits = 2),round(max(rtp_test@polygons[[1]]@Polygons[[1]]@coords[,2]),digits = 2),by = 0.01)))
  }
  
  rtp_test@data$locpolys <- lapply(rtp_test@data$id, function(x)get_full_coords(x))
  names(rtp_test@data$locpolys) <- 1:nrow(rtp@data)
  locs_mat <- matrix(0,nrow = )
  all_locs_lst <- as.vector(unlist(rtp_test@data$locpolys,use.names = TRUE))
  longs_list <- as.vector(unlist(lapply(rtp_test@data$id, function(x)get_long_coords(x))))
  lats_list <- as.vector(unlist(lapply(rtp_test@data$id, function(x)get_lat_coords(x))))
  ulats <- unique(lats_list)
  ulongs <- unique(longs_list)
  
  m <- t(as.matrix(c(tx,ty),nrow=1,ncol=2))
 # names(all_locs_lst) <- lapply(all_locs_lst,function(x) match())
  
  #Matrix that will hold the numerator for the virtual value
 # zc_vals <- matrix(intp_coords[["z"]],nrow = nrow(intp_coords[["z"]]),ncol = ncol(intp_coords[["z"]]))
 # zc_vals <<- structure(vapply(zc_vals, function(x) ifelse(is.na(x),0,x), numeric(1)), dim=dim(zc_vals))
  #Matrix that will hold the denominator for the virtual value
 # zc_dists <<- matrix(0,nrow = nrow(intp_coords[["z"]]),ncol = ncol(intp_coords[["z"]]))
}
#Unused
update_grid_volt <- function(time){
  bus_locs <- update_volt(time)
  for (n in 1:nrow(bus_locs)) {
   update_neighbor_points(n,"Voltage")
    }

  # n_cores <- detectCores()-1
  # cl<-makeCluster(n_cores)
  # registerDoParallel(cl)
  # foreach(n=1:(nrow(bus_locs)), .packages=c( "sp","raster"), 
  #         .export=c("intp_coords","zc_dists","zc_vals","bus_locs","update_neighbor_points","update_virtualvalue")) %dopar% { 
  #   update_neighbor_points(n)
  # } 
  for (x in 1:nrow(zc_vals)) {
    for (y in 1:ncol(zc_vals)) {
      if(zc_dists[x,y]!=0){
        intp_coords[["z"]][x,y] <- (zc_vals[x,y]/zc_dists[x,y])
      } else{
        intp_coords[["z"]][x,y] <- 0
      }
    }
  }
#  stopCluster(cl)
  assign("intp_coords",intp_coords,envir = .GlobalEnv)
}


alarm_time <- (60*10) #sample/sec * seconds to check
upper_vlimit <- 1.05
lower_vlimit <- 0.95
update_alarmstatus_volt <- function(t,b){
  state <- 1 #Meaning there are no values in the past <alarm_time> steps that are outside of the upper/lower limits

  start <- ifelse((t>alarm_time), (t-alarm_time), 1)
  v <- Volt[start:t, b["Bus.Name"]]
  if(length(v[ ((v>upper_vlimit) | (v<lower_vlimit)) ]) > 0) {
    state <- 2 #If there are values outside the limits
  }
  state
}
upper_flimit <- 60.05
lower_flimit <- 59.95
update_alarmstatus_freq <- function(t,b){
  state <- 1 #Meaning there are no values in the past <alarm_time> steps that are outside of the upper/lower limits
  
  start <- ifelse((t>alarm_time), (t-alarm_time), 1)
  f <- Freq[start:t, b["Bus.Name"]]
  if(length(f[ ((f>upper_flimit) | (f<lower_flimit)) ]) > 0) {
    state <- 2 #If there are values outside the limits
  }
  state
}
upper_alimit <- 20
lower_alimit <- -20
update_alarmstatus_angle <- function(t,b){
  state <- 1 #Meaning there are no values in the past <alarm_time> steps that are outside of the upper/lower limits
  start <- ifelse((t>alarm_time), (t-alarm_time), 1)
  a <- Pangle[start:t, b["Bus.Name"]]
  if(length(a[ ((a>upper_alimit) | (a<lower_alimit)) ]) > 0) {
    state <- 2 #If there are values outside the limits
  }
  state
}

zoom_map <- function(point){
  if(!exists("is_zoom")){
    is_zoom <<- FALSE
  }
  if((is_zoom)){
    is_zoom <<- FALSE
    g <<- ggmap(mapten) +
      scale_x_continuous(limits=c(map_lims[1], map_lims[2]), expand=c(0,0)) + 
      scale_y_continuous(limits=c(map_lims[3], map_lims[4]), expand=c(0,0))
  } else{
    is_zoom <<- TRUE
    xmin <- min(bus_locs$Longitude)
    xmax <- max(bus_locs$Longitude)
    ymin <- min(bus_locs$Latitude)
    ymax <- max(bus_locs$Latitude)
    
    ratio <- abs(xmax-xmin)/abs(ymax-ymin)
    
    xrange <- abs(xmax-xmin)/4
    yrange <- abs(ymax-ymin)/4 #TODO: Zoom into the nearest cluster instead
    
    z_xmin <<- point[1]-xrange
    z_xmax <<- point[1]+xrange
    z_ymin <<- point[2]-yrange
    z_ymax <<- point[2]+yrange
    
    g <<- ggmap(mapten) +
      scale_x_continuous(limits=c(z_xmin, z_xmax), expand=c(0,0)) + 
      scale_y_continuous(limits=c(z_ymin, z_ymax), expand=c(0,0))
  }
}

plot_heatmapvolt_alarms<- function(t){
  if(!exists("autosc")){
    autosc <<- FALSE
  }
  if(!exists("is_zoom")){
    is_zoom <<- FALSE
  }
  bus_locs <- update_volt(t)
  if (is_zoom) {
    xmn <- min(bus_locs[(bus_locs$Longitude>=z_xmin),"Longitude"])
    xmx <- max(bus_locs[(bus_locs$Longitude<=z_xmin),"Longitude"])
    ymn <- min(bus_locs[(bus_locs$Latitude>z_ymin),"Latitude"])
    ymx <- max(bus_locs[(bus_locs$Latitude>z_ymin),"Latitude"])
  } else{
    xmn <- min(bus_locs$Longitude)
    xmx <- max(bus_locs$Longitude)
    ymn <- min(bus_locs$Latitude)
    ymx <- max(bus_locs$Latitude)
  }
  if(autosc == TRUE){
     vmin <- min(bus_locs$Voltage)
     vmax <- max(bus_locs$Voltage)
    #vmin <- ifelse(min(bus_locs$Voltage)<0.8,min(bus_locs$Voltage),0.8)
   # vmax <- ifelse(max(bus_locs$Voltage)>1.2,max(bus_locs$Voltage),1.2)
  } else{
    vmin <- 0.8
    vmax <- 1.2
  }
  xstep <- (xmx-xmn)/80
  ystep <- (ymx-ymn)/80
  intp_coords <- interp(bus_locs$Longitude, bus_locs$Latitude, bus_locs$Voltage, duplicate = "mean",
                      xo=seq(xmn,xmx, by=xstep),
                      yo=seq(ymn,ymx, by=ystep))
  r <- raster(intp_coords)

  rtp <- rasterToPolygons(r)
  rtp@data$id <- 1:nrow(rtp@data)   # add id column for join
  rtpFort <- fortify(rtp, data = rtp@data)
  rtpFortMer <- merge(rtpFort, rtp@data, by.x = 'id', by.y = 'id')  # join data
  
  if (vmin<0.8 & vmax <= 1.2) {
    v_cols <-c("red","yellow","orange","blue","green")
  } else if(vmax <1.2 & vmin >= 0.8){
    v_cols <-c("green","blue","orange","yellow","red")
  } else{
    v_cols <-c("red","yellow","green","blue","black")
  }
  
  bus_locs$alarm <-apply(bus_locs,1, function(x) update_alarmstatus_volt(t, x))
  bv_high <- subset(bus_locs, (alarm==2 & Voltage > upper_vlimit))
  if(nrow(bv_high) > 0L){bv_high$color <- 2} #above limit = red
  
  bv_low <- subset(bus_locs, (alarm==2 & Voltage < lower_vlimit))
  if(nrow(bv_low) > 0L){bv_low$color <- 4} #below limit = blue
  
  bv_normal <- subset(bus_locs, (alarm==1))
  if(nrow(bv_normal) > 0L){bv_normal$color <- 1} #within limits = green
  
  bv_past <- subset(bus_locs, (alarm==2 & (Voltage >= lower_vlimit & Voltage <= upper_vlimit)))
  if(nrow(bv_past) > 0L){bv_past$color <- 3} #currently within limits, but above/below in past=yellow
  alarm_labs <- NULL
  alarm_vals <- NULL
  g <- g + 
  geom_polygon(data = rtpFortMer,
                       aes(x = long, y = lat, group = group, fill = layer),
                       alpha = 0.5,
                       size = 0) +  ## size = 0 to remove the polygon outlines
    scale_fill_gradientn("Voltage",colours = v_cols,limits=c(vmin,vmax))
  if(nrow(bv_normal) > 0L){
    alarm_labs <- c(alarm_labs,paste(lower_vlimit," < Voltage < ",upper_vlimit,sep=""))
    alarm_vals <- c(alarm_vals,"1"="green")
    g <- g+geom_point(data = bv_normal, aes(x=Longitude, y=Latitude, colour=factor(color)),alpha=1, size = 5, shape = 16)#, show.legend=FALSE)
  }
  if(nrow(bv_high) > 0L){
    alarm_labs <- c(alarm_labs,paste("Voltage > ",upper_vlimit,sep=""))
    alarm_vals <- c(alarm_vals,"2"="red")
    g <- g+geom_point(data = bv_high, aes(x=Longitude, y=Latitude, colour=factor(color)),alpha=1, size = 5, shape = 16)#, show.legend=FALSE)
  }
  if(nrow(bv_past) > 0L){
    alarm_labs <- c(alarm_labs,"Voltage Previously outside of Limits")
    alarm_vals <- c(alarm_vals,"3"="yellow")
    g <- g+geom_point(data = bv_past, aes(x=Longitude, y=Latitude, colour=factor(color)),alpha=1, size = 5, shape = 16)#, show.legend=FALSE)
  }
  if(nrow(bv_low) > 0L){
    alarm_labs <- c(alarm_labs,paste("Voltage < ",lower_vlimit,sep=""))
    alarm_vals <- c(alarm_vals,"4"="blue")
    g <- g+geom_point(data = bv_low, aes(x=Longitude, y=Latitude, colour=factor(color)),alpha=1, size = 5, shape = 16)#, show.legend=FALSE)
  }
  g <- g+scale_colour_manual("Alarm Status", values = alarm_vals,labels=alarm_labs) +
    #geom_point(data=bus_locs,aes(x=Longitude,y=Latitude, colour=alarm,group=Sub.Name),size=5,alpha=0.7,shape=16) +
  #  scale_colour_gradientn("Alarm Status",colours = c("green","blue","orange","yellow"),limits=c(vmin,vmax)) +
    theme(legend.position="right",legend.direction="vertical",legend.box="horizontal") +
    ggtitle(bquote(atop("Voltage at Time",atop(.(Volt[t,1]),""))))
  g
}

plot_heatmapvolt<- function(t){
  if(!exists("autosc")){
    autosc <<- FALSE
  }
  if(!exists("is_zoom")){
    is_zoom <<- FALSE
    z_xmin <<- 0
    z_xmax <<- 0
    z_ymin <<- 0
    z_ymax <<- 0
  }
  bus_locs <- update_volt(t)
  if (is_zoom) {
    xmn <- min(bus_locs[(bus_locs$Longitude>=z_xmin),"Longitude"])
    xmx <- max(bus_locs[(bus_locs$Longitude<=z_xmin),"Longitude"])
    ymn <- min(bus_locs[(bus_locs$Latitude>z_ymin),"Latitude"])
    ymx <- max(bus_locs[(bus_locs$Latitude>z_ymin),"Latitude"])
  } else{
    xmn <- min(bus_locs$Longitude)
    xmx <- max(bus_locs$Longitude)
    ymn <- min(bus_locs$Latitude)
    ymx <- max(bus_locs$Latitude)
  }

  if(autosc == TRUE){
    vmin <- min(bus_locs$Voltage)
    vmax <- max(bus_locs$Voltage)
    #vmin <- ifelse(min(bus_locs$Voltage)<0.8,min(bus_locs$Voltage),0.8)
    # vmax <- ifelse(max(bus_locs$Voltage)>1.2,max(bus_locs$Voltage),1.2)
  } else{
    vmin <- 0.8
    vmax <- 1.2
  }
  xstep <- (xmx-xmn)/80
  ystep <- (ymx-ymn)/80
  intp_coords <- interp(bus_locs$Longitude, bus_locs$Latitude, bus_locs$Voltage, duplicate = "mean",
                        xo=seq(xmn,xmx, by=xstep),
                        yo=seq(ymn,ymx, by=ystep))
  r <- raster(intp_coords)
  
  rtp <- rasterToPolygons(r)
  rtp@data$id <- 1:nrow(rtp@data)   # add id column for join
  rtpFort <- fortify(rtp, data = rtp@data)
  rtpFortMer <- merge(rtpFort, rtp@data, by.x = 'id', by.y = 'id')  # join data
  
  if (vmin<0.8 & vmax <= 1.2) {
    v_cols <-c("red","yellow","orange","blue","green")
  } else if(vmax <1.2 & vmin >= 0.8){
    v_cols <-c("green","blue","orange","yellow","red")
  } else{
    v_cols <-c("red","yellow","green","blue","black")
  }
  
  g <- g + 
    geom_polygon(data = rtpFortMer,
                 aes(x = long, y = lat, group = group, fill = layer),
                 alpha = 0.5,
                 size = 0) +  ## size = 0 to remove the polygon outlines
    #geom_point(data=bus_locs,aes(x=Longitude,y=Latitude, colour=alarm,group=Sub.Name),size=5,alpha=0.7,shape=16) +
    #  scale_colour_gradientn("Alarm Status",colours = c("green","blue","orange","yellow"),limits=c(vmin,vmax)) +
    scale_fill_gradientn("Voltage",colours = v_cols,limits=c(vmin,vmax))+
    theme(legend.position="right",legend.direction="vertical",legend.box="horizontal") +
    ggtitle(bquote(atop("Voltage at Time",atop(.(Volt[t,1]),""))))
  g
}

plot_heatmapangle_alarms<- function(t){
  if(!exists("autosc")){
    autosc <<- FALSE
  }
  if(!exists("is_zoom")){
    is_zoom <<- FALSE
  }
  bus_locs <- update_pangle(t)
  if (is_zoom) {
    xmn <- min(bus_locs[(bus_locs$Longitude>=z_xmin),"Longitude"])
    xmx <- max(bus_locs[(bus_locs$Longitude<=z_xmin),"Longitude"])
    ymn <- min(bus_locs[(bus_locs$Latitude>z_ymin),"Latitude"])
    ymx <- max(bus_locs[(bus_locs$Latitude>z_ymin),"Latitude"])
  } else{
    xmn <- min(bus_locs$Longitude)
    xmx <- max(bus_locs$Longitude)
    ymn <- min(bus_locs$Latitude)
    ymx <- max(bus_locs$Latitude)
  }
  xstep <- (xmx-xmn)/80
  ystep <- (ymx-ymn)/80
  
  intp_coords <- interp(bus_locs$Longitude, bus_locs$Latitude, bus_locs$Angle, duplicate = "mean",
                        xo=seq(xmn,xmx, by=xstep),
                        yo=seq(ymn,ymx, by=ystep))
  r <- raster(intp_coords)
  rtp <- rasterToPolygons(r)
  
  rtp@data$id <- 1:nrow(rtp@data)   # add id column for join
  rtpFort <- fortify(rtp, data = rtp@data)
  rtpFortMer <- merge(rtpFort, rtp@data, by.x = 'id', by.y = 'id')  # join data
  
  if(autosc == TRUE){
    # amin <- min(b$Angle)
    # amax <- max(b$Angle)
    amin <- ifelse(min(bus_locs$Angle)<(-40),min(bus_locs$Angle),-40)
    amax <- ifelse(max(bus_locs$Angle)>40,max(bus_locs$Angle),40)
    #  adiff <- (amax-amin)
    #  a_lab <- c(amin,(amin+(adiff/4)),(amin+(adiff/2)),(amax-(adiff/4)),amax)
  } else{
    amin <- -40
    amax <- 40
    # a_lab <- c(-40,-20,0,20,40)
  }
  
  bus_locs$alarm <-apply(bus_locs,1, function(x) update_alarmstatus_angle(t, x))
  ba_high <- subset(bus_locs, (alarm==2 & Angle > upper_alimit))
  if(nrow(ba_high) > 0L){ba_high$color <- 2} #above limit = red
  
  ba_low <- subset(bus_locs, (alarm==2 & Angle < lower_alimit))
  if(nrow(ba_low) > 0L){ba_low$color <- 4} #below limit = blue
  
  ba_normal <- subset(bus_locs, (alarm==1))
  if(nrow(ba_normal) > 0L){ba_normal$color <- 1} #within limits = green
  
  ba_past <- subset(bus_locs, (alarm==2 & (Angle >= lower_alimit & Angle <= upper_alimit)))
  if(nrow(ba_past) > 0L){ba_past$color <- 3} #currently within limits, but above/below in past=yellow
  
  g <- g + geom_polygon(data = rtpFortMer, 
                        aes(x = long, y = lat, group = group, fill = layer), 
                        alpha = 0.5, 
                        size = 0) + ## size = 0 to remove the polygon outlines
    scale_fill_gradientn("Angle",colours = c("red","yellow","green","blue","black"),limits=c(amin,amax))
  alarm_labs <- NULL
  alarm_vals <- NULL
  if(nrow(ba_normal) > 0L){
    alarm_labs <- c(alarm_labs,paste(lower_alimit," < Angle < ",upper_alimit,sep=""))
    alarm_vals <- c(alarm_vals,"1"="green")
    g <- g+geom_point(data = ba_normal, aes(x=Longitude, y=Latitude, colour=factor(color)),alpha=1, size = 5, shape = 16)
  }
  if(nrow(ba_high) > 0L){
    alarm_labs <- c(alarm_labs,paste("Angle > ",upper_alimit,sep=""))
    alarm_vals <- c(alarm_vals,"2"="red")
    g <- g+geom_point(data = ba_high, aes(x=Longitude, y=Latitude, colour=factor(color)),alpha=1, size = 5, shape = 16)#, show.legend=FALSE)
  }
  if(nrow(ba_past) > 0L){
    alarm_labs <- c(alarm_labs,"Angle Previously outside of Limits")
    alarm_vals <- c(alarm_vals,"3"="yellow")
    g <- g+geom_point(data = ba_past, aes(x=Longitude, y=Latitude, colour=factor(color)),alpha=1, size = 5, shape = 16)#, show.legend=FALSE)
  }
  if(nrow(ba_low) > 0L){
    alarm_labs <- c(alarm_labs,paste("Angle < ",lower_alimit,sep=""))
    alarm_vals <- c(alarm_vals,"4"="blue")
    g <- g+geom_point(data = ba_low, aes(x=Longitude, y=Latitude, colour=factor(color)),alpha=1, size = 5, shape = 16)#, show.legend=FALSE)
  }
  g <- g+  
    scale_colour_manual("Alarm Status",values = alarm_vals,# c("1"="blue", "2"="green", "3"="red", "4"="yellow"),
                        labels=alarm_labs) +
    theme(legend.position="right",legend.direction="vertical",legend.box="horizontal") +
    ggtitle(bquote(atop("Phase Angle at Time",atop(.(Pangle[t,1]),""))))
  g
}

plot_heatmapangle<- function(t){
  if(!exists("autosc")){
    autosc <<- FALSE
  }
  if(!exists("is_zoom")){
    is_zoom <<- FALSE
  }
  bus_locs <- update_pangle(t)
  if (is_zoom) {
    xmn <- min(bus_locs[(bus_locs$Longitude>=z_xmin),"Longitude"])
    xmx <- max(bus_locs[(bus_locs$Longitude<=z_xmin),"Longitude"])
    ymn <- min(bus_locs[(bus_locs$Latitude>z_ymin),"Latitude"])
    ymx <- max(bus_locs[(bus_locs$Latitude>z_ymin),"Latitude"])
  } else{
    xmn <- min(bus_locs$Longitude)
    xmx <- max(bus_locs$Longitude)
    ymn <- min(bus_locs$Latitude)
    ymx <- max(bus_locs$Latitude)
  }
  xstep <- (xmx-xmn)/80
  ystep <- (ymx-ymn)/80
  
  intp_coords <- interp(bus_locs$Longitude, bus_locs$Latitude, bus_locs$Angle, duplicate = "mean",
                        xo=seq(xmn,xmx, by=xstep),
                        yo=seq(ymn,ymx, by=ystep))
  r <- raster(intp_coords)
  rtp <- rasterToPolygons(r)
  
  rtp@data$id <- 1:nrow(rtp@data)   # add id column for join
  rtpFort <- fortify(rtp, data = rtp@data)
  rtpFortMer <- merge(rtpFort, rtp@data, by.x = 'id', by.y = 'id')  # join data
  
  if(autosc == TRUE){
    # amin <- min(b$Angle)
    # amax <- max(b$Angle)
    amin <- ifelse(min(bus_locs$Angle)<(-40),min(bus_locs$Angle),-40)
    amax <- ifelse(max(bus_locs$Angle)>40,max(bus_locs$Angle),40)
    #  adiff <- (amax-amin)
    #  a_lab <- c(amin,(amin+(adiff/4)),(amin+(adiff/2)),(amax-(adiff/4)),amax)
  } else{
    amin <- -40
    amax <- 40
    # a_lab <- c(-40,-20,0,20,40)
  }
  g <- g + geom_polygon(data = rtpFortMer, 
                        aes(x = long, y = lat, group = group, fill = layer), 
                        alpha = 0.5, 
                        size = 0) +  ## size = 0 to remove the polygon outlines
    scale_fill_gradientn("Angle",colours = c("red","yellow","green","blue","black"),limits=c(amin,amax))+
    theme(legend.position="right",legend.direction="vertical",legend.box="horizontal") +
    ggtitle(bquote(atop("Phase Angle at Time",atop(.(Pangle[t,1]),""))))
  g
}

plot_heatmapfreq_alarms<- function(t){
  if(!exists("autosc")){
    autosc <<- FALSE
  }
  if(!exists("is_zoom")){
    is_zoom <<- FALSE
  }
  bus_locs <- update_freq(t)
  if (is_zoom) {
    xmn <- min(bus_locs[(bus_locs$Longitude>=z_xmin),"Longitude"])
    xmx <- max(bus_locs[(bus_locs$Longitude<=z_xmin),"Longitude"])
    ymn <- min(bus_locs[(bus_locs$Latitude>z_ymin),"Latitude"])
    ymx <- max(bus_locs[(bus_locs$Latitude>z_ymin),"Latitude"])
  } else{
    xmn <- min(bus_locs$Longitude)
    xmx <- max(bus_locs$Longitude)
    ymn <- min(bus_locs$Latitude)
    ymx <- max(bus_locs$Latitude)
  }
  if(autosc == TRUE){
    fmin <- ifelse(min(bus_locs$Frequency)<59.8,min(bus_locs$Frequency),59.8)
    fmax <- ifelse(max(bus_locs$Frequency)>60.2,max(bus_locs$Frequency),60.2)
  } else{
    fmin <- 59.8
    fmax <- 60.2
  }
  xstep <- (xmx-xmn)/80
  ystep <- (ymx-ymn)/80
  intp_coords <- interp(bus_locs$Longitude, bus_locs$Latitude, bus_locs$Frequency, duplicate = "mean",
                        xo=seq(xmn,xmx, by=xstep),
                        yo=seq(ymn,ymx, by=ystep))
  r <- raster(intp_coords)
  
  rtp <- rasterToPolygons(r)
  rtp@data$id <- 1:nrow(rtp@data)   # add id column for join
  rtpFort <- fortify(rtp, data = rtp@data)
  rtpFortMer <- merge(rtpFort, rtp@data, by.x = 'id', by.y = 'id')  # join data
  
  if (fmin<59.8 & fmax <= 60.2) {
    f_cols <-c("red","yellow","orange","blue","green")
  } else if(fmax <60.2 & fmin >= 59.8){
    f_cols <-c("green","blue","orange","yellow","red")
  } else{
    f_cols <-c("red","yellow","green","blue","black")
  }
  
  bus_locs$alarm <-apply(bus_locs,1, function(x) update_alarmstatus_freq(t, x))
  bf_high <- subset(bus_locs, (alarm==2 & Frequency > upper_flimit))
  if(nrow(bf_high) > 0L){bf_high$color <- 2} #above limit = red
  
  bf_low <- subset(bus_locs, (alarm==2 & Frequency < lower_flimit))
  if(nrow(bf_low) > 0L){bf_low$color <- 4} #below limit = blue
  
  bf_normal <- subset(bus_locs, (alarm==1))
  if(nrow(bf_normal) > 0L){bf_normal$color <- 1} #within limits = green
  
  bf_past <- subset(bus_locs, (alarm==2 & (Frequency >= lower_flimit & Frequency <= upper_flimit)))
  if(nrow(bf_past) > 0L){bf_past$color <- 3} #currently within limits, but above/below in past=yellow
  alarm_labs <- NULL
  alarm_vals <- NULL
  g <- g + 
    geom_polygon(data = rtpFortMer,
                 aes(x = long, y = lat, group = group, fill = layer),
                 alpha = 0.5,
                 size = 0) +  ## size = 0 to remove the polygon outlines
    scale_fill_gradientn("Frequency",colours = f_cols,limits=c(fmin,fmax))
  if(nrow(bf_normal) > 0L){
    alarm_labs <- c(alarm_labs,paste(lower_flimit," < Frequency < ",upper_flimit,sep=""))
    alarm_vals <- c(alarm_vals,"1"="green")
    g <- g+geom_point(data = bf_normal, aes(x=Longitude, y=Latitude, colour=factor(color)),alpha=1, size = 3, shape = 16)#, show.legend=FALSE)
  }
  if(nrow(bf_high) > 0L){
    alarm_labs <- c(alarm_labs,paste("Frequency > ",upper_flimit,sep=""))
    alarm_vals <- c(alarm_vals,"2"="red")
    g <- g+geom_point(data = bf_high, aes(x=Longitude, y=Latitude, colour=factor(color)),alpha=1, size = 3, shape = 16)#, show.legend=FALSE)
  }
  if(nrow(bf_past) > 0L){
    alarm_labs <- c(alarm_labs,"Frequency Previously outside of Limits")
    alarm_vals <- c(alarm_vals,"3"="yellow")
    g <- g+geom_point(data = bf_past, aes(x=Longitude, y=Latitude, colour=factor(color)),alpha=1, size = 3, shape = 16)#, show.legend=FALSE)
  }
  if(nrow(bf_low) > 0L){
    alarm_labs <- c(alarm_labs,paste("Frequency <",lower_flimit,sep=""))
    alarm_vals <- c(alarm_vals,"4"="blue")
    g <- g+geom_point(data = bf_low, aes(x=Longitude, y=Latitude, colour=factor(color)),alpha=1, size = 3, shape = 16)#, show.legend=FALSE)
  }
  g <- g+scale_colour_manual("Alarm Status",values = alarm_vals,labels=alarm_labs) +
    theme(legend.position="right",legend.direction="vertical",legend.box="horizontal") +
    ggtitle(bquote(atop("Frequency at Time",atop(.(Freq[t,1]),""))))
  g
}

plot_heatmapfreq<- function(t){
  bus_locs <- update_freq(t)
  if(!exists("autosc")){
    autosc <<- FALSE
  }
  if(!exists("is_zoom")){
    is_zoom <<- FALSE
  }
  if(autosc == TRUE){
    fmin <- ifelse(min(bus_locs$Frequency)<59.8,min(bus_locs$Frequency),59.8)
    fmax <- ifelse(max(bus_locs$Frequency)>60.2,max(bus_locs$Frequency),60.2)
    #  vdiff <- (vmax-vmin)
    #   v_lab <- c(vmin,(vmin+(vdiff/4)),(vmin+(vdiff/2)),(vmax-(vdiff/4)),vmax)
  } else{
    fmin <- 59.8
    fmax <- 60.2
  }
  if (is_zoom) {
    xmn <- min(bus_locs[(bus_locs$Longitude>=z_xmin),"Longitude"])
    xmx <- max(bus_locs[(bus_locs$Longitude<=z_xmin),"Longitude"])
    ymn <- min(bus_locs[(bus_locs$Latitude>z_ymin),"Latitude"])
    ymx <- max(bus_locs[(bus_locs$Latitude>z_ymin),"Latitude"])
  } else{
    xmn <- min(bus_locs$Longitude)
    xmx <- max(bus_locs$Longitude)
    ymn <- min(bus_locs$Latitude)
    ymx <- max(bus_locs$Latitude)
  }
  xstep <- (xmx-xmn)/80
  ystep <- (ymx-ymn)/80
  intp_coords <- interp(bus_locs$Longitude, bus_locs$Latitude, bus_locs$Frequency, duplicate = "mean",
                        xo=seq(xmn,xmx, by=xstep),
                        yo=seq(ymn,ymx, by=ystep))
  r <- raster(intp_coords)
  rtp <- rasterToPolygons(r)
  
  rtp@data$id <- 1:nrow(rtp@data)   # add id column for join
  rtpFort <- fortify(rtp, data = rtp@data)
  rtpFortMer <- merge(rtpFort, rtp@data, by.x = 'id', by.y = 'id')  # join data
  if (fmin<59.8 & fmax <= 60.2) {
    f_cols <-c("red","yellow","orange","blue","green")
  } else if(fmax <60.2 & fmin >= 59.8){
    f_cols <-c("green","blue","orange","yellow","red")
  } else{
    f_cols <-c("red","yellow","green","blue","black")
  }
  g <- g + geom_polygon(data = rtpFortMer, 
                        aes(x = long, y = lat, group = group, fill = layer), 
                        alpha = 0.5, 
                        size = 0) +  ## size = 0 to remove the polygon outlines
    geom_point(data=bus_locs,aes(x=Longitude,y=Latitude, colour=Frequency,group=Sub.Name),size=5,alpha=0.7,shape=16) +
    scale_colour_gradientn("Bus Frequency",colours = c("green","blue","orange","yellow"),limits=c(fmin,fmax)) +
    #scale_fill_gradientn("Frequency",colours = topo.colors(255))+
    scale_fill_gradientn("Frequency",colours = f_cols,limits=c(fmin,fmax))+
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




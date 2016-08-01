library(ggplot2)
library(ggmap)
library(outliers)

#Return the names of the functions for display purposes
fnames <- function(){
    n <- list(Map="map",
              Voltage="plot_mapvolt",
              Frequency="plot_mapfreq",
              Voltage_Large="plot_mapvolt_large",
              Frequency_Large="plot_mapfreq_large",
              "Voltage Alarms"="plot_mapvolt_alarms",
              "Frequency Alarms"="plot_mapfreq_alarms")
    if (exists("Pangle")) {
      n <- c(n,Angle="plot_mapangle_lines",
             "Angle Alarms"="plot_mapangle_alarms",
             "Power Factor"="plot_mappowfactor") 
    }

  n
}

#Update the frequency covariance matrix to the given time value
update_covbus_freq <- function(time) {
  #This is in case we call this function on a timepoint before the last one we left off at; if that is the case we want to make sure the loop
  # is starting at time=3 and the xbar is only the mean of the first 2 timepoints.
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
#get covariance matrix of the voltage values, starting from whatever the previous time (curr_sv)
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
#Update the phase angle covariance matrix to the given time value
update_covbus_pangle <- function(time) {
  #This is in case we call this function on a timepoint before the last one we left off at; if that is the case we want to make sure the loop
  # is starting at time=3 and the xbar is only the mean of the first 2 timepoints.
  if (curr_sa<time&&curr_sa>2) {
    ca <- curr_sa
  }
  else{
    ca <- 3
    xabar <- colMeans(Xa[1:2,])
    Sa <- cov(Xa[1:2,])
  }
  for (t in ca:time) {
    xn1 <- Xa[t,]
    Sa <- (((t-1)/t)*Sa)+
      ((1/t+1)*(xn1-xabar)%*%(t((xn1-xabar))))
    xabar <- xabar + (1/(t+1))*(xn1-xabar)
  }
  assign("xabar",xabar,envir = .GlobalEnv)
  assign("curr_sa",time,envir = .GlobalEnv)
  assign("Sa",Sa,envir = .GlobalEnv)
}

#Probably won't be needing these two functions, but I'm leaving them in just in case
get_minmax_covfreq <- function(){
  mincovf <<- 1
  maxcovf <<- 0
  for (t in 1:nrow(Freq)) {
    update_covbus_freq(t)
    mincovf <<- ifelse(min(Sf[,])<mincovf,min(Sf[,]),mincovf)
    maxcovf <<- ifelse(max(Sf[,])>maxcovf,max(Sf[,]),maxcovf)
  }
}
get_minmax_covvolt <- function(){
  mincovv <<- 1
  maxcovv <<- 0
  for (t in 1:nrow(Volt)) {
    update_covbus_volt(t)
    mincovv <<- ifelse(min(Sv[,])<mincovv,min(Sv[,]),mincovv)
    maxcovv <<- ifelse(max(Sv[,])>maxcovv,max(Sv[,]),maxcovv)
  }
} 
  

#Updates the voltage covariance matrix to the given time, converts the covariance matrix to a correlation
# matrix, then adds it to the column in the lines dataframe (linesb) to be used for plotting the
# lines between the buses and returns the new linesb dataframe
get_busline_voltcov <- function(time){
  update_covbus_volt(time)
  for (x in 1:nrow(linesb)) {
    curr_row <- linesb[x,]
    svc <- cov2cor(Sv[,])
    curr_row$Correlation <- as.numeric(as.character(svc[[curr_row$From.Bus.Name,curr_row$To.Bus.Name]]))
    #curr_row$Variance <- as.numeric(as.character(Sv[[curr_row$From.Bus.Name,curr_row$To.Bus.Name]]))
    linesb[x,"Correlation"] <- curr_row$Correlation
  }
  linesb
}
#Updates the frequency covariance matrix to the given time, converts the covariance matrix to a correlation
# matrix, then adds it to the column in the lines dataframe (linesb) to be used for plotting the
# lines between the buses and returns the new linesb dataframe
get_busline_freqcov <- function(time){
  update_covbus_freq(time)
  for (x in 1:nrow(linesb)) {
    curr_row <- linesb[x,]
    sfc <- cov2cor(Sf[,])
    curr_row$Correlation <- as.numeric(as.character(sfc[[curr_row$From.Bus.Name,curr_row$To.Bus.Name]]))
    linesb[x,"Correlation"] <- curr_row$Correlation
  }
  linesb
}
#Updates the phase angle covariance matrix to the given time, converts the covariance matrix to a correlation
# matrix, then adds it to the column in the lines dataframe (linesb) to be used for plotting the
# lines between the buses and returns the new linesb dataframe
get_busline_panglecov <- function(time){
  update_covbus_pangle(time)
  for (x in 1:nrow(linesb)) {
    curr_row <- linesb[x,]
    sac <- cov2cor(Sa[,])
    curr_row$Correlation <- as.numeric(as.character(sac[[curr_row$From.Bus.Name,curr_row$To.Bus.Name]]))
    linesb[x,"Correlation"] <- curr_row$Correlation
  }
  linesb
}



#Change the Frequency column of bus_locs with the frequencies for a given time, then returns the
# new dataframe
update_freq <- function(time){
  tf <- t(Freq[time,-1])
  tf <- cbind(rownames(tf),tf)
  colnames(tf) <- c("Bus.Name","Frequency")
  bus_locs <-  merge(bus_locs[ , ! colnames(bus_locs) %in% c("Frequency")],tf, by="Bus.Name")
  bus_locs$Frequency <- as.numeric(as.character(bus_locs$Frequency))
  #assign("bus_locs",bus_locs,envir = .GlobalEnv)
  bus_locs
}
#Change the Voltage column of bus_locs with the voltages for a given time, then returns the
# new dataframe
update_volt <- function(time){
  tv <- t(Volt[time,-1])
  tv <- cbind(rownames(tv),tv)
  colnames(tv) <- c("Bus.Name","Voltage")
  bus_locs <- merge(bus_locs[ , ! colnames(bus_locs) %in% c("Voltage")],tv, by="Bus.Name")
  bus_locs$Voltage <- as.numeric(as.character(bus_locs$Voltage))
  #assign("bus_locs",bus_locs,envir = .GlobalEnv)
  bus_locs
} 
#Change the Angle column of bus_locs with the angles for a given time, then returns the
# new dataframe
update_pangle <- function(time){
  ta <- t(Pangle[time,-1])
  ta <- cbind(rownames(ta),ta)
  colnames(ta) <- c("Bus.Name","Angle")
  bus_locs <- merge(bus_locs[ , ! colnames(bus_locs) %in% c("Angle")],ta, by="Bus.Name")
  bus_locs$Angle <- as.numeric(as.character(bus_locs$Angle))
  #assign("bus_locs",bus_locs,envir = .GlobalEnv)
  bus_locs
}

#Unusued
get_volt_outliers <- function(time){
  time <- 1205
  curr_v <-Volt[time,-1]
  cvo <- outlier(curr_v)
}
#Unusued
make_sparklines_volt <- function(time){
  curr_v <-Volt[1:time,-1]
  distclust <- gpuDist(curr_v,method = "euclidean")
  cvkm <- kmeans(x = curr_v,iter.max = 20,centers = 10)
  cvkmt <- kmeans(x = t(curr_v),iter.max = 20,centers = 10)
  #Choose number of clusters, kn, and small number, ep, for stopping the iterative process
  
  #QT algorithm
  qual_thresh <- 0.1
  numnode <- sample(1:150,1)
  curr_node <- curr_v[numnode]
  closest_node <- NULL
  for (i in 1:150) {
    curr_dist <- gpuDist(c(curr_node,curr_v[i]),method = "euclidean")
    if (condition) {
      
    }
  }
}


alarm_time <- (60*10) #sample/sec * seconds to check
#Upper/lower limit for the voltage to be considered alarmed
upper_vlimit <- 1.05
lower_vlimit <- 0.95
#Upper/lower limit for the frequency to be considered alarmed
upper_flimit <- 60.05
lower_flimit <- 59.95
#Upper/lower limit for the angle to be considered alarmed
upper_alimit <- 20
lower_alimit <- -20
#Go back <alarm_time> steps to see if the voltage for the given bus ever went outside of the limits
#returns 1 if it never went out of the limits; 2 otherwise
#t = time (current time to look back from)
#b = bus_locs row (bus we want to check)
update_alarmstatus_volt <- function(t,b){
  state <- 1 #Meaning there are no values in the past <alarm_time> steps that are outside of the upper/lower limits
  
  start <- ifelse((t>alarm_time), (t-alarm_time), 1)
  v <- Volt[start:t, b["Bus.Name"]]
  if(length(v[ ((v>upper_vlimit) | (v<lower_vlimit)) ]) > 0) {
    state <- 2 #If there are values outside the limits
  }
  state
}
#Go back <alarm_time> steps to see if the frequency for the given bus ever went outside of the limits
#returns 1 if it never went out of the limits; 2 otherwise
#t = time (current time to look back from)
#b = bus_locs row (bus we want to check)
update_alarmstatus_freq <- function(t,b){
  state <- 1 #Meaning there are no values in the past <alarm_time> steps that are outside of the upper/lower limits
  
  start <- ifelse((t>alarm_time), (t-alarm_time), 1)
  f <- Freq[start:t, b["Bus.Name"]]
  if(length(f[ ((f>upper_flimit) | (f<lower_flimit)) ]) > 0) {
    state <- 2 #If there are values outside the limits
  }
  state
}
#Go back <alarm_time> steps to see if the angle for the given bus ever went outside of the limits
#returns 1 if it never went out of the limits; 2 otherwise
#t = time (current time to look back from)
#b = bus_locs row (bus we want to check)
update_alarmstatus_angle <- function(t,b){
  state <- 1 #Meaning there are no values in the past <alarm_time> steps that are outside of the upper/lower limits
  start <- ifelse((t>alarm_time), (t-alarm_time), 1)
  a <- Pangle[start:t, b["Bus.Name"]]
  if(length(a[ ((a>upper_alimit) | (a<lower_alimit)) ]) > 0) {
    state <- 2 #If there are values outside the limits
  }
  state
}

#given a point that the user clicked on (<point>, from the shiny app), sets <is_zoom> to 1 if it was 0 before,
# and the ggplot that is used for plotting, <g> has its limits changed to 1/4 of the previous size.
# If it was 1 or 2, <g> is reset to its original size using the map_lims list (created in the import_data() function)
# <is_zoom> will be 0 if the map is not zoomed in, 1 if zoomed in, and 2 if we are looking at a single bus.
zoom_map <- function(point){
  if(!exists("is_zoom")){
    is_zoom <<- 0
  }
  if(is_zoom == 1 | is_zoom == 2){
    is_zoom <<- 0
    g <<- ggmap(mapten) +
      scale_x_continuous(limits=c(map_lims[1], map_lims[2]), expand=c(0,0)) + 
      scale_y_continuous(limits=c(map_lims[3], map_lims[4]), expand=c(0,0))
  } else{
    is_zoom <<- 1
    xmin <- min(bus_locs$Longitude)
    xmax <- max(bus_locs$Longitude)
    ymin <- min(bus_locs$Latitude)
    ymax <- max(bus_locs$Latitude)
    
    ratio <- abs(xmax-xmin)/abs(ymax-ymin)
    
    xrange <- abs(xmax-xmin)/4
    yrange <- abs(ymax-ymin)/4 #TODO: Zoom into the nearest cluster instead
    
    xmin <- point[1]-xrange
    xmax <- point[1]+xrange
    ymin <- point[2]-yrange
    ymax <- point[2]+yrange
    
    g <<- ggmap(mapten) +
      scale_x_continuous(limits=c(xmin, xmax), expand=c(0,0)) + 
      scale_y_continuous(limits=c(ymin, ymax), expand=c(0,0))
  }
}
#When called, change the autosc boolean from TRUE to FALSE or FALSE to TRUE
#autoscale being TRUE means that the limits of whatever you are mapping is 
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
#Plot the power factor of each bus - power factor is the absolute value of the cosign of the bus angle
plot_mappowfactor <- function(t){
  if(!exists("is_zoom")){
    is_zoom <<- 0
  }
  bus_locs <- update_pangle(t)
  pf <- subset(bus_locs,select = c("Bus.Name","Longitude","Latitude","Angle"))
  pf$PowFact  <- apply(as.matrix(bus_locs[,"Angle"]), 1,function(x) abs(cos(x)))
  pf$cg <- 0
  pf$cg[pf$PowFact >=.95] <- 2
  pf$cg[pf$PowFact <.95 & pf$PowFact >= .90] <- 1
  linesb <- get_busline_panglecov(t)
  linesb$Correlation[is.nan(linesb$Correlation)] <- 1
  #pf$group[pf$PowFact < 90] <- 
  if(is_zoom==1 | is_zoom==2){
    bus_size <- 10
  } else{
    bus_size <- 5
  }
  g <- g+
    geom_segment(data = linesb,aes(y=From.Latitude,yend=To.Latitude,x=From.Longitude,xend=To.Longitude,colour=Correlation,size=1),show.legend = FALSE) +
    scale_colour_gradientn("Correlation",colours = c("black","brown","grey"),limits=c(-1,1)) +
    geom_point(data=pf,aes(x=Longitude,y=Latitude,fill=factor(cg)),size=bus_size,shape=21) +
    #geom_segment(data = linesb,aes(y=From.Latitude,yend=To.Latitude,x=From.Longitude,xend=To.Longitude,alpha=Variance),show.legend = TRUE) +
    scale_fill_manual(values = c("0"="red","1"="yellow","2"="green"),
                        labels=c("Power Factor<90%","95%>Power Factor>90%","Power Factor>95%"),
                        name="") +
    labs(x = "Longitude", y = "Latitude") +
    theme(legend.position="right",legend.direction="vertical",legend.box="horizontal")# +
   # ggtitle(bquote(atop("Power Factor at Time",atop(.(Pangle[t,1]),""))))
  g
}

#Plot angle of each bus, without the connecting lines
plot_mapangle <- function(t){
  if(!exists("autosc")){
    autosc <<- FALSE
  }
  if(!exists("is_zoom")){
    is_zoom <<- 0
  }
  bus_locs <- update_pangle(t)
  if(autosc == TRUE){
    # amin <- min(b$Angle)
    # amax <- max(b$Angle)
    amin <- ifelse(min(bus_locs$Angle)<(-20),min(bus_locs$Angle),-20)
    amax <- ifelse(max(bus_locs$Angle)>20,max(bus_locs$Angle),20)
    #  adiff <- (amax-amin)
    #  a_lab <- c(amin,(amin+(adiff/4)),(amin+(adiff/2)),(amax-(adiff/4)),amax)
  } else{
    amin <- (-20)
    amax <- 20
    # a_lab <- c(-40,-20,0,20,40)
  }
  if(is_zoom==1 | is_zoom==2){
    bus_size <- 10
  } else{
    bus_size <- 5
  }
  g <- g+
    geom_point(data=bus_locs,aes(x=Longitude,y=Latitude,colour=Angle,group=Sub.Name),size=bus_size,alpha=0.5,shape=16) +
    scale_colour_gradientn("Bus Angle",colours = c("red","yellow","green","blue","black"),limits=c(amin,amax)) +
    labs(x = "Longitude", y = "Latitude") +
    theme(legend.position="right",legend.direction="vertical",legend.box="horizontal") #+
   # ggtitle(bquote(atop("Phase Angle at Time",atop(.(Pangle[t,1]),""))))
  g
}

#Plot angle of a single bus, <near_bus>, without the connecting lines
plot_mapangle_singlebus <- function(t,near_bus){
  if(!exists("autosc")){
    autosc <<- FALSE
  }
  if(!exists("is_zoom")){
    is_zoom <<- 0
  }
  bus_locs <- update_pangle(t)
  bus_locs <- bus_locs[(bus_locs$Latitude==near_bus$Latitude & bus_locs$Longitude == near_bus$Longitude),]
  if(autosc == TRUE){
    # amin <- min(b$Angle)
    # amax <- max(b$Angle)
    amin <- ifelse(min(bus_locs$Angle)<(-20),min(bus_locs$Angle),-20)
    amax <- ifelse(max(bus_locs$Angle)>20,max(bus_locs$Angle),20)
    #  adiff <- (amax-amin)
    #  a_lab <- c(amin,(amin+(adiff/4)),(amin+(adiff/2)),(amax-(adiff/4)),amax)
  } else{
    amin <- (-20)
    amax <- 20
    # a_lab <- c(-40,-20,0,20,40)
  }
  
  if(is_zoom==1 | is_zoom==2){
    bus_size <- 10
  } else{
    bus_size <- 5
  }
  g <- g+
    geom_point(data=bus_locs,aes(x=Longitude,y=Latitude,colour=Angle,group=Sub.Name),size=bus_size,alpha=0.5,shape=16) +
    scale_colour_gradientn("Bus Angle",colours = c("red","yellow","green","blue","black"),limits=c(amin,amax)) +
    labs(x = "Longitude", y = "Latitude") +
    theme(legend.position="right",legend.direction="vertical",legend.box="horizontal") #+
   # ggtitle(bquote(atop("Phase Angle at Time",atop(.(Pangle[t,1]),""))))
  g
}

#Plot angle of each bus with connecting lines (colored by correlation between the buses)
#Autoscale isn't implemented in this yet; just using the min/max angle values
plot_mapangle_lines <- function(t){
  if(!exists("autosc")){
    autosc <<- FALSE
  }
  if(!exists("is_zoom")){
    is_zoom <<- 0
  }
  if(is_zoom==1 | is_zoom==2){
    bus_size <- 10
  } else{
    bus_size <- 5
  }
  bus_locs <- update_pangle(t)
  linesb <- get_busline_panglecov(t)
  linesb$Correlation[is.nan(linesb$Correlation)] <- 1
  g <- g+
    geom_segment(data = linesb,aes(y=From.Latitude,yend=To.Latitude,x=From.Longitude,xend=To.Longitude,colour=Correlation,size=1),alpha=0.4,show.legend = FALSE) +
    scale_colour_gradientn("Correlation",colours = c("red","white","blue"),limits=c(-1,1)) +
    geom_point(data=bus_locs,aes(x=Longitude,y=Latitude,fill=Angle),size=bus_size,shape=21) +
    scale_fill_gradientn("Angle",colours = c("red","yellow","green","blue","black"),limits=c(min(Pangle[,-1]),max(Pangle[,-1]))) +
    labs(x = "Longitude", y = "Latitude") +
    theme(legend.position="right",legend.direction="vertical",legend.box="horizontal") #+
  #  ggtitle(bquote(atop("Phase Angle at Time",atop(.(Pangle[t,1]),""))))
  g
}

#Plot the alarm state of the angles of each bus at the given time
#t = time to plot
#return g, a ggmap object
plot_mapangle_alarms<- function(t){
  if(!exists("autosc")){
    autosc <<- FALSE
  }
  if(!exists("is_zoom")){
    is_zoom <<- 0
  }
  bus_locs <- update_pangle(t)
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
  if(is_zoom==1 | is_zoom==2){
    bus_size <- 10
  } else{
    bus_size <- 5
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
  alarm_labs <- NULL
  alarm_vals <- NULL
  if(nrow(ba_normal) > 0L){
    alarm_labs <- c(alarm_labs,paste(lower_alimit," < Angle < ",upper_alimit,sep=""))
    alarm_vals <- c(alarm_vals,"1"="green")
    g <- g+geom_point(data = ba_normal, aes(x=Longitude, y=Latitude, colour=factor(color)),alpha=1, size = bus_size, shape = 16)
  }
  if(nrow(ba_high) > 0L){
    alarm_labs <- c(alarm_labs,paste("Angle > ",upper_alimit,sep=""))
    alarm_vals <- c(alarm_vals,"2"="red")
    g <- g+geom_point(data = ba_high, aes(x=Longitude, y=Latitude, colour=factor(color)),alpha=1, size = bus_size, shape = 16)#, show.legend=FALSE)
  }
  if(nrow(ba_past) > 0L){
    alarm_labs <- c(alarm_labs,"Angle Previously outside of Limits")
    alarm_vals <- c(alarm_vals,"3"="yellow")
    g <- g+geom_point(data = ba_past, aes(x=Longitude, y=Latitude, colour=factor(color)),alpha=1, size = bus_size, shape = 16)#, show.legend=FALSE)
  }
  if(nrow(ba_low) > 0L){
    alarm_labs <- c(alarm_labs,paste("Angle < ",lower_alimit,sep=""))
    alarm_vals <- c(alarm_vals,"4"="blue")
    g <- g+geom_point(data = ba_low, aes(x=Longitude, y=Latitude, colour=factor(color)),alpha=1, size = bus_size, shape = 16)#, show.legend=FALSE)
  }
  g <- g+  
    scale_colour_manual("Alarm Status",values = alarm_vals,# c("1"="blue", "2"="green", "3"="red", "4"="yellow"),
                        labels=alarm_labs) +
    theme(legend.position="right",legend.direction="vertical",legend.box="horizontal")# +
    #ggtitle(bquote(atop("Phase Angle at Time",atop(.(Pangle[t,1]),""))))
  g
}

#Plot the alarm state of the angles of a bus at the given time
#t = time to plot
#return g, a ggmap object
plot_mapangle_alarms_singlebus<- function(t,near_bus){
  if(!exists("autosc")){
    autosc <<- FALSE
  }
  if(!exists("is_zoom")){
    is_zoom <<- 0
  }
  bus_locs <- update_pangle(t)
  bus_locs <- bus_locs[(bus_locs$Latitude==near_bus$Latitude & bus_locs$Longitude == near_bus$Longitude),]
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
  if(is_zoom==1 | is_zoom==2){
    bus_size <- 10
  } else{
    bus_size <- 5
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
  alarm_labs <- NULL
  alarm_vals <- NULL
  if(nrow(ba_normal) > 0L){
    alarm_labs <- c(alarm_labs,paste(lower_alimit," < Angle < ",upper_alimit,sep=""))
    alarm_vals <- c(alarm_vals,"1"="green")
    g <- g+geom_point(data = ba_normal, aes(x=Longitude, y=Latitude, colour=factor(color)),alpha=1, size = bus_size, shape = 16)
  }
  if(nrow(ba_high) > 0L){
    alarm_labs <- c(alarm_labs,paste("Angle > ",upper_alimit,sep=""))
    alarm_vals <- c(alarm_vals,"2"="red")
    g <- g+geom_point(data = ba_high, aes(x=Longitude, y=Latitude, colour=factor(color)),alpha=1, size = bus_size, shape = 16)#, show.legend=FALSE)
  }
  if(nrow(ba_past) > 0L){
    alarm_labs <- c(alarm_labs,"Angle Previously outside of Limits")
    alarm_vals <- c(alarm_vals,"3"="yellow")
    g <- g+geom_point(data = ba_past, aes(x=Longitude, y=Latitude, colour=factor(color)),alpha=1, size = bus_size, shape = 16)#, show.legend=FALSE)
  }
  if(nrow(ba_low) > 0L){
    alarm_labs <- c(alarm_labs,paste("Angle < ",lower_alimit,sep=""))
    alarm_vals <- c(alarm_vals,"4"="blue")
    g <- g+geom_point(data = ba_low, aes(x=Longitude, y=Latitude, colour=factor(color)),alpha=1, size = bus_size, shape = 16)#, show.legend=FALSE)
  }
  g <- g+  
    scale_colour_manual("Alarm Status",values = alarm_vals,# c("1"="blue", "2"="green", "3"="red", "4"="yellow"),
                        labels=alarm_labs) +
    theme(legend.position="right",legend.direction="vertical",legend.box="horizontal")# +
   # ggtitle(bquote(atop("Phase Angle at Time",atop(.(Pangle[t,1]),""))))
  g
}


#return g (a ggmap object) with each point (representing each bus) colored according to the 
# voltage at time t and lines connecting buses colored by the correlation of the bus
plot_mapvolt <- function(t){
  if(!exists("autosc")){
    autosc <<- FALSE
  }
  if(!exists("is_zoom")){
    is_zoom <<- 0
  }
  # g <- ggmap(mapten)+
  #  scale_x_continuous(limits = c(-90.6, -81), expand = c(0, 0)) +
  #   scale_y_continuous(limits = c(34.5, 37), expand = c(0, 0))

  bus_locs <- update_volt(t)
  linesb <- get_busline_voltcov(t)
  linesb$Correlation[is.nan(linesb$Correlation)] <- 1
  if(autosc == TRUE){
    # vmin <- min(Volt[t,-1])
    #vmax <- max(Volt[t,-1])
    vmin <- ifelse(min(bus_locs$Voltage)<0.95,min(bus_locs$Voltage),0.95)
    vmax <- ifelse(max(bus_locs$Voltage)>1.05,max(bus_locs$Voltage),1.05)
  } else{
    vmin <- 0.95
    vmax <- 1.05
  }
  if (vmin<0.95 & vmax <= 1.05) {
    v_cols <-c("red","orange","yellow","blue","green")
  } else if(vmax <1.05 & vmin >= 0.95){
    v_cols <-c("green","blue","orange","yellow","red")
  } else{
    v_cols <-c("red","orange","yellow","green","blue")
  }
  if(is_zoom==1 | is_zoom==2){
    bus_size <- 10
  } else{
    bus_size <- 5
  }
 g <- g + geom_segment(data = linesb,aes(y=From.Latitude,yend=To.Latitude,x=From.Longitude,xend=To.Longitude,colour=Correlation,size=1),alpha=0.4,show.legend = FALSE) +
    scale_colour_gradientn("Correlation",colours = c("red","white","blue"),limits=c(-1,1)) +
    geom_point(data = bus_locs, aes(x=Longitude,y=Latitude,fill = Voltage ), size = bus_size, shape = 21) +
    scale_fill_gradientn("Voltage",colours = v_cols,limits=c(vmin,vmax)) +
    theme(legend.position="right",legend.direction="vertical",legend.box="horizontal")# +
#    ggtitle(bquote(atop("Voltage at Time",atop(.(Volt[t,1]),""))))
  g
}

#return g (a ggmap object) with a point (representing a bus, <near_bus>) colored according to the 
# voltage at time t
plot_mapvolt_singlebus <- function(t,near_bus){
  if(!exists("autosc")){
    autosc <<- FALSE
  }
  if(!exists("is_zoom")){
    is_zoom <<- 0
  }
  bus_locs <- update_volt(t)
 # linesb <- get_busline_voltcov(t)
 # linesb$Correlation[is.nan(linesb$Correlation)] <- 1
 # print(typeof(near_bus))
 # print(paste("near_bus[1]: ",near_bus[1],sep = ""))
 # print(paste("near_bus[2]: ",near_bus[2],sep = ""))
  b <- bus_locs[((bus_locs$Latitude == near_bus[2]) & (bus_locs$Longitude == near_bus[1])),]
#  print(b)
#  print(paste("b: ",b,sep = ""))
 # bus_locs <- b#bus_locs[((bus_locs$Latitude %in% b$Latitude) & (bus_locs$Longitude%in%b$Longitude)),]
#  print(paste("bus_locs: ",bus_locs,sep = ""))
  if (autosc == TRUE & is_zoom==2) {
    vmin <- 0.95
    vmax <- 1.05
  } else if(autosc == TRUE){
    # vmin <- min(Volt[t,-1])
    #vmax <- max(Volt[t,-1])
    vmin <- ifelse(min(bus_locs$Voltage)<0.95,min(bus_locs$Voltage),0.95)
    vmax <- ifelse(max(bus_locs$Voltage)>1.05,max(bus_locs$Voltage),1.05)
  } else{
    vmin <- 0.95
    vmax <- 1.05
  }
  if (vmin<0.95 & vmax <= 1.05) {
    v_cols <-c("red","orange","yellow","blue","green")
  } else if(vmax <1.05 & vmin >= 0.95){
    v_cols <-c("green","blue","orange","yellow","red")
  } else{
    v_cols <-c("red","orange","yellow","green","blue")
  }
  if(is_zoom==1 | is_zoom==2){
    bus_size <- 10
  } else{
    bus_size <- 5
  }
  g <- g + #geom_segment(data = linesb,aes(y=From.Latitude,yend=To.Latitude,x=From.Longitude,xend=To.Longitude,colour=Correlation,size=1),alpha=0.4,show.legend = FALSE) +
  #  scale_colour_gradientn("Correlation",colours = c("red","white","blue"),limits=c(-1,1)) +
    geom_jitter(data = b, aes(x=Longitude,y=Latitude,fill = Voltage ), size = bus_size, shape = 21) +
    scale_fill_gradientn("Voltage",colours = v_cols,limits=c(vmin,vmax)) +
    theme(legend.position="right",legend.direction="vertical",legend.box="horizontal")# +
   # ggtitle(bquote(atop("Voltage at Time",atop(.(Volt[t,1]),""))))
  g
}

#Plot the alarm state of the voltage of each bus at the given time
#t = time to plot
#return g, a ggmap object
plot_mapvolt_alarms<- function(t){
  if(!exists("autosc")){
    autosc <<- FALSE
  }
  if(!exists("is_zoom")){
    is_zoom <<- 0
  }
  bus_locs <- update_volt(t)
  if(autosc == TRUE){
    vmin <- min(bus_locs$Voltage)
    vmax <- max(bus_locs$Voltage)
    #vmin <- ifelse(min(bus_locs$Voltage)<0.8,min(bus_locs$Voltage),0.8)
    # vmax <- ifelse(max(bus_locs$Voltage)>1.2,max(bus_locs$Voltage),1.2)
  } else{
    vmin <- 0.8
    vmax <- 1.2
  }
  if (vmin<0.8 & vmax <= 1.2) {
    v_cols <-c("red","yellow","orange","blue","green")
  } else if(vmax <1.2 & vmin >= 0.8){
    v_cols <-c("green","blue","orange","yellow","red")
  } else{
    v_cols <-c("red","yellow","green","blue","black")
  }
  if(is_zoom==1 | is_zoom==2){
    bus_size <- 10
  } else{
    bus_size <- 5
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
  if(nrow(bv_normal) > 0L){
    alarm_labs <- c(alarm_labs,paste(lower_vlimit," < Voltage < ",upper_vlimit,sep=""))
    alarm_vals <- c(alarm_vals,"1"="green")
    g <- g+geom_point(data = bv_normal, aes(x=Longitude, y=Latitude, colour=factor(color)),alpha=1, size = bus_size, shape = 16)#, show.legend=FALSE)
  }
  if(nrow(bv_high) > 0L){
    alarm_labs <- c(alarm_labs,paste("Voltage > ",upper_vlimit,sep=""))
    alarm_vals <- c(alarm_vals,"2"="red")
    g <- g+geom_point(data = bv_high, aes(x=Longitude, y=Latitude, colour=factor(color)),alpha=1, size = bus_size, shape = 16)#, show.legend=FALSE)
  }
  if(nrow(bv_past) > 0L){
    alarm_labs <- c(alarm_labs,"Voltage Previously outside of Limits")
    alarm_vals <- c(alarm_vals,"3"="yellow")
    g <- g+geom_point(data = bv_past, aes(x=Longitude, y=Latitude, colour=factor(color)),alpha=1, size = bus_size, shape = 16)#, show.legend=FALSE)
  }
  if(nrow(bv_low) > 0L){
    alarm_labs <- c(alarm_labs,paste("Voltage < ",lower_vlimit,sep=""))
    alarm_vals <- c(alarm_vals,"4"="blue")
    g <- g+geom_point(data = bv_low, aes(x=Longitude, y=Latitude, colour=factor(color)),alpha=1, size = bus_size, shape = 16)#, show.legend=FALSE)
  }
  g <- g+scale_colour_manual("Alarm Status", values = alarm_vals,labels=alarm_labs) +
    #geom_point(data=bus_locs,aes(x=Longitude,y=Latitude, colour=alarm,group=Sub.Name),size=5,alpha=0.7,shape=16) +
    #  scale_colour_gradientn("Alarm Status",colours = c("green","blue","orange","yellow"),limits=c(vmin,vmax)) +
    theme(legend.position="right",legend.direction="vertical",legend.box="horizontal") #+
  #ggtitle(bquote(atop("Voltage at Time",atop(.(Volt[t,1]),""))))
  g
}

#Plot the alarm state of the voltage of each bus at the given time
#t = time to plot
#return g, a ggmap object
plot_mapvolt_alarms_singlebus<- function(t, near_bus){
  if(!exists("autosc")){
    autosc <<- FALSE
  }
  if(!exists("is_zoom")){
    is_zoom <<- 0
  }
  bus_locs <- update_volt(t)
  bus_locs <- bus_locs[(bus_locs$Latitude==near_bus$Latitude & bus_locs$Longitude == near_bus$Longitude),]
  if(autosc == TRUE){
    vmin <- min(bus_locs$Voltage)
    vmax <- max(bus_locs$Voltage)
    #vmin <- ifelse(min(bus_locs$Voltage)<0.8,min(bus_locs$Voltage),0.8)
    # vmax <- ifelse(max(bus_locs$Voltage)>1.2,max(bus_locs$Voltage),1.2)
  } else{
    vmin <- 0.8
    vmax <- 1.2
  }
  if (vmin<0.8 & vmax <= 1.2) {
    v_cols <-c("red","yellow","orange","blue","green")
  } else if(vmax <1.2 & vmin >= 0.8){
    v_cols <-c("green","blue","orange","yellow","red")
  } else{
    v_cols <-c("red","yellow","green","blue","black")
  }
  if(is_zoom==1 | is_zoom==2){
    bus_size <- 10
  } else{
    bus_size <- 5
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
  if(nrow(bv_normal) > 0L){
    alarm_labs <- c(alarm_labs,paste(lower_vlimit," < Voltage < ",upper_vlimit,sep=""))
    alarm_vals <- c(alarm_vals,"1"="green")
    g <- g+geom_point(data = bv_normal, aes(x=Longitude, y=Latitude, colour=factor(color)),alpha=1, size = bus_size, shape = 16)#, show.legend=FALSE)
  }
  if(nrow(bv_high) > 0L){
    alarm_labs <- c(alarm_labs,paste("Voltage > ",upper_vlimit,sep=""))
    alarm_vals <- c(alarm_vals,"2"="red")
    g <- g+geom_point(data = bv_high, aes(x=Longitude, y=Latitude, colour=factor(color)),alpha=1, size = bus_size, shape = 16)#, show.legend=FALSE)
  }
  if(nrow(bv_past) > 0L){
    alarm_labs <- c(alarm_labs,"Voltage Previously outside of Limits")
    alarm_vals <- c(alarm_vals,"3"="yellow")
    g <- g+geom_point(data = bv_past, aes(x=Longitude, y=Latitude, colour=factor(color)),alpha=1, size = bus_size, shape = 16)#, show.legend=FALSE)
  }
  if(nrow(bv_low) > 0L){
    alarm_labs <- c(alarm_labs,paste("Voltage < ",lower_vlimit,sep=""))
    alarm_vals <- c(alarm_vals,"4"="blue")
    g <- g+geom_point(data = bv_low, aes(x=Longitude, y=Latitude, colour=factor(color)),alpha=1, size = bus_size, shape = 16)#, show.legend=FALSE)
  }
  g <- g+scale_colour_manual("Alarm Status", values = alarm_vals,labels=alarm_labs) +
    #geom_point(data=bus_locs,aes(x=Longitude,y=Latitude, colour=alarm,group=Sub.Name),size=5,alpha=0.7,shape=16) +
    #  scale_colour_gradientn("Alarm Status",colours = c("green","blue","orange","yellow"),limits=c(vmin,vmax)) +
    theme(legend.position="right",legend.direction="vertical",legend.box="horizontal") #+
  #  ggtitle(bquote(atop("Voltage at Time",atop(.(Volt[t,1]),""))))
  g
}

#return g (a ggmap object) with each point (representing each bus) colored according to the 
# frequency at time t and lines connecting buses colored by the correlation of the bus
plot_mapfreq <- function(t){
  if(!exists("autosc")){
    autosc <<- FALSE
  }
  if(!exists("is_zoom")){
    is_zoom <<- 0
  }
  bus_locs <- update_freq(t)
  if(autosc == TRUE){
    fmin <- ifelse(min(bus_locs$Frequency)<59.8,min(bus_locs$Frequency),59.8)
    fmax <- ifelse(max(bus_locs$Frequency)>60.2,max(bus_locs$Frequency),60.2)
  } else{
    fmin <- 59.8
    fmax <- 60.2
  }
  linesb <- get_busline_freqcov(t)
  linesb$Correlation[is.nan(linesb$Correlation)] <- 1
  if (fmin<59.8 & fmax <= 60.2) {
    f_cols <-c("red","yellow","orange","blue","green")
  } else if(fmax <60.2 & fmin >= 59.8){
    f_cols <-c("green","blue","orange","yellow","red")
  } else{
    f_cols <-c("red","yellow","green","blue","black")
  }
  if(is_zoom==1 | is_zoom==2){
    bus_size <- 10
  } else{
    bus_size <- 5
  }
 # f_cols <- ifelse(min(Freq[,-1])<60,c("red","yellow","orange","blue","green"),c("green","blue","red","orange","yellow"))
  #color_vals_freq <- as.numeric(sapply( c((mincovf+0.1),(maxcovf/4),(maxcovf/2),(maxcovf-0.1)), function(N) formatC(signif(N, digits=3), digits=3,format="fg", flag="#")))
  #Below is for the TS dataset
  #color_vals_freq <- as.numeric(sapply( c(mincovf,0.2,0.4,0.6,(maxcovf-0.08)), function(N) formatC(signif(N, digits=3), digits=3,format="fg", flag="#")))
  #Below is for the GMD dataset
  #color_vals_freq <- as.numeric(sapply( c(mincovf,50,100,150,200,(maxcovf-5)), function(N) formatC(signif(N, digits=3), digits=3,format="fg", flag="#")))
  g <- g + geom_segment(data = linesb,aes(y=From.Latitude,yend=To.Latitude,x=From.Longitude,xend=To.Longitude,colour=Correlation,size=0.1),alpha=0.4,show.legend = FALSE) +
    scale_colour_gradientn("Correlation",colours = c("red","white","blue"),limits=c(-1,1)) +
    geom_point(data = bus_locs, aes(x=Longitude,y=Latitude,fill = Frequency ), size = bus_size, shape = 21) +
    scale_fill_gradientn("Frequency",colours = f_cols,limits=c(fmin,fmax)) +
    theme(legend.position="right",legend.direction="vertical",legend.box="horizontal") #+
  #    ggtitle(bquote(atop("Frequency at Time",atop(.(Freq[t,1]),""))))
  g
}


#return g (a ggmap object) with a point (representing a bus, <near_bus>) colored according to the 
# frequency at time t
plot_mapfreq_singlebus <- function(t, near_bus){
  if(!exists("autosc")){
    autosc <<- FALSE
  }
  if(!exists("is_zoom")){
    is_zoom <<- 0
  }
  bus_locs <- update_freq(t)
  bus_locs <- bus_locs[(bus_locs$Latitude==near_bus$Latitude & bus_locs$Longitude == near_bus$Longitude),]
  if(autosc == TRUE){
    fmin <- ifelse(min(bus_locs$Frequency)<59.8,min(bus_locs$Frequency),59.8)
    fmax <- ifelse(max(bus_locs$Frequency)>60.2,max(bus_locs$Frequency),60.2)
  } else{
    fmin <- 59.8
    fmax <- 60.2
  }
#  linesb <- get_busline_freqcov(t)
#  linesb$Correlation[is.nan(linesb$Correlation)] <- 1
  if (fmin<59.8 & fmax <= 60.2) {
    f_cols <-c("red","yellow","orange","blue","green")
  } else if(fmax <60.2 & fmin >= 59.8){
    f_cols <-c("green","blue","orange","yellow","red")
  } else{
    f_cols <-c("red","yellow","green","blue","black")
  }
  if(is_zoom==1 | is_zoom==2){
    bus_size <- 10
  } else{
    bus_size <- 5
  }
  # f_cols <- ifelse(min(Freq[,-1])<60,c("red","yellow","orange","blue","green"),c("green","blue","red","orange","yellow"))
  #color_vals_freq <- as.numeric(sapply( c((mincovf+0.1),(maxcovf/4),(maxcovf/2),(maxcovf-0.1)), function(N) formatC(signif(N, digits=3), digits=3,format="fg", flag="#")))
  #Below is for the TS dataset
  #color_vals_freq <- as.numeric(sapply( c(mincovf,0.2,0.4,0.6,(maxcovf-0.08)), function(N) formatC(signif(N, digits=3), digits=3,format="fg", flag="#")))
  #Below is for the GMD dataset
  #color_vals_freq <- as.numeric(sapply( c(mincovf,50,100,150,200,(maxcovf-5)), function(N) formatC(signif(N, digits=3), digits=3,format="fg", flag="#")))
  g <- g +# geom_segment(data = linesb,aes(y=From.Latitude,yend=To.Latitude,x=From.Longitude,xend=To.Longitude,colour=Correlation,size=0.1),alpha=0.4,show.legend = FALSE) +
    #scale_colour_gradientn("Correlation",colours = c("red","white","blue"),limits=c(-1,1)) +
    geom_point(data = bus_locs, aes(x=Longitude,y=Latitude,fill = Frequency ), size = bus_size, shape = 21) +
    scale_fill_gradientn("Frequency",colours = f_cols,limits=c(fmin,fmax)) +
    theme(legend.position="right",legend.direction="vertical",legend.box="horizontal")# +
  #ggtitle(bquote(atop("Frequency at Time",atop(.(Freq[t,1]),""))))
  g
}

#Plot the alarm state of the frequency of each bus at the given time
#t = time to plot
#return g, a ggmap object
plot_mapfreq_alarms<- function(t){
  if(!exists("autosc")){
    autosc <<- FALSE
  }
  if(!exists("is_zoom")){
    is_zoom <<- 0
  }
  bus_locs <- update_freq(t)
  if(autosc == TRUE){
    fmin <- ifelse(min(bus_locs$Frequency)<59.8,min(bus_locs$Frequency),59.8)
    fmax <- ifelse(max(bus_locs$Frequency)>60.2,max(bus_locs$Frequency),60.2)
  } else{
    fmin <- 59.8
    fmax <- 60.2
  }

  if (fmin<59.8 & fmax <= 60.2) {
    f_cols <-c("red","yellow","orange","blue","green")
  } else if(fmax <60.2 & fmin >= 59.8){
    f_cols <-c("green","blue","orange","yellow","red")
  } else{
    f_cols <-c("red","yellow","green","blue","black")
  }
  if(is_zoom==1 | is_zoom==2){
    bus_size <- 10
  } else{
    bus_size <- 5
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
  if(nrow(bf_normal) > 0L){
    alarm_labs <- c(alarm_labs,paste(lower_flimit," < Frequency < ",upper_flimit,sep=""))
    alarm_vals <- c(alarm_vals,"1"="green")
    g <- g+geom_point(data = bf_normal, aes(x=Longitude, y=Latitude, colour=factor(color)),alpha=0.9, size = bus_size, shape = 16)#, show.legend=FALSE)
  }
  if(nrow(bf_high) > 0L){
    alarm_labs <- c(alarm_labs,paste("Frequency > ",upper_flimit,sep=""))
    alarm_vals <- c(alarm_vals,"2"="red")
    g <- g+geom_point(data = bf_high, aes(x=Longitude, y=Latitude, colour=factor(color)),alpha=0.9, size = bus_size, shape = 16)#, show.legend=FALSE)
  }
  if(nrow(bf_past) > 0L){
    alarm_labs <- c(alarm_labs,"Frequency Previously outside of Limits")
    alarm_vals <- c(alarm_vals,"3"="yellow")
    g <- g+geom_point(data = bf_past, aes(x=Longitude, y=Latitude, colour=factor(color)),alpha=0.9, size = bus_size, shape = 16)#, show.legend=FALSE)
  }
  if(nrow(bf_low) > 0L){
    alarm_labs <- c(alarm_labs,paste("Frequency <",lower_flimit,sep=""))
    alarm_vals <- c(alarm_vals,"4"="blue")
    g <- g+geom_point(data = bf_low, aes(x=Longitude, y=Latitude, colour=factor(color)),alpha=0.9, size = bus_size, shape = 16)#, show.legend=FALSE)
  }
  g <- g+scale_colour_manual("Alarm Status",values = alarm_vals,labels=alarm_labs) +
    theme(legend.position="right",legend.direction="vertical",legend.box="horizontal")# +
  # ggtitle(bquote(atop("Frequency at Time",atop(.(Freq[t,1]),""))))
  g
}

#Plot the alarm state of the frequency of a bus at the given time
#t = time to plot
#near_bus = bus to plot
#return g, a ggmap object
plot_mapfreq_alarms_singlebus<- function(t){
  if(!exists("autosc")){
    autosc <<- FALSE
  }
  if(!exists("is_zoom")){
    is_zoom <<- 0
  }
  bus_locs <- update_freq(t)
  bus_locs <- bus_locs[(bus_locs$Latitude==near_bus$Latitude & bus_locs$Longitude == near_bus$Longitude),]
  if(autosc == TRUE){
    fmin <- ifelse(min(bus_locs$Frequency)<59.8,min(bus_locs$Frequency),59.8)
    fmax <- ifelse(max(bus_locs$Frequency)>60.2,max(bus_locs$Frequency),60.2)
  } else{
    fmin <- 59.8
    fmax <- 60.2
  }
  
  if (fmin<59.8 & fmax <= 60.2) {
    f_cols <-c("red","yellow","orange","blue","green")
  } else if(fmax <60.2 & fmin >= 59.8){
    f_cols <-c("green","blue","orange","yellow","red")
  } else{
    f_cols <-c("red","yellow","green","blue","black")
  }
  if(is_zoom==1 | is_zoom==2){
    bus_size <- 10
  } else{
    bus_size <- 5
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
  if(nrow(bf_normal) > 0L){
    alarm_labs <- c(alarm_labs,paste(lower_flimit," < Frequency < ",upper_flimit,sep=""))
    alarm_vals <- c(alarm_vals,"1"="green")
    g <- g+geom_point(data = bf_normal, aes(x=Longitude, y=Latitude, colour=factor(color)),alpha=0.9, size = bus_size, shape = 16)#, show.legend=FALSE)
  }
  if(nrow(bf_high) > 0L){
    alarm_labs <- c(alarm_labs,paste("Frequency > ",upper_flimit,sep=""))
    alarm_vals <- c(alarm_vals,"2"="red")
    g <- g+geom_point(data = bf_high, aes(x=Longitude, y=Latitude, colour=factor(color)),alpha=0.9, size = bus_size, shape = 16)#, show.legend=FALSE)
  }
  if(nrow(bf_past) > 0L){
    alarm_labs <- c(alarm_labs,"Frequency Previously outside of Limits")
    alarm_vals <- c(alarm_vals,"3"="yellow")
    g <- g+geom_point(data = bf_past, aes(x=Longitude, y=Latitude, colour=factor(color)),alpha=0.9, size = bus_size, shape = 16)#, show.legend=FALSE)
  }
  if(nrow(bf_low) > 0L){
    alarm_labs <- c(alarm_labs,paste("Frequency <",lower_flimit,sep=""))
    alarm_vals <- c(alarm_vals,"4"="blue")
    g <- g+geom_point(data = bf_low, aes(x=Longitude, y=Latitude, colour=factor(color)),alpha=0.9, size = bus_size, shape = 16)#, show.legend=FALSE)
  }
  g <- g+scale_colour_manual("Alarm Status",values = alarm_vals,labels=alarm_labs) +
    theme(legend.position="right",legend.direction="vertical",legend.box="horizontal") #+
  #  ggtitle(bquote(atop("Frequency at Time",atop(.(Freq[t,1]),""))))
  g
}

#return g (a ggmap object) with each point (representing each bus) colored according to the 
# voltage at time t; each point is a large semi-transparent circle rather than a small solid circle
plot_mapvolt_large <- function(t){
  if(!exists("is_zoom")){
    is_zoom <<- 0
  }
  if(!exists("autosc")){
    autosc <<- FALSE
  }
  bus_locs <- update_volt(t)
  # linesb <- get_busline_voltcov(t)
  if(autosc == TRUE){
    vmin <- min(bus_locs$Voltage)
    vmax <- max(max(bus_locs$Voltage))
    #vmin <- ifelse(min(bus_locs$Voltage)<0.8,min(bus_locs$Voltage),0.8)
    #vmax <- ifelse(max(bus_locs$Voltage)>1.2,max(bus_locs$Voltage),1.2)
  } else{
    vmin <- 0.95
    vmax <- 1.05
  }
  if (vmin<0.95 & vmax <= 1.05) {
    v_cols <-c("red","yellow","orange","blue","green")
  } else if(vmax <1.05 & vmin >= 0.95){
    v_cols <-c("green","blue","orange","yellow","red")
  } else{
    v_cols <-c("red","yellow","green","blue","black")
  }
  if(is_zoom==1 | is_zoom==2){
    bus_size <- 25
  } else{
    bus_size <- 10
  }
  g <- g+
    geom_point(data=bus_locs,aes(x=Longitude,y=Latitude,color=Voltage),size=bus_size,alpha=0.25,shape=16) +
    #geom_segment(data = linesb,aes(y=From.Latitude,yend=To.Latitude,x=From.Longitude,xend=To.Longitude,alpha=Variance),show.legend = TRUE) +
    scale_color_gradientn("Bus Voltage",colors = v_cols,limits=c(vmin,vmax)) +
    labs(x = "Longitude", y = "Latitude") +
    theme(legend.position="right",legend.direction="vertical",legend.box="horizontal") #+
  #  ggtitle(bquote(atop("Voltage at Time",atop(.(Volt[t,1]),""))))
  g
}
#return g (a ggmap object) with each point (representing each bus) colored according to the 
# frequency at time t; each point is a large semi-transparent circle rather than a small solid circle
plot_mapfreq_large <- function(t){
  if(!exists("is_zoom")){
    is_zoom <<- 0
  }
  if(is_zoom==1 | is_zoom==2){
    bus_size <- 25
  } else{
    bus_size <- 10
  }
  #  g <- ggmap(mapten)+scale_x_continuous(limits = c(-90.6, -81), expand = c(0, 0)) +scale_y_continuous(limits = c(34.5, 37), expand = c(0, 0))
  bus_locs <- update_freq(t)
  linesb <- get_busline_freqcov(t)
 # color_vals_freq <- as.numeric(sapply( c((mincovf+0.1),(maxcovf/4),(maxcovf/2),(maxcovf-0.1)), function(N) formatC(signif(N, digits=3), digits=3,format="fg", flag="#")))
  g <- g+ 
    geom_point(data=bus_locs,aes(x=Longitude,y=Latitude, colour=Frequency,group=Sub.Name),size=bus_size,alpha=0.25,shape=16) +
    scale_colour_gradientn("Bus Frequency",colours = c("green","blue","orange","yellow"),limits=c(min(bus_locs$Frequency),max(bus_locs$Frequency))) +
    labs(x = "Longitude", y = "Latitude") +
    #geom_segment(data = linesb,aes(y=From.Latitude,yend=To.Latitude,x=From.Longitude,xend=To.Longitude,fill=Variance),show.legend = TRUE) +
    #theme(legend.position="bottom",legend.direction="vertical",legend.box="horizontal") +
    #scale_fill_gradientn("Variance",colours = c("green","blue","red","orange","yellow"),breaks=color_vals_freq,limits=c(0,maxcovf)) +
    #geom_point(data = bus_locs, aes(x=Longitude,y=Latitude,fill = Frequency ), size = 2, shape = 21) +
    #scale_fill_gradientn("Frequency",colours = c("blue","white","red"),limits=c(min(Freq[,-1]),max(Freq[,-1]))) +
    theme(legend.position="right",legend.direction="vertical",legend.box="horizontal")# +
  # ggtitle(bquote(atop("Frequency at Time",atop(.(Freq[t,1]),""))))
  g
}



#Plot the voltages of just the buses with a Nominal kV of 230. This isn't used by the display
plot_mapvolt_230 <- function(t){
  if(!exists("autosc")){
    autosc <<- FALSE
  }
  if(!exists("is_zoom")){
    is_zoom <<- 0
  }
  # g <- ggmap(mapten)+
  #  scale_x_continuous(limits = c(-90.6, -81), expand = c(0, 0)) +
  #   scale_y_continuous(limits = c(34.5, 37), expand = c(0, 0))
  bus_locs <- update_volt(t)
  b <- bus_locs[with(bus_locs, (Nominal.kV == 230) ),]
  linesb <- get_busline_voltcov(t)
  linesb$Correlation[is.nan(linesb$Correlation)] <- 1
  lb <- linesb[with(linesb,((From.Bus.Name %in% b$Bus.Name) | (To.Bus.Name %in% b$Bus.Name))),]
  if(autosc == TRUE){
    # vmin <- min(Volt[t,-1])
    #vmax <- max(Volt[t,-1])
    vmin <- ifelse(min(b$Voltage)<0.95,min(b$Voltage),0.95)
    vmax <- ifelse(max(b$Voltage)>1.05,max(b$Voltage),1.05)
  } else{
    vmin <- 0.95
    vmax <- 1.05
  }
  if (vmin<0.95 & vmax <= 1.05) {
    v_cols <-c("red","orange","yellow","blue","green")
  } else if(vmax <1.05 & vmin >= 0.95){
    v_cols <-c("green","blue","orange","yellow","red")
  } else{
    v_cols <-c("red","orange","yellow","green","blue")
  }
  g <- g + 
    geom_point(data = b, aes(x=Longitude,y=Latitude,fill = Voltage ), size = 4, shape = 21) +
    scale_fill_gradientn("Voltage",colours = v_cols,limits=c(vmin,vmax)) +
    geom_segment(data = lb,aes(y=From.Latitude,yend=To.Latitude,x=From.Longitude,xend=To.Longitude,colour=Correlation,size=1),alpha=0.4,show.legend = FALSE) +
    scale_colour_gradientn("Correlation",colours = c("red","white","blue"),limits=c(-1,1)) +
    theme(legend.position="right",legend.direction="vertical",legend.box="horizontal")# +
  # ggtitle(bquote(atop("Voltage at Time",atop(.(Volt[t,1]),""))))
  g
}

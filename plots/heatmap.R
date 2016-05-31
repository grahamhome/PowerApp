library(ggplot2)
library(ggmap)

source("data/import_ts.R")
import_data()

fnames <- function(){
  n <- list(heatmap="Heat Map",
            plot_heatmapvolt="Voltage",
            plot_heatmapfreq="Frequency")
  n
}


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


plot_heatmapvolt <- function(t){
  #g <- ggmap(mapten)+scale_x_continuous(limits = c(-90.6, -81), expand = c(0, 0)) +scale_y_continuous(limits = c(34.5, 37), expand = c(0, 0))
  update_volt(t)
  linesb <- get_busline_voltcov(t)
  #  g <- g %+% bus_locs + aes(x=Longitude,y=Latitude,z=Voltage) +
  #geom_point(data=bus_locs,aes(x=Longitude,y=Latitude,z=Voltage)) +
  #   stat_summary_2d(fun=median, binwidth = c(.45, .45),alpha = 1)+
  #    scale_fill_gradientn(name = "Voltage",colours = c('yellow','orange','brown'),space = "Lab") + 
  g <- g+
    geom_tile(data = bus_locs, aes(x=Longitude,y=Latitude,alpha=Voltage),fill='red')+
    #stat_density2d(data = bus_locs, aes(x=Longitude,y=Latitude,fill= bus_locs$Voltage,alpha = ..level..),geom = 'polygon')+
    #scale_fill_gradientn("Voltage Density", colours = c('yellow','red','brown'),limits=c(min(Volt[,-1]),max(Volt[,-1]))) + 
    #scale_alpha(name="Density")+
    #geom_point(data=bus_locs,aes(x=Longitude,y=Latitude,colour=Voltage)) +
   # scale_colour_gradientn("Bus Voltage",colours = c("red","white","blue"),limits=c(min(Volt[,-1]),max(Volt[,-1]))) +
    labs(x = "Longitude", y = "Latitude") +
    #coord_map()+
    theme(legend.position="bottom",legend.direction="vertical",legend.box="horizontal") +
    ggtitle(bquote(atop("Voltage at Time",atop(.(Volt[t,1]),""))))
  g
}
plot_heatmapfreq <- function(t){
  #  g <- ggmap(mapten)+scale_x_continuous(limits = c(-90.6, -81), expand = c(0, 0)) +scale_y_continuous(limits = c(34.5, 37), expand = c(0, 0))
  update_freq(t)
  linesb <- get_busline_freqcov(t)
  g <- g+ 
    #geom_density_2d(data = bus_locs, aes(x=Longitude,y=Latitude,fill= bus_locs$Frequency))+
    #stat_density_2d(data = bus_locs, aes(x=Longitude,y=Latitude,fill= bus_locs$Frequency,alpha = ..level..),geom = 'polygon')+
    #scale_fill_gradientn("Frequency Density", colours = c('white','red','brown'),limits=c(min(Freq[,-1]),max(Freq[,-1]))) + 
    #scale_alpha(name="Density")+
    geom_point(data=bus_locs,aes(x=Longitude,y=Latitude,colour=Frequency)) +
    scale_colour_gradientn("Bus Frequency",colours = c("blue","white","red"),limits=c(min(Freq[,-1]),max(Freq[,-1]))) +
    labs(x = "Longitude", y = "Latitude") +
    coord_map()+
    theme(legend.position="bottom",legend.direction="vertical",legend.box="horizontal") +
    ggtitle(bquote(atop("Frequency at Time",atop(.(Freq[t,1]),""))))
  g
}

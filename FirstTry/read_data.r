library(ggplot2)
library(ggmap)
library(manipulate)
library(compare)
library(animation)
library(scales)

#Change the frequency column of bus_locs with the frequencies for a given time 
# must (assign function return to bus_locs)
update_freq <- function(time){
  tf <- t(F[time,-1])
  tf <- cbind(rownames(tf),tf)
  colnames(tf) <- c("Bus.Name","Frequency")
  bus_locs <- merge(subset(bus_locs,select = c("Bus.Num","Bus.Name","Sub.Name","Latitude","Longitude","Voltage")),tf, by="Bus.Name")
  bus_locs$Frequency <- as.numeric(as.character(bus_locs$Frequency))
  bus_locs
}

#Change the voltage column of bus_locs with the frequencies for a given time
# must (assign function return to bus_locs)
update_volt <- function(time){
  vf <- t(V[time,-1])
  vf <- cbind(rownames(vf),vf)
  colnames(vf) <- c("Bus.Name","Voltage")
  bus_locs <- merge(subset(bus_locs,select = c("Bus.Num","Bus.Name","Sub.Name","Latitude","Longitude","Frequency")),vf, by="Bus.Name")
  bus_locs$Voltage <- as.numeric(as.character(bus_locs$Voltage))
  bus_locs
} 

#get covariance matrix of the frequency values, starting from whatever the previous time (curr_sf)
# was to whatever "time" is. 
get_covbus_freq <- function(time) {
  if (curr_sf<time&&curr_sf>2) {
    cf <- curr_sf
  }
  else{
    cf <- 3
    xfbar <- colMeans(Xf[1:(cf-1),])
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
get_covbus_volt <- function(time) {
  #This is in case we call this function on a timepoint before the last one we left off at; if that is the case we want to make sure the loop
  # is starting at time=3 and the xbar is only the mean of the first 2 timepoints.
  if (curr_sv<time&&curr_sv>2) {
    cv <- curr_sv
  }
  else{
    cv <- 3
    xvbar <- colMeans(Xv[1:(cv-1),])
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

#init_environ <- function(){
buses <- read.csv("Data/tenn150_buses.csv")
substat <- read.csv("Data/tenn150_substations.csv")
loads <- read.csv("Data/tenn150_loads.csv")
generators <- read.csv("Data/tenn150_generators.csv")
linesb <- read.csv("Data/tenn150_lines.csv")
trans <- read.csv("Data/tenn150_transformers.csv")
#V <- read.csv("tenn150gmd_bus_voltage.csv")
#F <- read.csv("tenn150gmd_bus_frequency.csv")
V <- read.csv("Data/tenn150ts_bus_voltage.csv")
F <- read.csv("Data/tenn150ts_bus_frequency.csv")

bn <- buses$Bus.Name
bn <- gsub(" ",".",bn)
buses$Bus.Name <- bn

bn <- linesb$To.Bus.Name
bn <- gsub(" ",".",bn)
linesb$To.Bus.Name <- bn
bn <- linesb$From.Bus.Name
bn <- gsub(" ",".",bn)
linesb$From.Bus.Name <- bn

sub_buses <- merge(buses,substat,by = "Sub.ID")

#replace names in Frequency table with just bus names 
cn <- colnames(F[,-1])
cn <- gsub("Bus[.]","",cn)
cn <- gsub("[.]Frequency","",cn)
cn <- gsub("[.][.]",".",cn)
cn <- gsub("Gallatin[.]TN[.]","Gallatin.(TN)",cn)
colnames(F) <- c("Time",cn)
#replace names in Voltage table with just bus names 
cn <- colnames(V[,-1])
cn <- gsub("Bus[.]","",cn)
cn <- gsub("[.]V[.]pu","",cn)
cn <- gsub("[.][.]",".",cn)
cn <- gsub("Gallatin[.]TN[.]","Gallatin.(TN)",cn)
colnames(V) <- c("Time",cn)

#Set up dataframe with each bus name, bus number, the name of the substation it's at,
# the lat/long of the station, the frequency (at time=0), and the voltage (at time=0)
bus_locs <- data.frame(sub_buses$Bus.Num,sub_buses$Bus.Name,sub_buses$Sub.Name.x,sub_buses$Latitude,sub_buses$Longitude,"Frequency","Voltage")
colnames(bus_locs) <- c("Bus.Num","Bus.Name","Sub.Name", "Latitude","Longitude","Frequency","Voltage")
bus_locs$Frequency <- bus_locs$Voltage <- 0 

#Add the frequency values from time t=0 to bus_locs dataframe
bus_locs <- update_freq(1)
#Add the voltage values from time t=0 to bus_locs dataframe
bus_locs <- update_volt(1)

#Add the from/to bus latitudes/longitudes to the line data
linesb <- merge(linesb,bus_locs[,c("Bus.Num","Latitude","Longitude")], by.x = "From.Bus.Num", by.y = "Bus.Num")
linesb <- merge(linesb,bus_locs[,c("Bus.Num","Latitude","Longitude")], by.x = "To.Bus.Num", by.y = "Bus.Num")
cn <- colnames(linesb)
cn <- gsub("Latitude[.]x","From.Latitude",cn)
cn <- gsub("Longitude[.]x","From.Longitude",cn)
cn <- gsub("Latitude[.]y","To.Latitude",cn)
cn <- gsub("Longitude[.]y","To.Longitude",cn)
colnames(linesb) <- cn
linesb$Variance <- 0
#trans <- merge(trans,bus_locs[,c("Bus.Num","Latitude","Longitude")], by.x = "From.Bus.Num", by.y = "Bus.Num")
#trans <- merge(trans,bus_locs[,c("Bus.Num","Latitude","Longitude")], by.x = "To.Bus.Num", by.y = "Bus.Num")
#Create the map to use as the background for the ggplot
mapten <- get_map(location = c(lon = mean(bus_locs$Longitude), lat = mean(bus_locs$Latitude)), 
                  zoom = 6, maptype = "roadmap", scale = 2)
g <- ggmap(mapten)+ scale_x_continuous(limits = c(-90.6, -81), expand = c(0, 0)) + scale_y_continuous(limits = c(34.5, 37), expand = c(0, 0))
#Set up the variables for the voltage covariance matrix
Xv <- as.matrix(V[,-1])
Sv <- cov(Xv[1:2,])
curr_sv <- 3
xvbar <- colMeans(Xv[1:2,])
#Set up these variables for the frequency covariance matrix
Xf <- as.matrix(F[,-1])
Sf <- cov(Xf[1:2,])
curr_sf <- 3
xfbar <- colMeans(Xf[1:2,])

mincovf <- 1
maxcovf <- 0
for (t in 3:3606) {
  get_covbus_freq(t)
  mincovf <- ifelse(min(Sf[,])<mincovf,min(Sf[,]),mincovf)
  maxcovf <- ifelse(max(Sf[,])>maxcovf,max(Sf[,]),maxcovf)
}
mincovv <- 1
maxcovv <- 0
for (t in 3:3606) {
  get_covbus_volt(t)
  mincovv <- ifelse(min(Sv[,])<mincovv,min(Sv[,]),mincovv)
  maxcovv <- ifelse(max(Sv[,])>maxcovv,max(Sv[,]),maxcovv)
}

#}
# create_freqvideo(1,3606,"freq_by_time_1to3606_mapconnected_ts_colscale.mp4")
# create_voltvideo(1000,1200,"voltmap_1000to1200_ts_colscale_covlinecolors2.mp4")
# create_voltvideo(1,3605,"voltmap_1to3605_ts_colscale_covlinecolors2.mp4")
# create_freqvideo(1,3605,"freqmap_1to3605_ts_colscale_covlinecolors.mp4")

#This finds the min/max covariances over the whole dataset, for labeling purposes

#covariance matrix of the voltage values
#get_covbus_volt <- function(time,S) {
#  X <- as.matrix(V[,-1])
#  S <- cov(X[1:2,])
#  xbar <- colMeans(X[1:2,])
#  for (t in 3:time) {
#    xn1 <- X[t,]
#    S <- (((t-1)/t)*S)+
#      ((1/t+1)*(xn1-xbar)%*%(t((xn1-xbar))))
#    xbar <- xbar + (1/(t+1))*(xn1-xbar)
#  }
#  S
#}
update_busline_voltcov <- function(time){
  get_covbus_volt(time)
  cov_volt <- Sv
  for (x in 1:nrow(linesb)) {
    curr_row <- linesb[x,]
    curr_row$Variance <- as.numeric(as.character(cov_volt[[curr_row$From.Bus.Name,curr_row$To.Bus.Name]]))
    linesb[x,"Variance"] <- curr_row$Variance
  }
  linesb
}
update_busline_freqcov <- function(time){
  get_covbus_freq(time)
  cov_freq <- Sf
  for (x in 1:nrow(linesb)) {
    curr_row <- linesb[x,]
    curr_row$Variance <- as.numeric(as.character(cov_freq[[curr_row$From.Bus.Name,curr_row$To.Bus.Name]]))
    linesb[x,"Variance"] <- curr_row$Variance
  }
  linesb
}

#make a ggmap object, and create+save a .mp4 file with a given name for the frequency of the
# buses over the specified time period (from start-stop)
create_freqvideo <- function(start,stop,vidtitle){

  saveVideo({
    ani.options(interval = 0.05)
    for (t in start:stop) {
      print(plotmapfreq(t))
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
      suppressMessages(print(plotmapvolt(t)))
    }
  },video.name = vidtitle)
}


#Attempt to print ggmaps fast enough for real-time updates. 
#Right now it's too slow, but is that because of creating the graphs or printing them?
#Creating all the graphs in the series before we print any of them should answer that.
plotmapvolt <- function(t1, t2){
  x11()

  gList = list() # @Jake: I think this is how I would make a list...

  for (t in t1:t2) {
  	bus_locs <- update_volt(t)
  	linesb <- update_busline_voltcov(t)
  	color_vals_volt <- as.numeric(sapply( c(mincovv,2,4,6,(maxcovv-0.4)), function(N) formatC(signif(N, digits=3), digits=3,format="fg", flag="#")))
  	g <- g + geom_segment(data = linesb,aes(y=From.Latitude,yend=To.Latitude,x=From.Longitude,xend=To.Longitude,colour=Variance),show.legend = TRUE) +
   	 scale_colour_gradientn("Variance",colours = c("black","blue","red"),breaks=color_vals_volt,limits=c(0,maxcovv)) +
   	 geom_point(data = bus_locs, aes(x=Longitude,y=Latitude,fill = Voltage ), size = 2, shape = 21) +
  	  scale_fill_gradientn("Voltage",colours = c("blue","white","red"),limits=c(min(V[,-1]),max(V[,-1]))) +
   	 theme(legend.position="bottom",legend.direction="vertical",legend.box="horizontal") +
    	ggtitle(V[t,1])

 	print(g)			#Jake: Here I want to push g onto my list instead of printing it
 	#Sys.sleep(.09)
  }

  # @Jake: Then here I would loop through my list and print each ggmap object with the Sys.sleep(.09) delay used above.
}

#Returns a single plot for a given time value
plotmapvoltsingle <- function(t){
  bus_locs <- update_volt(t)
  linesb <- update_busline_voltcov(t)
  color_vals_volt <- as.numeric(sapply( c(mincovv,2,4,6,(maxcovv-0.4)), function(N) formatC(signif(N, digits=3), digits=3,format="fg", flag="#")))
  g <- g + geom_segment(data = linesb,aes(y=From.Latitude,yend=To.Latitude,x=From.Longitude,xend=To.Longitude,colour=Variance),show.legend = TRUE) +
   scale_colour_gradientn("Variance",colours = c("black","blue","red"),breaks=color_vals_volt,limits=c(0,maxcovv)) +
   geom_point(data = bus_locs, aes(x=Longitude,y=Latitude,fill = Voltage ), size = 2, shape = 21) +
   scale_fill_gradientn("Voltage",colours = c("blue","white","red"),limits=c(min(V[,-1]),max(V[,-1]))) +
   theme(legend.position="bottom",legend.direction="vertical",legend.box="horizontal") +
   ggtitle(V[t,1])
  g
}

#plot the a line graph of each bus from {start} to {stop} of the voltage
plotvoltage <- function(start,stop,plotname){
  par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
  start <- ifelse(missing(start),1,start)
  stop <- ifelse(missing(stop),nrow(V),stop)
  xrange <- range(V[start:stop,1])
  yrange <- range(V[start:stop,-1])
  plot(xrange,yrange,type = "n",xlab = "Time (seconds)",ylab = "Voltage")
  num_sig_bus <- 0
  n <- ncol(V)-1
  colors <- rainbow(n)
  linetype <- c(1:n)
  plotchar <- colnames(V[,-1])
  for (z in 2:n){
    bus <- V[start:stop,z]
    lines(y=bus,x=V[start:stop,1],col=colors[z],type = "l",lty=linetype[z])
  }
  title(plotname)
  legend("topright", inset=c(-0.2,-0.15),legend = 1:n,col = colors, lty=linetype,cex=0.8)
}
#plot the a line graph of each bus from {start} to {stop} of the voltage
plotfrequency <- function(start,stop,plotname){
  par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
  start <- ifelse(missing(start),1,start)
  stop <- ifelse(missing(stop),nrow(F),stop)
  xrange <- range(F[start:stop,1])
  yrange <- range(F[start:stop,-1])
  plot(xrange,yrange,type = "n",xlab = "Time (seconds)",ylab = "Frequency")
  num_sig_bus <- 0
  n <- ncol(F)-1
  colors <- rainbow(n)
  linetype <- c(1:n)
  plotchar <- colnames(F[,-1])
  for (z in 2:n){
    bus <- F[start:stop,z]
    lines(y=bus,x=F[start:stop,1],col=colors[z],type = "l",lty=linetype[z])
  }
  title(plotname)
  legend("topright", inset=c(-0.2,-0.15),legend = 1:n,col = colors, lty=linetype,cex=0.8)
}

#return g (a ggmap object) with each point (representing each bus) colored according to the 
# frequency at time t
plotmapfreq <- function(t){
  bus_locs <- update_freq(t)
  linesb <- update_busline_freqcov(t)
  color_vals_freq <- as.numeric(sapply( c(mincovf,0.2,0.4,0.6,(maxcovf-0.08)), function(N) formatC(signif(N, digits=3), digits=3,format="fg", flag="#")))
  g <- g + geom_segment(data = linesb,aes(y=From.Latitude,yend=To.Latitude,x=From.Longitude,xend=To.Longitude,colour=Variance),show.legend = TRUE) +
    scale_colour_gradientn("Variance",colours = c("black","blue","red"),breaks=color_vals_freq,limits=c(0,maxcovf)) +
    geom_point(data = bus_locs, aes(x=Longitude,y=Latitude,fill = Frequency ), size = 2, shape = 21) +
    scale_fill_gradientn("Frequency",colours = c("blue","white","red"),limits=c(min(F[,-1]),max(F[,-1]))) +
    theme(legend.position="bottom",legend.direction="vertical",legend.box="horizontal") +
    ggtitle(F[t,1])
}

#x11()
#plot(plotmapfreq(1))
#Sys.sleep(10)
#dev.off()

#g <- ggmap(mapten)
#g <- g+ geom_point(data = bus_locs, aes(x=Longitude,y=Latitude,fill = Frequency), size = 2, shape = 21) 
#g <- g+ scale_fill_continuous("Frequency",low = "red",high = "blue",guide = "colourbar")
#g <- g+ geom_point(data = bus_locs, aes(x=Longitude,y=Latitude,fill = Frequency), size = 2, shape = 21) 

#Create the map to use as the background for the ggplot
#mapten <- get_map(location = c(lon = mean(station_locs$substat.Longitude), lat = mean(station_locs$substat.Latitude)), 
#                  zoom = 6, maptype = "roadmap", scale = 2)
#Save as an mp4 file - takes a while if you use >100 timepoints
#saveVideo({
  #g is a ggplot (ggmap) object
#  g <- ggmap(mapten)+
#    scale_x_continuous(limits = c(-90.6, -81), expand = c(0, 0)) +
#    scale_y_continuous(limits = c(34.5, 37), expand = c(0, 0))
#  for (t in 1:3606) {
#    bus_locs <- update_volt(t)
#    bus_locs$Voltage <- as.numeric(as.character(bus_locs$Voltage))
#    #colors <- colorRampPalette(c("blue", "yellow", "red"))(length(levels(bus_locs$Voltage)))
 #   print(g
#          + geom_jitter(data = bus_locs, aes(x=Longitude,y=Latitude,fill = Voltage ), size = 2, shape = 21) 
#          #+ scale_fill_manual(values = setNames(colors,levels(bus_locs$Voltage)))
          #+ scale_fill_continuous(low = "red",high = "blue",limits=c(0,1.0616))
#          + scale_fill_continuous(low = "red",high = "blue")
#          + theme(legend.position="bottom",legend.direction="vertical")
#          + ggtitle(t)
#    )
#  }
#},video.name = "mapplotvolt_tennTS_1to3606.mp4")





#mapgilbert <- get_map(location = c(lon = mean(station_locs$substat.Longitude), lat = mean(station_locs$substat.Latitude)), zoom = 6,
#mapgilbert <- get_map(location = "tennessee", zoom = 6,
#                      maptype = "roadmap", scale = 2)
#ggmap(mapgilbert) +
#  geom_point(data = station_locs, aes(x=substat.Longitude,y=substat.Latitude, fill = "red"), size = 2, shape = 21) 


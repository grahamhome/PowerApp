library(ggplot2)
library(ggmap)
library(outliers)

fnames <- function(){
  n <- list(Map="map",
            Voltage="plot_mapvolt",
            Frequency="plot_mapfreq",
            Voltage_Large="plot_mapvolt_large",
            Frequency_Large="plot_mapfreq_large")

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

#return g (a ggmap object) with each point (representing each bus) colored according to the 
# voltage at time t
plot_mapvolt <- function(t){
  if(!exists("mincovf") | !exists("maxcovf")){
    get_minmax_covfreq()
  }
  if(!exists("mincovv") | !exists("maxcovv")){
    get_minmax_covvolt()
  }
  # g <- ggmap(mapten)+
  #  scale_x_continuous(limits = c(-90.6, -81), expand = c(0, 0)) +
  #   scale_y_continuous(limits = c(34.5, 37), expand = c(0, 0))
  #color_vals_freq <- rescale(c(min(F[,-1]),60.05,60.1,60.15,max(F[,-1])))
  #color_vals_volt <- rescale(c(min(V[,-1]),0.25,0.5,0.75,1,max(V[,-1])))
  update_volt(t)
  linesb <- get_busline_voltcov(t)
  color_vals_volt <- as.numeric(sapply(  c(mincovf,(maxcovf/4),(maxcovf/2),(maxcovf-0.1)), function(N) formatC(signif(N, digits=3), digits=3,format="fg", flag="#")))
  #Below is for the TS dataset
  #color_vals_volt <- as.numeric(sapply( c(mincovv,2,4,6,(maxcovv-0.4)), function(N) formatC(signif(N, digits=3), digits=3,format="fg", flag="#")))
  #Below is for the GMD dataset
  #color_vals_volt <- as.numeric(sapply( c(mincovv,1.5,3,4,(maxcovv-0.2)), function(N) formatC(signif(N, digits=3), digits=3,format="fg", flag="#")))
  g <- g + geom_segment(data = linesb,aes(y=From.Latitude,yend=To.Latitude,x=From.Longitude,xend=To.Longitude,colour=Variance),show.legend = TRUE) +
    scale_colour_gradientn("Variance",colours = c("black","blue","red"),breaks=color_vals_volt,limits=c(mincovv,maxcovv)) +
    geom_point(data = bus_locs, aes(x=Longitude,y=Latitude,fill = Voltage ), size = 4, shape = 21) +
    scale_fill_gradientn("Voltage",colours = c("orange","green","blue","red"),limits=c(min(Volt[,-1]),max(Volt[,-1]))) +
    theme(legend.position="right",legend.direction="vertical",legend.box="horizontal") +
    ggtitle(bquote(atop("Voltage at Time",atop(.(Volt[t,1]),""))))
  g
}
#return g (a ggmap object) with each point (representing each bus) colored according to the 
# frequency at time t
plot_mapfreq <- function(t){
  if(!exists("mincovf") | !exists("maxcovf")){
    get_minmax_covfreq()
  }
  if(!exists("mincovv") | !exists("maxcovv")){
    get_minmax_covvolt()
  }
  update_freq(t)
  linesb <- get_busline_freqcov(t)
  color_vals_freq <- as.numeric(sapply( c((mincovf+0.1),(maxcovf/4),(maxcovf/2),(maxcovf-0.1)), function(N) formatC(signif(N, digits=3), digits=3,format="fg", flag="#")))
  #Below is for the TS dataset
  #color_vals_freq <- as.numeric(sapply( c(mincovf,0.2,0.4,0.6,(maxcovf-0.08)), function(N) formatC(signif(N, digits=3), digits=3,format="fg", flag="#")))
  #Below is for the GMD dataset
  #color_vals_freq <- as.numeric(sapply( c(mincovf,50,100,150,200,(maxcovf-5)), function(N) formatC(signif(N, digits=3), digits=3,format="fg", flag="#")))
  g <- g + geom_segment(data = linesb,aes(y=From.Latitude,yend=To.Latitude,x=From.Longitude,xend=To.Longitude,colour=Variance),show.legend = TRUE) +
    scale_colour_gradientn("Variance",colours = c("black","blue","red"),breaks=color_vals_freq,limits=c(mincovf,maxcovf)) +
    geom_point(data = bus_locs, aes(x=Longitude,y=Latitude,fill = Frequency ), size = 4, shape = 21) +
    scale_fill_gradientn("Frequency",colours = c("yellow","orange","blue","green"),limits=c(min(Freq[,-1]),max(Freq[,-1]))) +
    theme(legend.position="right",legend.direction="vertical",legend.box="horizontal") +
    ggtitle(bquote(atop("Frequency at Time",atop(.(Freq[t,1]),""))))
  g
}

plot_mapvolt_large <- function(t){
  if(!exists("mincovf") | !exists("maxcovf")){
    get_minmax_covfreq()
  }
  if(!exists("mincovv") | !exists("maxcovv")){
    get_minmax_covvolt()
  }
  #g <- ggmap(mapten)+scale_x_continuous(limits = c(-90.6, -81), expand = c(0, 0)) +scale_y_continuous(limits = c(34.5, 37), expand = c(0, 0))
  update_volt(t)
  linesb <- get_busline_voltcov(t)
  g <- g+
    geom_point(data=bus_locs,aes(x=Longitude,y=Latitude,colour=Voltage,group=Sub.Name),size=25,alpha=0.25,shape=16) +
    #geom_segment(data = linesb,aes(y=From.Latitude,yend=To.Latitude,x=From.Longitude,xend=To.Longitude,alpha=Variance),show.legend = TRUE) +
    scale_colour_gradientn("Bus Voltage",colours = c("yellow","orange","blue","green"),limits=c(min(Volt[,-1]),max(Volt[,-1]))) +
    labs(x = "Longitude", y = "Latitude") +
    theme(legend.position="right",legend.direction="vertical",legend.box="horizontal") +
    ggtitle(bquote(atop("Voltage at Time",atop(.(Volt[t,1]),""))))
  g
}
plot_mapfreq_large <- function(t){
  if(!exists("mincovf") | !exists("maxcovf")){
    get_minmax_covfreq()
  }
  if(!exists("mincovv") | !exists("maxcovv")){
    get_minmax_covvolt()
  }
  #  g <- ggmap(mapten)+scale_x_continuous(limits = c(-90.6, -81), expand = c(0, 0)) +scale_y_continuous(limits = c(34.5, 37), expand = c(0, 0))
  update_freq(t)
  linesb <- get_busline_freqcov(t)
  g <- g+ 
    geom_point(data=bus_locs,aes(x=Longitude,y=Latitude,colour=Frequency,group=Sub.Name),size=25,alpha=0.25,shape=16) +
    #geom_segment(data = linesb,aes(y=From.Latitude,yend=To.Latitude,x=From.Longitude,xend=To.Longitude,alpha=Variance),show.legend = TRUE) +
    scale_colour_gradientn("Bus Frequency",colours = c("green","blue","orange","yellow"),limits=c(min(Freq[,-1]),max(Freq[,-1]))) +
    labs(x = "Longitude", y = "Latitude") +
    #theme(legend.position="bottom",legend.direction="vertical",legend.box="horizontal") +
    #scale_colour_gradientn("Variance",colours = c("black","blue","red"),breaks=color_vals_freq,limits=c(0,maxcovf)) +
    #geom_point(data = bus_locs, aes(x=Longitude,y=Latitude,fill = Frequency ), size = 2, shape = 21) +
    #scale_fill_gradientn("Frequency",colours = c("blue","white","red"),limits=c(min(Freq[,-1]),max(Freq[,-1]))) +
    theme(legend.position="right",legend.direction="vertical",legend.box="horizontal") +
    ggtitle(bquote(atop("Frequency at Time",atop(.(Freq[t,1]),""))))
  g
}



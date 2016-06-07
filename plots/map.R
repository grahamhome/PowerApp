library(ggplot2)
library(ggmap)
library(outliers)


fnames <- function(){
  if (!exists("Pangle")) {
    n <- list(Map="map",
              Voltage="plot_mapvolt",
              Frequency="plot_mapfreq",
              Voltage_Large="plot_mapvolt_large",
              Frequency_Large="plot_mapfreq_large",
              Angle="plot_mapangle",
              Angle_lines="plot_mapangle_lines")
  } else{
    n <- list(Map="map",
              Voltage="plot_mapvolt",
              Frequency="plot_mapfreq",
              Voltage_Large="plot_mapvolt_large",
              Frequency_Large="plot_mapfreq_large")
  }

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
  tv <- t(Volt[time,-1])
  tv <- cbind(rownames(tv),tv)
  colnames(tv) <- c("Bus.Name","Voltage")
  bus_locs <- merge(subset(bus_locs,select = c("Bus.Num","Bus.Name","Sub.Name","Latitude","Longitude","Frequency")),tv, by="Bus.Name")
  bus_locs$Voltage <- as.numeric(as.character(bus_locs$Voltage))
  assign("bus_locs",bus_locs,envir = .GlobalEnv)
} 

update_pangle <- function(time){
  ta <- t(Pangle[time,-1])
  ta <- cbind(rownames(ta),ta)
  colnames(ta) <- c("Bus.Name","Angle")
  bus_locs <- merge(subset(bus_locs,select = c("Bus.Num","Bus.Name","Sub.Name","Latitude","Longitude","Voltage","Frequency")),ta, by="Bus.Name")
  bus_locs$Angle <- as.numeric(as.character(bus_locs$Angle))
  assign("bus_locs",bus_locs,envir = .GlobalEnv)
}

get_volt_outliers <- function(time){
  curr_v <- Volt[time,]
  
}


plot_mapangle <- function(t){
 # if (!exists("Pangle")) {
  #  p <- ggplot(data.frame())+
   #   geom_text(data=NULL,aes(x=2,y=2,label="Phase angle data not available for this data set"))
  #  return(p)
#  }
  update_pangle(t)
  g <- g+
    geom_point(data=bus_locs,aes(x=Longitude,y=Latitude,colour=Angle,group=Sub.Name),size=10,alpha=0.5,shape=16) +
    #geom_segment(data = linesb,aes(y=From.Latitude,yend=To.Latitude,x=From.Longitude,xend=To.Longitude,alpha=Variance),show.legend = TRUE) +
    scale_colour_gradientn("Bus Angle",colours = c("yellow","orange","blue","green"),limits=c(min(Pangle[,-1]),max(Pangle[,-1]))) +
    labs(x = "Longitude", y = "Latitude") +
    theme(legend.position="right",legend.direction="vertical",legend.box="horizontal") +
    ggtitle(bquote(atop("Phase Angle at Time",atop(.(Pangle[t,1]),""))))
  g
}
plot_mapangle_lines <- function(t){
  update_pangle(t)
  linesb <- get_busline_panglecov(t)
  linesb$Correlation[is.nan(linesb$Correlation)] <- 1
  g <- g+
    geom_segment(data = linesb,aes(y=From.Latitude,yend=To.Latitude,x=From.Longitude,xend=To.Longitude,colour=Correlation,size=1),show.legend = FALSE) +
    scale_colour_gradientn("Correlation",colours = c("red","yellow","green"),limits=c(-1,1)) +
    geom_point(data=bus_locs,aes(x=Longitude,y=Latitude,fill=Angle),size=5,shape=21) +
    scale_fill_gradientn("Angle",colours = c("yellow","orange","blue","green"),limits=c(min(Pangle[,-1]),max(Pangle[,-1]))) +
    labs(x = "Longitude", y = "Latitude") +
    theme(legend.position="right",legend.direction="vertical",legend.box="horizontal") +
    ggtitle(bquote(atop("Phase Angle at Time",atop(.(Pangle[t,1]),""))))
  g
}

#return g (a ggmap object) with each point (representing each bus) colored according to the 
# voltage at time t
plot_mapvolt <- function(t){

  # g <- ggmap(mapten)+
  #  scale_x_continuous(limits = c(-90.6, -81), expand = c(0, 0)) +
  #   scale_y_continuous(limits = c(34.5, 37), expand = c(0, 0))
  #color_vals_freq <- rescale(c(min(F[,-1]),60.05,60.1,60.15,max(F[,-1])))
  #color_vals_volt <- rescale(c(min(V[,-1]),0.25,0.5,0.75,1,max(V[,-1])))
  update_volt(t)
  linesb <- get_busline_voltcov(t)
  linesb$Correlation[is.nan(linesb$Correlation)] <- 1
  if (min(Volt[,-1])<1) {
    v_cols <-c("red","yellow","orange","blue","green")
  } else{
    v_cols <-c("green","blue","red","orange","yellow")
  }
  #color_vals_volt <- as.numeric(sapply(  c(mincovf,(maxcovf/4),(maxcovf/2),(maxcovf-0.1)), function(N) formatC(signif(N, digits=3), digits=3,format="fg", flag="#")))
  #Below is for the TS dataset
  #color_vals_volt <- as.numeric(sapply( c(mincovv,2,4,6,(maxcovv-0.4)), function(N) formatC(signif(N, digits=3), digits=3,format="fg", flag="#")))
  #Below is for the GMD dataset
  #color_vals_volt <- as.numeric(sapply( c(mincovv,1.5,3,4,(maxcovv-0.2)), function(N) formatC(signif(N, digits=3), digits=3,format="fg", flag="#")))
  g <- g + geom_segment(data = linesb,aes(y=From.Latitude,yend=To.Latitude,x=From.Longitude,xend=To.Longitude,colour=Correlation,size=1),show.legend = FALSE) +
    scale_colour_gradientn("Correlation",colours = c("red","yellow","green"),limits=c(0,1)) +
    geom_point(data = bus_locs, aes(x=Longitude,y=Latitude,fill = Voltage ), size = 4, shape = 21) +
    scale_fill_gradientn("Voltage",colours = v_cols,limits=c(min(Volt[,-1]),max(Volt[,-1]))) +
    theme(legend.position="right",legend.direction="vertical",legend.box="horizontal") +
    ggtitle(bquote(atop("Voltage at Time",atop(.(Volt[t,1]),""))))
  g
}
#return g (a ggmap object) with each point (representing each bus) colored according to the 
# frequency at time t
plot_mapfreq <- function(t){

  update_freq(t)
  linesb <- get_busline_freqcov(t)
  linesb$Correlation[is.nan(linesb$Correlation)] <- 1
  if (min(Freq[,-1])<60) {
    f_cols <-c("red","yellow","orange","blue","green")
  } else{
    f_cols <-c("green","blue","red","orange","yellow")
  }
 # f_cols <- ifelse(min(Freq[,-1])<60,c("red","yellow","orange","blue","green"),c("green","blue","red","orange","yellow"))
  #color_vals_freq <- as.numeric(sapply( c((mincovf+0.1),(maxcovf/4),(maxcovf/2),(maxcovf-0.1)), function(N) formatC(signif(N, digits=3), digits=3,format="fg", flag="#")))
  #Below is for the TS dataset
  #color_vals_freq <- as.numeric(sapply( c(mincovf,0.2,0.4,0.6,(maxcovf-0.08)), function(N) formatC(signif(N, digits=3), digits=3,format="fg", flag="#")))
  #Below is for the GMD dataset
  #color_vals_freq <- as.numeric(sapply( c(mincovf,50,100,150,200,(maxcovf-5)), function(N) formatC(signif(N, digits=3), digits=3,format="fg", flag="#")))
  g <- g + geom_segment(data = linesb,aes(y=From.Latitude,yend=To.Latitude,x=From.Longitude,xend=To.Longitude,colour=Correlation,size=0.1),show.legend = FALSE) +
    scale_colour_gradientn("Correlation",colours = c("red","yellow","green"),limits=c(0,1)) +
    geom_point(data = bus_locs, aes(x=Longitude,y=Latitude,fill = Frequency ), size = 4, shape = 21) +
    scale_fill_gradientn("Frequency",colours = f_cols,limits=c(min(Freq[,-1]),max(Freq[,-1]))) +
    theme(legend.position="right",legend.direction="vertical",legend.box="horizontal") +
    ggtitle(bquote(atop("Frequency at Time",atop(.(Freq[t,1]),""))))
  g
}

plot_mapvolt_large <- function(t){
#  if(!exists("mincovf") | !exists("maxcovf")){
#    get_minmax_covfreq()
#  }
#  if(!exists("mincovv") | !exists("maxcovv")){
#   get_minmax_covvolt()
#  }
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
 # if(!exists("mincovf") | !exists("maxcovf")){
  #  get_minmax_covfreq()
#  }
#  if(!exists("mincovv") | !exists("maxcovv")){
#    get_minmax_covvolt()
#  }
  #  g <- ggmap(mapten)+scale_x_continuous(limits = c(-90.6, -81), expand = c(0, 0)) +scale_y_continuous(limits = c(34.5, 37), expand = c(0, 0))
  update_freq(t)
  linesb <- get_busline_freqcov(t)
 # color_vals_freq <- as.numeric(sapply( c((mincovf+0.1),(maxcovf/4),(maxcovf/2),(maxcovf-0.1)), function(N) formatC(signif(N, digits=3), digits=3,format="fg", flag="#")))
  g <- g+ 
    geom_point(data=bus_locs,aes(x=Longitude,y=Latitude, colour=Frequency,group=Sub.Name),size=25,alpha=0.25,shape=16) +
    scale_colour_gradientn("Bus Frequency",colours = c("green","blue","orange","yellow"),limits=c(min(Freq[,-1]),max(Freq[,-1]))) +
    labs(x = "Longitude", y = "Latitude") +
    #geom_segment(data = linesb,aes(y=From.Latitude,yend=To.Latitude,x=From.Longitude,xend=To.Longitude,fill=Variance),show.legend = TRUE) +
    #theme(legend.position="bottom",legend.direction="vertical",legend.box="horizontal") +
    #scale_fill_gradientn("Variance",colours = c("green","blue","red","orange","yellow"),breaks=color_vals_freq,limits=c(0,maxcovf)) +
    #geom_point(data = bus_locs, aes(x=Longitude,y=Latitude,fill = Frequency ), size = 2, shape = 21) +
    #scale_fill_gradientn("Frequency",colours = c("blue","white","red"),limits=c(min(Freq[,-1]),max(Freq[,-1]))) +
    theme(legend.position="right",legend.direction="vertical",legend.box="horizontal") +
    ggtitle(bquote(atop("Frequency at Time",atop(.(Freq[t,1]),""))))
  g
}




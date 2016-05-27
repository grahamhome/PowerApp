library(ggcorrplot)

source("data/import_gmd.R")
import_data()

correlation_names <- function(){
  n <- list(correlation="Correlation",
            plot_corrvolt="Voltage",
            plot_corrfreq="Frequency")
  n
}

update_covmat_freq <- function(time) {
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
update_covmat_volt <- function(time) {
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
#Get the min/max values that will be in the covariance matrix
mincovf <- 1
maxcovf <- 0
for (t in 1:nrow(Freq)) {
  update_covmat_freq(t)
  mincovf <- ifelse(min(Sf[,])<mincovf,min(Sf[,]),mincovf)
  maxcovf <- ifelse(max(Sf[,])>maxcovf,max(Sf[,]),maxcovf)
}
mincovv <- 1
maxcovv <- 0
for (t in 1:nrow(Volt)) {
  update_covmat_volt(t)
  mincovv <- ifelse(min(Sv[,])<mincovv,min(Sv[,]),mincovv)
  maxcovv <- ifelse(max(Sv[,])>maxcovv,max(Sv[,]),maxcovv)
}

#Change the frequency column of bus_locs with the frequencies for a given time 
update_busloc_freq <- function(time){
  tf <- t(Freq[time,-1])
  tf <- cbind(rownames(tf),tf)
  colnames(tf) <- c("Bus.Name","Frequency")
  bus_locs <- merge(subset(bus_locs,select = c("Bus.Num","Bus.Name","Sub.Name","Latitude","Longitude","Voltage")),tf, by="Bus.Name")
  bus_locs$Frequency <- as.numeric(as.character(bus_locs$Frequency))
  assign("bus_locs",bus_locs,envir = .GlobalEnv)
}
#Change the voltage column of bus_locs with the frequencies for a given time
update_busloc_volt <- function(time){
  vf <- t(Volt[time,-1])
  vf <- cbind(rownames(vf),vf)
  colnames(vf) <- c("Bus.Name","Voltage")
  bus_locs <- merge(subset(bus_locs,select = c("Bus.Num","Bus.Name","Sub.Name","Latitude","Longitude","Frequency")),vf, by="Bus.Name")
  bus_locs$Voltage <- as.numeric(as.character(bus_locs$Voltage))
  assign("bus_locs",bus_locs,envir = .GlobalEnv)
} 


plot_corrvolt <- function(t){
  update_busloc_volt(t)
  update_covmat_volt(t)
  ggcorrplot(cov2cor(Sv),tl.cex = 4.5,colors = c("blue","red","white"),title = bquote(atop("Correlation of Voltage at Time",atop(.(Volt[t,1]),""))))
}

plot_corrfreq <- function(t){
  update_busloc_freq(t)
  update_covmat_freq(t)
  ggcorrplot(cov2cor(Sf),tl.cex = 4.5,colors = c("blue","red","white"),title = bquote(atop("Correlation of Frequency at Time",atop(.(Freq[t,1]),""))))
}

for (t in 10:50) {
  print(plot_corrfreq(1))
  Sys.sleep(0.1)
}



#Note: This plotting method is no longer in use. Correlation data can be viewed in the Map plot.
library(ggplot2)
library(corrplot)
#Function that returns a list that maps the plot functions with the name we want for the display
fnames <- function(){
  n <- list(Correlation="correlation",
            Voltage="plot_corrvolt",
            Frequency="plot_corrfreq")
  if (exists("Pangle")) {
    n <- c(n,Angle="plot_corrpangle")
  }
  n
}
#update the covariance matrix of the frequency values, starting from whatever the previous time (curr_sv)
# was to whatever "time" is. 
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
#update the covariance matrix of the voltage values, starting from whatever the previous time (curr_sv)
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

#update the covariance matrix of the angle values, starting from whatever the previous time (curr_sv)
# was to whatever "time" is. 
update_covmat_pangle <- function(time) {
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


#Change the frequency column of bus_locs with the frequencies for a given time then returns the new data frame
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
#Change the voltage column of bus_locs with the voltages for a given time then returns the new data frame
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
#Change the angle column of bus_locs with the angles for a given time then returns the new data frame
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

#Plot the correlation plot of the angle for all of the buses at time <t>
plot_corrpangle <- function(t){
  bus_locs <- update_pangle(t)
  update_covmat_pangle(t)
  par(oma=c(2,0,1,0))
  corrplot(cov2cor(Sa),tl.cex = 0.37,na.label.col = "white",addgrid.col = NA,
           title =paste("Correlation of Phase Angle at Time",Pangle[t,1],sep = " "),
           mar=c(3,0,4,0))
  #title = bquote(atop("Correlation of Voltage at Time",atop(.(Volt[t,1]),""))))
}
#Plot the correlation plot of the voltage for all of the buses at time <t>
plot_corrvolt <- function(t){
  bus_locs <- update_volt(t)
  update_covmat_volt(t)
  par(oma=c(2,0,1,0))
  corrplot(cov2cor(Sv),tl.cex = 0.37,na.label.col = "white",addgrid.col = NA,
           title =paste("Correlation of Voltage at Time",Volt[t,1],sep = " "),
           mar=c(3,0,4,0))
           #title = bquote(atop("Correlation of Voltage at Time",atop(.(Volt[t,1]),""))))
}
#Plot the correlation plot of the frequency for all of the buses at time <t>
plot_corrfreq <- function(t){
  bus_locs <- update_freq(t)
  update_covmat_freq(t)
  par(oma=c(2,0,1,0))
  corrplot(cov2cor(Sf),tl.cex = 0.37,na.label.col = "white",addgrid.col = NA,
           mar=c(3,0,4,0),
           title = paste("Correlation of Frequency at Time",Volt[t,1],sep = " "))
}






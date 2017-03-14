
#Function that returns a list that maps the plot functions with the name we want for the display
fnames <- function(){
  n <- list(Linear="linear",
            Voltage="plot_voltage",
            Frequency="plot_frequency")
  if (exists("Pangle")) {
    n <- c(n,Angle="plot_pangle")
  }
  n
}

#Function that updates the dataframe containing the rate of change data for the phase angles
update_anglediff <- function(time){
  if(currt_anglediff < time && currt_anglediff >= 1){
    ta <- currt_anglediff
  }
  else{
    Ca <- Pangle[1,]
    ta <- 1
  }
  for (n in ta:time) {
    xt <- Da[n,]
    if(n==1){
      xt1 <- Da[1,]
    } else{
      xt1 <- Da[n-1,]
    }
    numerator <- ifelse((xt-1)==0,xt,(xt-1))
    Ca[n,-1] <- (xt-xt1)/numerator
    Ca[n,1] <- Pangle[n,1]
  }
  assign("currt_anglediff",time,envir = .GlobalEnv)
  assign("Ca",Ca,envir = .GlobalEnv)
}
#Function that updates the dataframe containing the rate of change data for the frequency
update_freqdiff <- function(time){
  if(currt_freqdiff < time && currt_freqdiff >= 1){
    tf <- currt_freqdiff
  }
  else{
    Cf <- Freq[1,]
    tf <- 1
  }
  for (n in tf:time) {
    xt <- Df[n,]
    if(n==1){
      xt1 <- Df[1,]
    } else{
      xt1 <- Df[n-1,]
    }
    numerator <- ifelse((xt-1)==0,xt,(xt-1))
    Cf[n,-1] <- (xt-xt1)/numerator
    Cf[n,1] <- Freq[n,1]
  }
  assign("currt_freqdiff",time,envir = .GlobalEnv)
  assign("Cf",Cf,envir = .GlobalEnv)
}
#Function that updates the dataframe containing the rate of change data for the voltage
update_voltdiff <- function(time){
  if(currt_voltdiff < time && currt_voltdiff >= 1){
    tv <- currt_voltdiff
  }
  else{
    Cv <- Volt[1,]
    tv <- 1
  }
  for (n in tv:time) {
    xt <- Dv[n,]
    if(n==1){
      xt1 <- Dv[1,]
    } else{
      xt1 <- Dv[n-1,]
    }
    numerator <- ifelse((xt-1)==0,xt,(xt-1))
    Cv[n,-1] <- (xt-xt1)/numerator
    Cv[n,1] <- Volt[n,1]
  }
  assign("currt_voltdiff",time,envir = .GlobalEnv)
  assign("Cv",Cv,envir = .GlobalEnv)
}

#Display the rate of chage in the phase angles
plot_angle_diff <- function(start,stop){
  par(mar=c(5.1, 4.1, 4.1, 4.1), xpd=TRUE)
  update_anglediff(stop)
  xrange <- range(Pangle[start:stop,1])#,na.rm = TRUE)
  yrange <- range(Ca[start:stop,-1])#,na.rm = TRUE)
  plot(xrange,yrange,type = "n",xlab = "Time (seconds)",ylab = "Rate of Change in Phase Angle")
  n <- ncol(Pangle)-1
  colors <- rainbow(n)
  linetype <- c(1:n)
  plotchar <- colnames(Pangle[,-1])
  for (z in 2:n){
    bus <- Ca[start:stop,z]
    lines(y=bus,x=Ca[start:stop,1],col=colors[z],type = "l",lty=linetype[z])
  }
  # legend("topright", inset=c(-0.2,-0.15),legend = colnames(Volt[,-1]),col = colors, lty=linetype,cex=0.8)
}
#Display the rate of chage in the frequency
plot_freq_diff <- function(start,stop){
  par(mar=c(5.1, 4.1, 4.1, 4.1), xpd=TRUE)
  update_freqdiff(stop)
  xrange <- range(Freq[start:stop,1])#,na.rm = TRUE)
  yrange <- range(Cf[start:stop,-1])#,na.rm = TRUE)
  plot(xrange,yrange,type = "n",xlab = "Time (seconds)",ylab = "Rate of Change in Frequency")
  n <- ncol(Freq)-1
  colors <- rainbow(n)
  linetype <- c(1:n)
  plotchar <- colnames(Freq[,-1])
  for (z in 2:n){
    bus <- Cf[start:stop,z]
    lines(y=bus,x=Cf[start:stop,1],col=colors[z],type = "l",lty=linetype[z])
  }
  # legend("topright", inset=c(-0.2,-0.15),legend = colnames(Volt[,-1]),col = colors, lty=linetype,cex=0.8)
}
#Display the rate of chage in the voltage
plot_voltage_diff <- function(start,stop){
  par(mar=c(5.1, 4.1, 4.1, 4.1), xpd=TRUE)
  update_voltdiff(stop)
  xrange <- range(Volt[start:stop,1])#,na.rm = TRUE)
  yrange <- range(Cv[start:stop,-1])#,na.rm = TRUE)
  plot(xrange,yrange,type = "n",xlab = "Time (seconds)",ylab = "Rate of Change in Voltage")
  n <- ncol(Volt)-1
  colors <- rainbow(n)
  linetype <- c(1:n)
  plotchar <- colnames(Volt[,-1])
  for (z in 2:n){
    bus <- Cv[start:stop,z]
    lines(y=bus,x=Cv[start:stop,1],col=colors[z],type = "l",lty=linetype[z])
  }
  # legend("topright", inset=c(-0.2,-0.15),legend = colnames(Volt[,-1]),col = colors, lty=linetype,cex=0.8)
}


#Plot the bus angles of all buses from <start> to <stop>
plot_pangle <- function(start,stop){
  #Vmelt <- melt(Volt[1:1000,], id="Time")
  #p <- ggplot(Vmelt,aes(x=Time,y=value,colour=variable,group=variable)) +
  #  geom_line() +
  #  theme(legend.position="right")
  #p
  par(mar=c(5.1, 4.1, 4.1, 14.1), xpd=TRUE)
  #start <- ifelse(missing(start),1,start)
  #stop <- ifelse(missing(stop),nrow(Volt),stop)
  xrange <- range(Pangle[start:stop,1])#,na.rm = TRUE)
  yrange <- range(Pangle[start:stop,-1])#,na.rm = TRUE) #c(-50,50) 
  plot(xrange,yrange,type = "n",xlab = "Time (seconds)",ylab = "Phase Angle")#,yaxt="n")
     #  main=paste("Phase Angle at time",start,"to",stop,sep = " "),yaxt="n")
 # axis(2, at = seq(-50, 50, by = 10), las=2)
  num_sig_bus <- 0
  n <- ncol(Pangle)-1
  colors <- rainbow(n)
  linetype <- c(1:n)
  plotchar <- colnames(Pangle[,-1])
  for (z in 2:n){
    bus <- Pangle[start:stop,z]
    lines(y=bus,x=Pangle[start:stop,1],col=colors[z],type = "l",lty=linetype[z])
  }
 # legend("right", inset=c(-0.2,-0.15),legend = colnames(Pangle[,-1]),col = colors, lty=linetype,cex=0.8)
}

#plot the a line graph of each bus from {start} to {stop} of the voltage
plot_voltage <- function(start,stop){
  #Vmelt <- melt(Volt[1:1000,], id="Time")
  #p <- ggplot(Vmelt,aes(x=Time,y=value,colour=variable,group=variable)) +
  #  geom_line() +
  #  theme(legend.position="right")
  #p
  par(mar=c(5.1, 4.1, 4.1, 14.1), xpd=TRUE)
  #start <- ifelse(missing(start),1,start)
  #stop <- ifelse(missing(stop),nrow(Volt),stop)
  xrange <- range(Volt[start:stop,1])#,na.rm = TRUE)
  yrange <- range(Volt[start:stop,-1])#,na.rm = TRUE)
 # yrange[1] <- ifelse(yrange[1]>0.8,0.8,yrange[1])
 # yrange[2] <- ifelse(yrange[2]<1.2,1.2,yrange[2])
  plot(xrange,yrange,type = "n",xlab = "Time (seconds)",ylab = "Voltage")
      #main=paste("Voltage at time",start,"to",stop,sep = " "))
  num_sig_bus <- 0
  n <- ncol(Volt)-1
  colors <- rainbow(n)
  linetype <- c(1:n)
  plotchar <- colnames(Volt[,-1])
  for (z in 2:n){
    bus <- Volt[start:stop,z]
    lines(y=bus,x=Volt[start:stop,1],col=colors[z],type = "l",lty=linetype[z])
  }
 # legend("topright", inset=c(-0.2,-0.15),legend = colnames(Volt[,-1]),col = colors, lty=linetype,cex=0.8)
}
#plot the a line graph of each bus from {start} to {stop} of the voltage
plot_frequency <- function(start,stop){
  par(mar=c(5.1, 4.1, 4.1, 14.1), xpd=TRUE)
  #start <- ifelse(missing(start),1,start)
  #stop <- ifelse(missing(stop),nrow(Freq),stop)
  xrange <- range(Freq[start:stop,1])#,na.rm = TRUE)
  yrange <- range(Freq[start:stop,-1])#,na.rm = TRUE)
#  yrange[1] <- ifelse(yrange[1]>58,58,yrange[1])
#  yrange[2] <- ifelse(yrange[2]<62,62,yrange[2])
  plot(xrange,yrange,type = "n",xlab = "Time (seconds)",ylab = "Frequency")
      # main=paste("Frequency at time",Freq[start,1],"to",Freq[stop,1],sep = " "))
  num_sig_bus <- 0
  n <- ncol(Freq)-1
  colors <- rainbow(n)
  linetype <- c(1:n)
  plotchar <- colnames(Freq[,-1])
  for (z in 2:n){
    bus <- Freq[start:stop,z]
    lines(y=bus,x=Freq[start:stop,1],col=colors[z],type = "l",lty=linetype[z])
  }
 # legend("topright", inset=c(-.30,-0.15),legend = colnames(Freq[,-1]),col = colors, lty=linetype,cex=0.8)
}




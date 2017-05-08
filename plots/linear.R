
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




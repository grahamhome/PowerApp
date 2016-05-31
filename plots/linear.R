fnames <- function(){
  n <- list(Linear="linear",
            Voltage="plot_voltage",
            Frequency="plot_frequency")
  n
}

#plot the a line graph of each bus from {start} to {stop} of the voltage
plot_voltage <- function(start,stop){
  # Vmelt <- melt(V[1:10,], id="Time")
  # p <- ggplot(Vmelt,aes(x=Time,y=value,colour=variable,group=variable)) +
  #   geom_line() +
  #    theme(legend.position="none")
  par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
  start <- ifelse(missing(start),1,start)
  stop <- ifelse(missing(stop),nrow(Volt),stop)
  xrange <- range(Volt[start:stop,1])
  yrange <- range(Volt[start:stop,-1])
  plot(xrange,yrange,type = "n",xlab = "Time (seconds)",ylab = "Voltage")
  num_sig_bus <- 0
  n <- ncol(Volt)-1
  colors <- rainbow(n)
  linetype <- c(1:n)
  plotchar <- colnames(Volt[,-1])
  for (z in 2:n){
    bus <- Volt[start:stop,z]
    lines(y=bus,x=Volt[start:stop,1],col=colors[z],type = "l",lty=linetype[z])
  }
  title(bquote(atop("Voltage at Time",atop(.(Volt[t,1]),""))))
  legend("topright", inset=c(-0.2,-0.15),legend = 1:n,col = colors, lty=linetype,cex=0.8)
}
#plot the a line graph of each bus from {start} to {stop} of the voltage
plot_frequency <- function(start,stop){
  par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
  start <- ifelse(missing(start),1,start)
  stop <- ifelse(missing(stop),nrow(Freq),stop)
  xrange <- range(Freq[start:stop,1])
  yrange <- range(Freq[start:stop,-1])
  plot(xrange,yrange,type = "n",xlab = "Time (seconds)",ylab = "Frequency")
  num_sig_bus <- 0
  n <- ncol(Freq)-1
  colors <- rainbow(n)
  linetype <- c(1:n)
  plotchar <- colnames(Freq[,-1])
  for (z in 2:n){
    bus <- Freq[start:stop,z]
    lines(y=bus,x=Freq[start:stop,1],col=colors[z],type = "l",lty=linetype[z])
  }
  title(bquote(atop("Frequency at Time",atop(.(Freq[t,1]),""))))
  legend("topright", inset=c(-0.2,-0.15),legend = 1:n,col = colors, lty=linetype,cex=0.8)
}
library(ggplot2)
library(reshape)
fnames <- function(){
  n <- list(Boxplot="boxplot",
            Voltage="plot_boxvolt",
            Frequency="plot_boxfreq")
  n
}


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


plot_boxfreq <- function(t){
  update_freq(t)
  cf <- Freq[1:t,]
  p <- ggplot(data=cf,aes(x=Time))+
    geom_boxplot()+
    theme(legend.position="right",legend.direction="vertical",legend.box="horizontal") +
    ggtitle(bquote(atop("Frequency at Time",atop(.(Freq[t,1]),""))))
  p
}

plot_boxvolt <- function(t){
  update_volt(t)
  cv <- melt(Volt[1:t,],id="Time")
  p <- ggplot(data=cv,aes(x=variable,y=value))+
    geom_boxplot(aes(group=variable))+
    theme(legend.position="none",legend.direction="vertical",legend.box="horizontal") +
    ggtitle(bquote(atop("Voltage at Time",atop(.(Volt[t,1]),""))))
  p
}




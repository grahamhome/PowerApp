library(ggplot2)
library(reshape)
library(data.table)
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


plot_boxfreq <- function(time){
  update_freq(time)
  cf <- as.data.frame(t(Freq[time,-1]))
  cf$group <- 0
  colnames(cf) <- c("Frequency","group")
  p <- ggplot(data=cf,aes(group,Frequency))+
    geom_boxplot()+
    ylim(min(Freq[,-1]),max(Freq[,-1]))+
    theme(legend.position="right",legend.direction="vertical",legend.box="horizontal") +
    ggtitle(bquote(atop("Frequency at Time",atop(.(Freq[time,1]),""))))
  p
}

plot_boxvolt <- function(time){
  update_volt(time)
  #cv <- melt(Volt[1:time,],id="Time")
  cv <- as.data.frame(t(Volt[time,-1]))
  cv$group <- 0
  colnames(cv) <- c("Voltage","group")
  p <- ggplot(data=cv,aes(group,Voltage))+
    geom_boxplot()+
    ylim(min(Volt[,-1]),max(Volt[,-1]))+
    theme(legend.position="none",legend.direction="vertical",legend.box="horizontal") +
    ggtitle(bquote(atop("Voltage at Time",atop(.(Volt[time,1]),""))))
  p
}




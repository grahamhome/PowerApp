library(ggplot2)

#Function that returns a list that maps the plot functions with the name we want for the display
fnames <- function(){
  n <- list(Histogram="histogram",
            Voltage="plot_histvolt",
            Frequency="plot_histfreq")
  n
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

#plot a histogram of the voltages for all of the buses at <time>
plot_histvolt <- function(time){
  bus_locs <- update_volt(time)
  b <- subset(bus_locs, select = c("Bus.Name","Voltage"))
 # b$group<-0
 # b$group[b$Voltage >1] <- 1
 # b$group[b$Voltage <1] <- -1
  p <- ggplot(b, aes(x=Voltage)) +
    geom_histogram(binwidth = 0.01,aes(colour="black"),show.legend = FALSE)+
   # geom_histogram(stat = "bin",position='identity')+
    #scale_fill_gradient2(low="red",mid = 'black',high = 'blue',midpoint = 0)+
    theme(axis.text.x=element_text(angle=-90, vjust=0.5,size = 14))+
    #ylim((min(b$Voltage)-1),(max(b$Voltage)-1))+
    ggtitle(bquote(atop("Voltage at Time",atop(.(Volt[time,1]),""))))
  xmn <- 0
  if (min(b$Frequency)<xmn) {
    xmn <- min(b$Frequency)
  }
  xmx <- 2
  if (max(b$Frequency)>xmx){
    xmx <- max(b$Frequency)
  }
  p <- p+xlim(xmn,xmx)
  p
}

#plot a histogram of the frequency for all of the buses at <time>
plot_histfreq <- function(time){
  bus_locs <- update_freq(time)
  b <- subset(bus_locs, select = c("Bus.Name","Frequency"))
  # b$group<-0
  # b$group[b$Voltage >1] <- 1
  # b$group[b$Voltage <1] <- -1
  p <- ggplot(b, aes(x=Frequency)) +
    geom_histogram(binwidth = 0.01,aes(colour="black"),show.legend = FALSE)+
    #stat_bin(aes(y=..count.., label=..count..), geom="text", vjust=-.5)+
    
    # geom_histogram(stat = "bin",position='identity')+
    #scale_fill_gradient2(low="red",mid = 'black',high = 'blue',midpoint = 0)+
    theme(axis.text.x=element_text(angle=-90, vjust=0.5,size = 14))+
    #ylim((min(b$Voltage)-1),(max(b$Voltage)-1))+
    ggtitle(bquote(atop("Frequency at Time",atop(.(Freq[time,1]),""))))
  xmn <- 59
  if (min(b$Frequency)<xmn) {
    xmn <- min(b$Frequency)
  }
  xmx <- 61
  if (max(b$Frequency)>xmx){
    xmx <- max(b$Frequency)
  }
  p <- p+xlim(xmn,xmx)
  p
}



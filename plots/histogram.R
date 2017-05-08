library(ggplot2)

#Function that returns a list that maps the plot functions with the name we want for the display
fnames <- function(){
  n <- list(Histogram="histogram",
            Voltage="plot_histvolt",
            Frequency="plot_histfreq")
  if (exists("Pangle")) {
    n <- c(n,Angle="plot_histpangle")
  }
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

autoscale <- function(){
  if(!exists("autosc")){
    autosc <<- FALSE
  }
  if(autosc == TRUE){
    autosc <<-FALSE
  } else{
    autosc <<-TRUE
  }
}


#plot a histogram of the voltages for all of the buses at <time>
plot_histvolt <- function(time){
  bus_locs <- update_volt(time)
  b <- subset(bus_locs, select = c("Bus.Name","Voltage"))
  y_size <- nsamples()
  if(autosc==TRUE){
    y_size <- max(hist.default(b$Voltage,plot = FALSE)$counts)
    y_size <- ifelse(y_size >nsamples(), nsamples(),y_size)
  }
 # b$group<-0
 # b$group[b$Voltage >1] <- 1
 # b$group[b$Voltage <1] <- -1
  p <- ggplot(b, aes(x=Voltage)) +
    geom_histogram(binwidth = 0.01,aes(colour="black"),show.legend = FALSE)+
   # geom_histogram(stat = "bin",position='identity')+
    #scale_fill_gradient2(low="red",mid = 'black',high = 'blue',midpoint = 0)+
    theme(axis.text.x=element_text(angle=-90, vjust=0.5,size = 14))+
    ylim(0,y_size)+
    ggtitle(bquote(atop("Voltage at Time",atop(.(Volt[time,1]),""))))+
    xlim(0,2)
  p
}

#plot a histogram of the frequency for all of the buses at <time>
plot_histpangle <- function(time){
  bus_locs <- update_pangle(time)
  b <- subset(bus_locs, select = c("Bus.Name","Angle"))
  y_size <- nsamples()
  if(autosc==TRUE){
    y_size <- max(hist.default(b$Angle,plot = FALSE)$counts)
    y_size <- ifelse(y_size >nsamples(), nsamples(),y_size)
    }
  # b$group<-0
  # b$group[b$Voltage >1] <- 1
  # b$group[b$Voltage <1] <- -1
  p <- ggplot(b, aes(x=Angle)) +
    geom_histogram(binwidth = 1,aes(colour="black"),show.legend = FALSE)+
    #stat_bin(aes(y=..count.., label=..count..), geom="text", vjust=-.5)+
    # geom_histogram(stat = "bin",position='identity')+
    #scale_fill_gradient2(low="red",mid = 'black',high = 'blue',midpoint = 0)+
    theme(axis.text.x=element_text(vjust=0.5,size = 14))+
    ylim(0,y_size)+
    ggtitle(bquote(atop("Angle at Time",atop(.(Pangle[time,1]),""))))+
    xlim(-90,90)+
    ylim(0,ncol(Pangle))
  p
}


#plot a histogram of the frequency for all of the buses at <time>
plot_histfreq <- function(time){
  bus_locs <- update_freq(time)
  b <- subset(bus_locs, select = c("Bus.Name","Frequency"))
  y_size <- nsamples()
  if(autosc==TRUE){
    y_size <- max(hist.default(b$Frequency,plot = FALSE)$counts)
    y_size <- ifelse(y_size >nsamples(), nsamples(),y_size)
  }
  # b$group<-0
  # b$group[b$Voltage >1] <- 1
  # b$group[b$Voltage <1] <- -1
  p <- ggplot(b, aes(x=Frequency)) +
    geom_histogram(binwidth = 0.01,aes(colour="black"),show.legend = FALSE)+
    #stat_bin(aes(y=..count.., label=..count..), geom="text", vjust=-.5)+
    
    # geom_histogram(stat = "bin",position='identity')+
    #scale_fill_gradient2(low="red",mid = 'black',high = 'blue',midpoint = 0)+
    theme(axis.text.x=element_text(angle=-90, vjust=0.5,size = 14))+
    ylim(0,y_size)+
    ggtitle(bquote(atop("Frequency at Time",atop(.(Freq[time,1]),""))))+
    xlim(59,61)
  p
}



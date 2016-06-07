library(ggplot2)


fnames <- function(){
  n <- list("Bar Graph"="bar",
            Voltage="plot_barvolt",
            Frequency="plot_barfreq")
  if (exists("Pangle")) {
    n <- c(n,
           Angle="plot_barpangle")
  }
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
update_pangle <- function(time){
  ta <- t(Pangle[time,-1])
  ta <- cbind(rownames(ta),ta)
  colnames(ta) <- c("Bus.Name","Angle")
  bus_locs <- merge(subset(bus_locs,select = c("Bus.Num","Bus.Name","Sub.Name","Latitude","Longitude","Voltage","Frequency")),ta, by="Bus.Name")
  bus_locs$Angle <- as.numeric(as.character(bus_locs$Angle))
  assign("bus_locs",bus_locs,envir = .GlobalEnv)
}


plot_barpangle <- function(time){
  update_pangle(time)
  b <- subset(bus_locs, select = c("Bus.Name","Angle"))
  b$group<-0
  b$group[b$Angle >0] <- 1
  b$group[b$Angle <0] <- -1
  p <- ggplot(b, aes(x=Bus.Name,y=Angle,fill=factor(group))) +
    geom_bar(stat = "identity",position='identity')+
    #scale_fill_gradient2(low="red",mid = 'black',high = 'blue',midpoint = 0)+
    theme(axis.text.x=element_text(angle=-90, vjust=0.5,size = 4))+
    #ylim((min(b$Voltage)-1),(max(b$Voltage)-1))+
    ylim(-90,90)+
    ylab("Angle")+
    ggtitle(bquote(atop("Angle at Time",atop(.(Pangle[time,1]),""))))
  if(unique(b$group)==0){
    p<- p+ scale_fill_manual(values=c("0"="grey50"),drop=FALSE, 
                             labels=c("Angle=0"),
                             name="") 
  } else{
    p<- p+ scale_fill_manual(values=c("-1"="blue","1"="red","0"="grey50"),
                             labels=c("Angle < 0",
                                      "Angle > 0","Angle=0"),
                             name="") 
  }
  p
}

plot_barvolt <- function(time){
  update_volt(time)
  b <- subset(bus_locs, select = c("Bus.Name","Voltage"))
  b$group<-0
  b$group[b$Voltage >1] <- 1
  b$group[b$Voltage <1] <- -1
  p <- ggplot(b, aes(x=Bus.Name,y=(Voltage-1),fill=factor(group))) +
    geom_bar(stat = "identity",position='identity')+
    #scale_fill_gradient2(low="red",mid = 'black',high = 'blue',midpoint = 0)+
    theme(axis.text.x=element_text(angle=-90, vjust=0.5,size = 4))+
    #ylim((min(b$Voltage)-1),(max(b$Voltage)-1))+
    ylim(-1,1)+
    ylab("Voltage")+
    ggtitle(bquote(atop("Voltage at Time",atop(.(Volt[time,1]),""))))
  if(unique(b$group)==0){
    p<- p+ scale_fill_manual(values=c("0"="grey50"),drop=FALSE, 
                             labels=c("Voltage=1"),
                             name="") 
  } else{
    p<- p+ scale_fill_manual(values=c("-1"="blue","1"="red","0"="grey50"),
                             labels=c("Voltage < 1",
                                      "Voltage > 1","Voltage=1"),
                             name="") 
  }
  p
}
plot_barfreq <- function(time){
  update_freq(time)
  b <- subset(bus_locs,select = c("Bus.Name","Frequency"))
  b$group<-0
  b$group[b$Frequency >60] <- 1
  b$group[b$Frequency <60] <- -1
  p <- ggplot(b, aes(x=Bus.Name,y=(Frequency-60),fill=factor(group))) +
    geom_bar(stat = "identity",position='identity')+
    theme(axis.text.x=element_text(angle=-90, vjust=0.5,size = 4))+
    #ylim((min(b$Frequency)-60),(max(b$Frequency)-60))+
    ylim(-1,1)+
    ylab("Frequency difference from 60")+
    ggtitle(bquote(atop("Frequency at Time",atop(.(Freq[time,1]),""))))
  if(unique(b$group)==0){
    p<- p+ scale_fill_manual(values=c("0"="grey50"),
                             labels=c("Frequency=60"),
                             name="") 
  } else{
    p<- p+ scale_fill_manual(values=c("-1"="blue","1"="red","0"="grey50"),drop=FALSE, 
                             labels=c("Frequency > 60",
                                      "Frequency < 60","Frequency=60"),
                             name="") 
  }
  
  p
}


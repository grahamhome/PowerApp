library(ggplot2)

#Function that returns a list that maps the plot functions with the name we want for the display
fnames <- function(){
  n <- list("Bar Graph"="bar",
            Voltage="plot_barvolt",
            Frequency="plot_barfreq")
  if (exists("Pangle")) {
    n <- c(n,Angle="plot_barpangle")
  }
  n
}

#Change the frequency column of bus_locs with the frequencies for a given time then returns the new matrix
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
#Change the voltage column of bus_locs with the voltages for a given time then returns the new matrix
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
#Change the angle column of bus_locs with the angles for a given time then returns the new matrix
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

plot_barpangle <- function(time){
  if(!exists("autosc")){
    autosc <<- FALSE
  }
  bus_locs <- update_pangle(time)
  b <- subset(bus_locs, select = c("Bus.Name","Angle"))
  b$group<-0
  b$group[b$Angle >0] <- 1
  b$group[b$Angle <0] <- -1
  if(autosc == TRUE){
    amin <- min(b$Angle)
    amax <- max(b$Angle)
    #amin <- ifelse(min(b$Angle)<-40,min(b$Angle),-40)
  #  amax <- ifelse(max(b$Angle)>40,max(b$Angle),40)
    adiff <- (amax-amin)
    a_lab <- c(amin,(amin+(adiff/4)),(amin+(adiff/2)),(amax-(adiff/4)),amax)
  } else{
    amin <- -40
    amax <- 40
    a_lab <- c(-40,-20,0,20,40)
  }
  p <- ggplot(b, aes(x=Bus.Name,y=Angle,fill=factor(group))) +
    geom_bar(stat = "identity",position='identity')+
    #scale_fill_gradient2(low="red",mid = 'black',high = 'blue',midpoint = 0)+
    theme(axis.text.x=element_text(angle=-90, vjust=0.5,size = 4))+
    #ylim((min(b$Voltage)-1),(max(b$Voltage)-1))+
    ylim(amin,amax)+
    ylab("Angle")+
    ggtitle(bquote(atop("Angle at Time",atop(.(Pangle[time,1]),""))))
  if(unique(b$group)==0 & length(unique(b$group))==1){
    bar_vals <- c("0"="grey50")
    bar_labs <- c("Angle = 0")
  } else if(unique(b$group)==1 & length(unique(b$group))==1){
    bar_vals <- c("1"="red")
    bar_labs <- c("Angle > 0")
  } else if(unique(b$group)==-1 & length(unique(b$group))==1){
    bar_vals <- c("-1"="blue")
    bar_labs <- c("Angle < 0")
  }  else if((unique(b$group) ==c(-1,1) | unique(b$group) ==c(1,-1)) & length(unique(b$group))==2){
    bar_vals <- c("-1"="blue","1"="red")
    bar_labs <- c("Angle < 0", "Angle > 0")
  } else if((unique(b$group) ==c(-1,0) | unique(b$group) ==c(0,-1)) & length(unique(b$group))==2){
    bar_vals <- c("-1"="blue","0"="grey50")
    bar_labs <- c("Angle < 0", "Angle = 0")
  } else if((unique(b$group) ==c(0,1) | unique(b$group) ==c(1,0))  & length(unique(b$group))==2 ){
    bar_vals <- c("0"="grey50","1"="red")
    bar_labs <- c("Angle = 0", "Angle > 0")
  } else{
    bar_vals <- c("-1"="blue","0"="grey50","1"="red")
    bar_labs <- c("Angle < 0","Angle = 0", "Angle > 0")
  }
  p<- p+ scale_fill_manual(values=bar_vals,
                           labels=bar_labs,
                           name="")
  p
}

plot_barvolt <- function(time){
  if(!exists("autosc")){
    autosc <<- FALSE
  }
  bus_locs <- update_volt(time)
  b <- subset(bus_locs, select = c("Bus.Name","Voltage"))
  b$group<-0
  b$group[b$Voltage >1] <- 1
  b$group[b$Voltage <1] <- -1
  if(autosc == TRUE){
    vmin <- min(b$Voltage)
    vmax <- max(b$Voltage)
  #  vmin <- ifelse(min(b$Voltage)<0.8,min(b$Voltage),0.8)
  #  vmax <- ifelse(max(b$Voltage)>1.2,max(b$Voltage),1.2)
    vdiff <- (vmax-vmin)
    v_lab <- c(vmin,(vmin+(vdiff/4)),(vmin+(vdiff/2)),(vmax-(vdiff/4)),vmax)
  } else{
    vmin <- 0.8
    vmax <- 1.2
    v_lab <-c(0.8,0.9,1,1.1,1.2) #c(-0.2,-0.1,0,0.1,0.2)#
  }
  p <- ggplot(b, aes(x=Bus.Name,y=(Voltage-1),fill=factor(group))) +
    geom_bar(stat = "identity",position='identity')+
    #scale_fill_gradient2(low="red",mid = 'black',high = 'blue',midpoint = 0)+
    theme(axis.text.x=element_text(angle=-90, vjust=0.5,size = 4))+
    #ylim((min(b$Voltage)-1),(max(b$Voltage)-1))+
    #ylim(-1,1)+
    scale_y_continuous("Voltage", labels=v_lab, limits=c((vmin-1),(vmax-1)))+
    ylab("Voltage")+
    ggtitle(bquote(atop("Voltage at Time",atop(.(Volt[time,1]),""))))
  if(unique(b$group)==0 & length(unique(b$group))==1){
    bar_vals <- c("0"="grey50")
    bar_labs <- c("Voltage = 1")
  } else if(unique(b$group)==1 & length(unique(b$group))==1){
    bar_vals <- c("1"="red")
    bar_labs <- c("Voltage > 1")
  } else if(unique(b$group)==-1 & length(unique(b$group))==1){
    bar_vals <- c("-1"="blue")
    bar_labs <- c("Voltage < 1")
  } else if((unique(b$group) ==c(-1,1) | unique(b$group) ==c(1,-1)) & length(unique(b$group))==2){
    bar_vals <- c("-1"="blue","1"="red")
    bar_labs <- c("Voltage < 1", "Voltage > 1")
  } else if((unique(b$group) ==c(-1,0)| unique(b$group) ==c(0,-1)) & length(unique(b$group))==2){
    bar_vals <- c("-1"="blue","0"="grey50")
    bar_labs <- c("Voltage < 1", "Voltage = 1")
  } else if((unique(b$group) ==c(0,1) | unique(b$group) ==c(1,0)) & length(unique(b$group))==2){
    bar_vals <- c("0"="grey50","1"="red")
    bar_labs <- c("Voltage = 1", "Voltage > 1")
  } else{
    bar_vals <- c("-1"="blue","0"="grey50","1"="red")
    bar_labs <- c("Voltage < 1","Voltage = 1", "Voltage > 1")
  }
  p<- p+ scale_fill_manual(values=bar_vals,
                           labels=bar_labs,
                           name="")
  p
}
plot_barfreq <- function(time){
  if(!exists("autosc")){
    autosc <<- FALSE
  }
  bus_locs <- update_freq(time)
  b <- subset(bus_locs,select = c("Bus.Name","Frequency"))
  b$group<-0
  b$group[b$Frequency >60] <- 1
  b$group[b$Frequency <60] <- -1
  if(autosc == TRUE){
    fmin <- min(b$Frequency)
    fmax <- max(b$Frequency)
  #  fmin <- ifelse(min(b$Frequency)<59.8,min(b$Frequency),59.8)
  #  fmax <- ifelse(max(b$Frequency)>60.2,max(b$Frequency),60.2)
    fdiff <- fmax-fmin
    f_lab <- c(fmin,(fmin+(fdiff/4)),(fmin+(fdiff/2)),(fmax-(fdiff/4)),fmax)
  } else{
    fmin <- 59.8#-0.2
    fmax <- 60.2#0.2
    f_lab <- c(59.8,59.9,60,60.1,60.2)#c(-0.2,-0.1,0,0.1,0.2)
  }
  
  bar_vals <- NULL
  bar_labs <- NULL
  bar_breaks <- NULL
  if(nrow(b[(b$group == -1),])>0){
    bar_vals <- c(bar_vals,("-1"="blue"))
    bar_labs <- c(bar_labs,"Frequency < 60")
    bar_breaks <- c(bar_breaks, -1)
  }
  if(nrow(b[(b$group == 0),])>0){
    bar_vals <- c(bar_vals,("0"="grey50"))
    bar_labs <- c(bar_labs,"Frequency = 60")
    bar_breaks <- c(bar_breaks, 0)
  }
  if(nrow(b[(b$group == 1),])>0){
    bar_vals <- c(bar_vals,("1"="red"))
    bar_labs <- c(bar_labs,"Frequency > 60")
    bar_breaks <- c(bar_breaks, 1)
  }
  # if(unique(b$group)==0 & length(unique(b$group))==1){
  #   bar_vals <- c("0"="grey50")
  #   bar_labs <- c("Frequency = 60")
  # } else if(unique(b$group)==1 & length(unique(b$group))==1){
  #   bar_vals <- c("1"="red")
  #   bar_labs <- c("Frequency > 60")
  # } else if(unique(b$group)==-1 & length(unique(b$group))==1){
  #   bar_vals <- c("-1"="blue")
  #   bar_labs <- c("Frequency < 60")
  # } else if((unique(b$group) ==c(-1,1) | unique(b$group) ==c(1,-1)) & length(unique(b$group))==2){
  #   bar_vals <- c("-1"="blue","1"="red")
  #   bar_labs <- c("Frequency < 60", "Frequency > 60")
  # } else if((unique(b$group) ==c(-1,0) | unique(b$group) ==c(0,-1)) & length(unique(b$group))==2){
  #   bar_vals <- c("-1"="blue","0"="grey50")
  #   bar_labs <- c("Frequency < 60", "Frequency = 60")
  # } else if((unique(b$group) ==c(0,1) | unique(b$group) ==c(1,0)) & length(unique(b$group))==2){
  #   bar_vals <- c("0"="grey50","1"="red")
  #   bar_labs <- c("Frequency = 60", "Frequency > 60")
  # } else{
  #   bar_vals <- c("-1"="blue","0"="grey50","1"="red")
  #   bar_labs <- c("Frequency < 60","Frequency = 60", "Frequency > 60")
  # }
  p <- ggplot(b, aes(x=Bus.Name,y=(Frequency-60),fill=factor(group))) +
    scale_fill_manual(values=bar_vals,labels=bar_labs)+#,breaks = bar_breaks)+
    geom_bar(stat = "identity",position='identity')+
    theme(axis.text.x=element_text(angle=-90, vjust=0.5,size = 4))+
    #ylim((min(b$Frequency)-60),(max(b$Frequency)-60))+
    #ylim(-1,1)+
    scale_y_continuous("Frequency",labels = f_lab,breaks = (f_lab-60), limits=c((fmin-60),(fmax-60)))+
    # ylab("Frequency difference from 60")+
    ggtitle(bquote(atop("Frequency at Time",atop(.(Freq[time,1]),""))))
  p
}


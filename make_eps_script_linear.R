library(TTR)
library(grid)
library(ggplot2)
library(reshape2)
library("scales")

reverselog_trans <- function(base = exp(1)) {
  trans <- function(x) -log(x, base)
  inv <- function(x) base^(-x)
  trans_new(paste0("reverselog-", format(base)), trans, inv, 
            log_breaks(base = base), 
            domain = c(1e-100, Inf))
}
firstorder_diff <- function(data){
  
  busmat <- apply(data[,!names(data)%in% c("Time")],2,diff)
  return(cbind("Time"=data$Time[-1],busmat))
  
}

firstorder_roc <- function(data){
  
  busmat <- data[,!names(data)%in%c("Time")]
  busmat <- apply(busmat,2,function(x) ROC(x,type="discrete"))
  return(cbind("Time"=data$Time[-1],busmat[-1,]))
  
}

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
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
  par(mar=c(5.1, 4.1, 4.1, 14.1), xpd=TRUE)
  xrange <- range(Volt[start:stop,1])#,na.rm = TRUE)
  yrange <- range(Volt[start:stop,-1])#,na.rm = TRUE)
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
  #par(mar=c(5.1, 4.1, 4.1, 14.1), xpd=TRUE)
  xrange <- range(Freq[start:stop,1])#,na.rm = TRUE)
  yrange <- range(Freq[start:stop,-1])#,na.rm = TRUE)
  n <- ncol(Freq)-1
  colors <- rainbow(n)
  
  fx <- Freq[,1]
  fy <- as.data.frame(Freq[,-1])
  fg <- colnames(fy)
  fmelt <- melt(data = Freq,id.vars = "Time",variable.name = "Bus",value.name = "Frequency")
  
  ggplot(data = fmelt,aes(x=Time,y=Frequency,group=Bus,colour=Bus))+
                                geom_line()+theme(legend.position="none")
  
  plot(xrange,yrange,type = "n",xlab = "Time (seconds)",ylab = "Frequency")
  # main=paste("Frequency at time",Freq[start,1],"to",Freq[stop,1],sep = " "))
  
  linetype <- c(1:n)
  for (z in 2:n){
    bus <- Freq[start:stop,z]
    lines(y=bus,x=Freq[start:stop,1],col=colors[z],type = "l",lty=linetype[z])
  }
  # legend("topright", inset=c(-.30,-0.15),legend = colnames(Freq[,-1]),col = colors, lty=linetype,cex=0.8)
}


#plot the a line graph of each bus from {start} to {stop} of the voltage
plot_frequency_roc <- function(start,stop){
  par(mar=c(5.1, 4.1, 4.1, 14.1), xpd=TRUE)
  xrange <- range(firstroc[start:stop,1])#,na.rm = TRUE)
  yrange <- range(firstroc[start:stop,-1])#,na.rm = TRUE)
  plot(xrange,yrange,type = "n",xlab = "Time (seconds)",ylab = "Frequency")
  # main=paste("Frequency at time",Freq[start,1],"to",Freq[stop,1],sep = " "))
  num_sig_bus <- 0
  n <- ncol(firstroc)-1
  colors <- rainbow(n)
  linetype <- c(1:n)
  plotchar <- colnames(firstroc[,-1])
  for (z in 2:n){
    bus <- firstroc[start:stop,z]
    lines(y=bus,x=firstroc[start:stop,1],col=colors[z],type = "l",lty=linetype[z])
  }
  # legend("topright", inset=c(-.30,-0.15),legend = colnames(Freq[,-1]),col = colors, lty=linetype,cex=0.8)
}

#plot the a line graph of each bus from {start} to {stop} of the voltage
plot_frequency_firstdif <- function(start,stop){
 # par(mar=c(5.1, 4.1, 4.1, 14.1), xpd=TRUE)
  xrange <- range(firstdiff[1:stop,1])#,na.rm = TRUE)
  yrange <- range(firstdiff[1:stop,-1])#,na.rm = TRUE)
  plot(xrange,yrange,type = "n",xlab = "Time (seconds)",ylab = "Frequency")
  # main=paste("Frequency at time",Freq[start,1],"to",Freq[stop,1],sep = " "))
  num_sig_bus <- 0
  n <- ncol(firstdiff)-1
  colors <- rainbow(n)
  linetype <- c(1:n)
  plotchar <- colnames(firstdiff[,-1])
  for (z in 2:n){
    bus <- firstdiff[start:stop,z]
    lines(y=bus,x=firstdiff[start:stop,1],col=colors[z],type = "l",lty=linetype[z])
  }
  # legend("topright", inset=c(-.30,-0.15),legend = colnames(Freq[,-1]),col = colors, lty=linetype,cex=0.8)
}

plot_frequency_seconddif <- function(start,stop){
 # par(mar=c(5.1, 4.1, 4.1, 14.1), xpd=TRUE)
  xrange <- range(seconddiff[start:stop,1])#,na.rm = TRUE)
  yrange <- range(seconddiff[start:stop,-1])#,na.rm = TRUE)
  plot(xrange,yrange,type = "n",xlab = "Time (seconds)",ylab = "Frequency")
  # main=paste("Frequency at time",Freq[start,1],"to",Freq[stop,1],sep = " "))
  num_sig_bus <- 0
  n <- ncol(seconddiff)-1
  colors <- rainbow(n)
  linetype <- c(1:n)
  plotchar <- colnames(seconddiff[,-1])
  for (z in 2:n){
    bus <- seconddiff[start:stop,z]
    lines(y=bus,x=seconddiff[start:stop,1],col=colors[z],type = "l",lty=linetype[z])
  }
  # legend("topright", inset=c(-.30,-0.15),legend = colnames(Freq[,-1]),col = colors, lty=linetype,cex=0.8)
}

make_linear_plots_stacked <- function(datafile){
  source(datafile)
  import_data()
  
  firstroc <<- firstorder_roc(Freq)
  firstdiff <<- firstorder_diff(Freq)
  seconddiff <<- firstorder_diff(as.data.frame(firstdiff))
  filename <- paste(gsub(pattern = "\ ","",name()),"freq",sep = "_")
  
  fmelt <- melt(data = Freq,id.vars = "Time",variable.name = "Bus",value.name = "Frequency")
  frocmelt <- melt(data = as.data.frame(firstroc),id.vars = "Time",variable.name = "Bus",value.name = "Rate Of Change")
  ffdiffmelt <- melt(data = as.data.frame(firstdiff),id.vars = "Time",variable.name = "Bus",value.name = "First Order")
  fsdiffmelt <- melt(data = as.data.frame(seconddiff),id.vars = "Time",variable.name = "Bus",value.name = "Second Order")
  
  fplot <- ggplot(data = fmelt,aes(x=Time,y=Frequency,group=Bus,colour=Bus))+geom_line()+theme(legend.position="none",axis.title.x=element_blank(),
                                                                                               axis.text.x=element_blank(),
                                                                                               axis.ticks.x=element_blank())+
    scale_x_log10()
  frocplot <- ggplot(data = frocmelt,aes(x=Time,y=`Rate Of Change`,group=Bus,colour=Bus))+geom_line()+theme(legend.position="none",axis.title.x=element_blank(),
                                                                                                     axis.text.x=element_blank(),
                                                                                                     axis.ticks.x=element_blank())+
    scale_x_log10()
  ffdiffplot <- ggplot(data = ffdiffmelt,aes(x=Time,y=`First Order`,group=Bus,colour=Bus))+geom_line()+theme(legend.position="none",axis.title.x=element_blank(),
                                                                                                         axis.text.x=element_blank(),
                                                                                                         axis.ticks.x=element_blank())+
    scale_x_log10()
  fsdiffplot <- ggplot(data = fsdiffmelt,aes(x=Time,y=`Second Order`,group=Bus,colour=Bus))+geom_line()+theme(legend.position="none") +
    scale_x_log10()
                                                                                                        # axis.text.y = element_text(angle=70))
  
  postscript(file = paste(filename, '_xlog_stacked.eps', sep=""))
  # print(plot_heatmapfreq_secondod(interestpoint))
  multiplot(fplot,frocplot,ffdiffplot,fsdiffplot)
  dev.off()
  
  
  
  firstroc <<- firstorder_roc(Volt)
  firstdiff <<- firstorder_diff(Volt)
  seconddiff <<- firstorder_diff(as.data.frame(firstdiff))
  filename <- paste(gsub(pattern = "\ ","",name()),"volt",sep = "_")
  
  vmelt <- melt(data = Volt,id.vars = "Time",variable.name = "Bus",value.name = "Voltage")
  vrocmelt <- melt(data = as.data.frame(firstroc),id.vars = "Time",variable.name = "Bus",value.name = "Rate Of Change")
  vfdiffmelt <- melt(data = as.data.frame(firstdiff),id.vars = "Time",variable.name = "Bus",value.name = "First Order")
  vsdiffmelt <- melt(data = as.data.frame(seconddiff),id.vars = "Time",variable.name = "Bus",value.name = "Second Order")
  
  vplot <- ggplot(data = vmelt,aes(x=Time,y=Voltage,group=Bus,colour=Bus))+geom_line()+theme(legend.position="none",axis.title.x=element_blank(),
                                                                                               axis.text.x=element_blank(),
                                                                                               axis.ticks.x=element_blank())+
    scale_x_log10()
  vrocplot <- ggplot(data = vrocmelt,aes(x=Time,y=`Rate Of Change`,group=Bus,colour=Bus))+geom_line()+theme(legend.position="none",axis.title.x=element_blank(),
                                                                                                     axis.text.x=element_blank(),
                                                                                                     axis.ticks.x=element_blank())+
    scale_x_log10()
  vfdiffplot <- ggplot(data = vfdiffmelt,aes(x=Time,y=`First Order`,group=Bus,colour=Bus))+geom_line()+theme(legend.position="none",axis.title.x=element_blank(),
                                                                                                         axis.text.x=element_blank(),
                                                                                                         axis.ticks.x=element_blank())+
    scale_x_log10()
  vsdiffplot <- ggplot(data = vsdiffmelt,aes(x=Time,y=`Second Order`,group=Bus,colour=Bus))+geom_line()+theme(legend.position="none") +
    scale_x_log10()
  # axis.text.y = element_text(angle=70))
  
  postscript(file = paste(filename, '_xlog_stacked.eps', sep=""))
  # print(plot_heatmapfreq_secondod(interestpoint))
  multiplot(vplot,vrocplot,vfdiffplot,vsdiffplot)
  dev.off()
  
  
  firstroc <<- firstorder_roc(Pangle)
  firstdiff <<- firstorder_diff(Pangle)
  seconddiff <<- firstorder_diff(as.data.frame(firstdiff))
  filename <- paste(gsub(pattern = "\ ","",name()),"angle",sep = "_")
  
  amelt <- melt(data = Pangle,id.vars = "Time",variable.name = "Bus",value.name = "Angle")
  arocmelt <- melt(data = as.data.frame(firstroc),id.vars = "Time",variable.name = "Bus",value.name = "Rate Of Change")
  afdiffmelt <- melt(data = as.data.frame(firstdiff),id.vars = "Time",variable.name = "Bus",value.name = "First Order")
  asdiffmelt <- melt(data = as.data.frame(seconddiff),id.vars = "Time",variable.name = "Bus",value.name = "Second Order")
  
  aplot <- ggplot(data = amelt,aes(x=Time,y=Angle,group=Bus,colour=Bus))+geom_line()+theme(legend.position="none",axis.title.x=element_blank(),
                                                                                             axis.text.x=element_blank(),
                                                                                             axis.ticks.x=element_blank())+
    scale_x_log10()
  arocplot <- ggplot(data = arocmelt,aes(x=Time,y=`Rate Of Change`,group=Bus,colour=Bus))+geom_line()+theme(legend.position="none",axis.title.x=element_blank(),
                                                                                                   axis.text.x=element_blank(),
                                                                                                   axis.ticks.x=element_blank())+
    scale_x_log10()
  afdiffplot <- ggplot(data = afdiffmelt,aes(x=Time,y=`First Order`,group=Bus,colour=Bus))+geom_line()+theme(legend.position="none",axis.title.x=element_blank(),
                                                                                                       axis.text.x=element_blank(),
                                                                                                       axis.ticks.x=element_blank())+
    scale_x_log10()
  asdiffplot <- ggplot(data = asdiffmelt,aes(x=Time,y=`Second Order`,group=Bus,colour=Bus))+geom_line()+theme(legend.position="none") +
    scale_x_log10()
  # axis.text.y = element_text(angle=70))
  
  
  postscript(file = paste(filename, '_xlog_stacked.eps', sep=""))
  # print(plot_heatmapfreq_secondod(interestpoint))
  multiplot(aplot,arocplot,afdiffplot,asdiffplot)
  dev.off()
}

make_linear_plots_stacked_mulvar <- function(datafile){
  source(datafile)
  import_data()
  
  filename <- paste(gsub(pattern = "\ ","",name()),"",sep = "_")
  
  firstroc_f <<- firstorder_roc(Freq)
  firstdiff_f <<- firstorder_diff(Freq)
  seconddiff_f <<- firstorder_diff(as.data.frame(firstdiff_f))
  
  firstroc_v <<- firstorder_roc(Volt)
  firstdiff_v <<- firstorder_diff(Volt)
  seconddiff_v <<- firstorder_diff(as.data.frame(firstdiff_v))
  
  firstroc_a <<- firstorder_roc(Pangle)
  firstdiff_a <<- firstorder_diff(Pangle)
  seconddiff_a <<- firstorder_diff(as.data.frame(firstdiff_a))
  
  fmelt <- melt(data = Freq,id.vars = "Time",variable.name = "Bus",value.name = "Frequency")
  frocmelt <- melt(data = as.data.frame(firstroc_f),id.vars = "Time",variable.name = "Bus",value.name = "Frequency Rate Of Change")
  ffdiffmelt <- melt(data = as.data.frame(firstdiff_f),id.vars = "Time",variable.name = "Bus",value.name = "Frequency First Order")
  fsdiffmelt <- melt(data = as.data.frame(seconddiff_f),id.vars = "Time",variable.name = "Bus",value.name = "Frequency Second Order")
  
  
  vmelt <- melt(data = Volt,id.vars = "Time",variable.name = "Bus",value.name = "Voltage")
  vrocmelt <- melt(data = as.data.frame(firstroc_v),id.vars = "Time",variable.name = "Bus",value.name = "Voltage Rate Of Change")
  vfdiffmelt <- melt(data = as.data.frame(firstdiff_v),id.vars = "Time",variable.name = "Bus",value.name = "Voltage First Order")
  vsdiffmelt <- melt(data = as.data.frame(seconddiff_v),id.vars = "Time",variable.name = "Bus",value.name = "Voltage Second Order")
  
  amelt <- melt(data = Pangle,id.vars = "Time",variable.name = "Bus",value.name = "Angle")
  arocmelt <- melt(data = as.data.frame(firstroc_a),id.vars = "Time",variable.name = "Bus",value.name = "Angle Rate Of Change")
  afdiffmelt <- melt(data = as.data.frame(firstdiff_a),id.vars = "Time",variable.name = "Bus",value.name = "Angle First Order")
  asdiffmelt <- melt(data = as.data.frame(seconddiff_a),id.vars = "Time",variable.name = "Bus",value.name = "Angle Second Order")
  
  
  fplot <- ggplot(data = fmelt,aes(x=Time,y=Frequency,group=Bus,colour=Bus))+geom_line()+theme(legend.position="none",axis.title.x=element_blank(),
                                                                                               axis.text.x=element_blank(),
                                                                                               axis.ticks.x=element_blank())#+scale_x_log10()
  frocplot <- ggplot(data = frocmelt,aes(x=Time,y=`Frequency Rate Of Change`,group=Bus,colour=Bus))+geom_line()+theme(legend.position="none",axis.title.x=element_blank(),
                                                                                                            axis.text.x=element_blank(),
                                                                                                            axis.ticks.x=element_blank())#+scale_x_log10()
  ffdiffplot <- ggplot(data = ffdiffmelt,aes(x=Time,y=`Frequency First Order`,group=Bus,colour=Bus))+geom_line()+theme(legend.position="none",axis.title.x=element_blank(),
                                                                                                             axis.text.x=element_blank(),
                                                                                                             axis.ticks.x=element_blank())#+scale_x_log10()
  fsdiffplot <- ggplot(data = fsdiffmelt,aes(x=Time,y=`Frequency Second Order`,group=Bus,colour=Bus))+geom_line()+theme(legend.position="none",axis.title.x=element_blank(),
                                                                                                                        axis.text.x=element_blank(),
                                                                                                                        axis.ticks.x=element_blank())#+scale_x_log10()
  # axis.text.y = element_text(angle=70))

  
  vplot <- ggplot(data = vmelt,aes(x=Time,y=Voltage,group=Bus,colour=Bus))+geom_line()+theme(legend.position="none",axis.title.x=element_blank(),
                                                                                             axis.text.x=element_blank(),
                                                                                             axis.ticks.x=element_blank())#+scale_x_log10()
  vrocplot <- ggplot(data = vrocmelt,aes(x=Time,y=`Voltage Rate Of Change`,group=Bus,colour=Bus))+geom_line()+theme(legend.position="none",axis.title.x=element_blank(),
                                                                                                            axis.text.x=element_blank(),
                                                                                                            axis.ticks.x=element_blank())#+ scale_x_log10()
  vfdiffplot <- ggplot(data = vfdiffmelt,aes(x=Time,y=`Voltage First Order`,group=Bus,colour=Bus))+geom_line()+theme(legend.position="none",axis.title.x=element_blank(),
                                                                                                             axis.text.x=element_blank(),
                                                                                                             axis.ticks.x=element_blank())#+scale_x_log10()
  vsdiffplot <- ggplot(data = vsdiffmelt,aes(x=Time,y=`Voltage Second Order`,group=Bus,colour=Bus))+geom_line()+theme(legend.position="none",axis.title.x=element_blank(),
                                                                                                                      axis.text.x=element_blank(),
                                                                                                                      axis.ticks.x=element_blank())#+scale_x_log10()
  

  aplot <- ggplot(data = amelt,aes(x=Time,y=Angle,group=Bus,colour=Bus))+geom_line()+theme(legend.position="none")#,axis.title.x=element_blank(),
  #   axis.text.x=element_blank(),
  #   axis.ticks.x=element_blank())#+scale_x_log10()
  arocplot <- ggplot(data = arocmelt,aes(x=Time,y=`Angle Rate Of Change`,group=Bus,colour=Bus))+geom_line()+theme(legend.position="none")#,axis.title.x=element_blank(),
  #   axis.text.x=element_blank(),
  #   axis.ticks.x=element_blank())#+scale_x_log10()
  afdiffplot <- ggplot(data = afdiffmelt,aes(x=Time,y=`Angle First Order`,group=Bus,colour=Bus))+geom_line()+theme(legend.position="none")#,axis.title.x=element_blank(),
                                                                                                          #   axis.text.x=element_blank(),
                                                                                                          #   axis.ticks.x=element_blank())#+scale_x_log10()
  asdiffplot <- ggplot(data = asdiffmelt,aes(x=Time,y=`Angle Second Order`,group=Bus,colour=Bus))+geom_line()+theme(legend.position="none") #+scale_x_log10()
  # axis.text.y = element_text(angle=70))
  
 # postscript(file = paste(filename, 'raw_stacked.eps', sep=""))
  png(paste(filename, 'raw_stacked.png', sep=""))
  multiplot(vplot,fplot,aplot)
  dev.off()
  
  #postscript(file = paste(filename, 'firstorder_stacked.eps', sep=""))
  png(paste(filename, 'firstorder_stacked.png', sep=""))
  multiplot(vfdiffplot,ffdiffplot,afdiffplot)
  dev.off()
  
#  postscript(file = paste(filename, 'secondorder_stacked.eps', sep=""))
  png(paste(filename, 'secondorder_stacked.png', sep=""))
  multiplot(vfdiffplot,ffdiffplot,afdiffplot)
  dev.off()
  
 # postscript(file = paste(filename, 'rateofchange_stacked.eps', sep=""))
  png(paste(filename, 'rateofchange_stacked.png', sep=""))
  multiplot(vrocplot,frocplot,arocplot)
  dev.off()
  
  
  fplot_xlog <- ggplot(data = fmelt,aes(x=Time,y=Frequency,group=Bus,colour=Bus))+geom_line()+theme(legend.position="none",axis.title.x=element_blank(),
                                                                                               axis.text.x=element_blank(),
                                                                                               axis.ticks.x=element_blank())+ scale_x_continuous(trans=reverselog_trans(10))
  frocplot_xlog <- ggplot(data = frocmelt,aes(x=Time,y=`Frequency Rate Of Change`,group=Bus,colour=Bus))+geom_line()+theme(legend.position="none",axis.title.x=element_blank(),
                                                                                                                      axis.text.x=element_blank(),
                                                                                                                      axis.ticks.x=element_blank())+ scale_x_continuous(trans=reverselog_trans(10))
  ffdiffplot_xlog <- ggplot(data = ffdiffmelt,aes(x=Time,y=`Frequency First Order`,group=Bus,colour=Bus))+geom_line()+theme(legend.position="none",axis.title.x=element_blank(),
                                                                                                                       axis.text.x=element_blank(),
                                                                                                                       axis.ticks.x=element_blank())+ scale_x_continuous(trans=reverselog_trans(10))
  fsdiffplot_xlog <- ggplot(data = fsdiffmelt,aes(x=Time,y=`Frequency Second Order`,group=Bus,colour=Bus))+geom_line()+theme(legend.position="none",axis.title.x=element_blank(),
                                                                                                                        axis.text.x=element_blank(),
                                                                                                                        axis.ticks.x=element_blank())+ scale_x_continuous(trans=reverselog_trans(10))

  
  vplot_xlog <- ggplot(data = vmelt,aes(x=Time,y=Voltage,group=Bus,colour=Bus))+geom_line()+theme(legend.position="none",axis.title.x=element_blank(),
                                                                                             axis.text.x=element_blank(),
                                                                                             axis.ticks.x=element_blank())+ scale_x_continuous(trans=reverselog_trans(10))
  vrocplot_xlog <- ggplot(data = vrocmelt,aes(x=Time,y=`Voltage Rate Of Change`,group=Bus,colour=Bus))+geom_line()+theme(legend.position="none",axis.title.x=element_blank(),
                                                                                                                    axis.text.x=element_blank(),
                                                                                                                    axis.ticks.x=element_blank())+ scale_x_continuous(trans=reverselog_trans(10))
  vfdiffplot_xlog <- ggplot(data = vfdiffmelt,aes(x=Time,y=`Voltage First Order`,group=Bus,colour=Bus))+geom_line()+theme(legend.position="none",axis.title.x=element_blank(),
                                                                                                                     axis.text.x=element_blank(),
                                                                                                                     axis.ticks.x=element_blank())+ scale_x_continuous(trans=reverselog_trans(10))
  vsdiffplot_xlog <- ggplot(data = vsdiffmelt,aes(x=Time,y=`Voltage Second Order`,group=Bus,colour=Bus))+geom_line()+theme(legend.position="none",axis.title.x=element_blank(),
                                                                                                                      axis.text.x=element_blank(),
                                                                                                                      axis.ticks.x=element_blank())+ scale_x_continuous(trans=reverselog_trans(10))
  
  
  aplot_xlog <- ggplot(data = amelt,aes(x=Time,y=Angle,group=Bus,colour=Bus))+geom_line()+theme(legend.position="none")+ scale_x_continuous(trans=reverselog_trans(10))
  #,axis.title.x=element_blank(),
  #   axis.text.x=element_blank(),
  #   axis.ticks.x=element_blank())#
  arocplot_xlog <- ggplot(data = arocmelt,aes(x=Time,y=`Angle Rate Of Change`,group=Bus,colour=Bus))+geom_line()+theme(legend.position="none")+ scale_x_continuous(trans=reverselog_trans(10))
  #,axis.title.x=element_blank(),
  #   axis.text.x=element_blank(),
  #   axis.ticks.x=element_blank())#+scale_x_log10()
  afdiffplot_xlog <- ggplot(data = afdiffmelt,aes(x=Time,y=`Angle First Order`,group=Bus,colour=Bus))+geom_line()+theme(legend.position="none")+ scale_x_continuous(trans=reverselog_trans(10))
  #,axis.title.x=element_blank(),
  #   axis.text.x=element_blank(),
  #   axis.ticks.x=element_blank())#+scale_x_log10()
  asdiffplot_xlog <- ggplot(data = asdiffmelt,aes(x=Time,y=`Angle Second Order`,group=Bus,colour=Bus))+geom_line()+theme(legend.position="none")+ scale_x_continuous(trans=reverselog_trans(10))
  #+scale_x_log10()
  # axis.text.y = element_text(angle=70))
  
 # postscript(file = paste(filename, 'raw_stacked_xlogrev.eps', sep=""))
  png(paste(filename, 'raw_stacked_xlogrev.png', sep=""))
  multiplot(vplot_xlog,fplot_xlog,aplot_xlog)
  dev.off()
  
 # postscript(file = paste(filename, 'firstorder_stacked_xlogrev.eps', sep=""))
  png(paste(filename, 'firstorder_stacked_xlogrev.png', sep=""))
  multiplot(vfdiffplot_xlog,ffdiffplot_xlog,afdiffplot_xlog)
  dev.off()
  
 # postscript(file = paste(filename, 'secondorder_stacked_xlogrev.eps', sep=""))
  png(paste(filename, 'secondorder_stacked_xlogrev.png', sep=""))
  multiplot(vfdiffplot_xlog,ffdiffplot_xlog,afdiffplot_xlog)
  dev.off()
  
 # postscript(file = paste(filename, 'rateofchange_stacked_xlogrev.eps', sep=""))
  png(paste(filename, 'rateofchange_stacked_xlogrev.png', sep=""))
  multiplot(vrocplot_xlog,frocplot_xlog,arocplot_xlog)
  dev.off()
  
  
}



make_linear_plots_stacked_mulvar("data/import_icestorm.R")
make_linear_plots_stacked_mulvar("data/import_icestorm2.R")
make_linear_plots_stacked_mulvar("data/import_mcnary.R")
make_linear_plots_stacked_mulvar("data/import_openac.R")
make_linear_plots_stacked_mulvar("data/import_opendc.R")
make_linear_plots_stacked_mulvar("data/import_opengen.R")
make_linear_plots_stacked_mulvar("data/import_opengen2.R")
make_linear_plots_stacked_mulvar("data/import_ponderosa.R")
make_linear_plots_stacked_mulvar("data/import_quake1.R")
make_linear_plots_stacked_mulvar("data/import_quake2.R")
make_linear_plots_stacked_mulvar("data/import_gmd.R")
make_linear_plots_stacked_mulvar("data/import_gmd2.R")

#Make X-axis logrithmic 



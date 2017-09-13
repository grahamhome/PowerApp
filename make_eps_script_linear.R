library(TTR)
library(grid)
library(ggplot2)
library(reshape2)
library(scales)
library(TSdist)
library(TSclust)

#Converts the data into the first order difference 
#Input: data = data frame, where the first column is the time and each additional column is a bus with values for each time point
#Returns a data frame of <data> 
#Usage example:     firstdiff <<- firstorder_diff(Freq)
#can run it again to get the second order data frame:  seconddiff <<- firstorder_diff(as.data.frame(firstdiff))
firstorder_diff <- function(data){
  
  busmat <- apply(data[,!names(data)%in% c("Time")],2,diff)
  return(cbind("Time"=data$Time[-1],busmat))
  
}

#Converts a data frame to the rate of change
#Input: data = data frame, where the first column is the time and each additional column is a bus with values for each time point
#Returns a data frame of <data> 
#Usage example: firstroc <<- firstorder_roc(Freq)
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
    #grid.newpage()
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



#The function makes eps image files of stacked plots, where each eps file contains a different variable (Frequency/Voltage/Phase Angle) displayed as
# the raw/rate of change/first order/second order graphs.
#Input: datafile = the import_data file, such as import_gmd.R, that has an import_data() function. Full path to the file has to be provided.
#Output: the different eps files will be created in the directory that this script is located (so in the PowerApp folder, if nothing is changed), 6 in total
# the 3 files will be created, plus the same plots with the x-axis represented in log10 form.
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

#The function makes eps image files of stacked plots, where each eps file contains each different variable (Frequency/Voltage/Phase Angle) displayed as
# either the raw/rate of change/first order/second order graphs.
#Input: datafile = the import_data file, such as import_gmd.R, that has an import_data() function. Full path to the file has to be provided.
#Output: the different eps files will be created in the directory that this script is located (so in the PowerApp folder, if nothing is changed), 8 in total
# the 4 files will be created, one for each of the transformations (raw/roc/first order/second order) plus the same plots with the x-axis represented
# in log10 form reversed - at the far left of the graph is the latest time point, and at the far right is the first time point, with the traditional log 
# scale condensation of the lines.
make_linear_plots_stacked_mulvar <- function(datafile){
  source(datafile)
  import_data()
  
  #setting up the data frames that will be used to make the graphs
  filename <- paste(gsub(pattern = "\ ","",name()),"",sep = "_")
  #frequency
  firstroc_f <<- firstorder_roc(Freq)
  firstdiff_f <<- firstorder_diff(Freq)
  seconddiff_f <<- firstorder_diff(as.data.frame(firstdiff_f))
  #voltage
  firstroc_v <<- firstorder_roc(Volt)
  firstdiff_v <<- firstorder_diff(Volt)
  seconddiff_v <<- firstorder_diff(as.data.frame(firstdiff_v))
  #phase angle
  firstroc_a <<- firstorder_roc(Pangle)
  firstdiff_a <<- firstorder_diff(Pangle)
  seconddiff_a <<- firstorder_diff(as.data.frame(firstdiff_a))
  
  #These are 'melted' versions of the data frames, so that ggplot2 can easily understand them. The value.name column explains what each one is.
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
  
  #These are the ggplot objects that represent each different value/data transformation. Because the order of the variables when stacked is top to bottom
  # Frequency/Voltage/Phase Angle and the x-axis is the same for each of the graphs, the voltage and frequency graphs do not have any tick marks/text/title
  # on their x-axis. The Angle graphs do and they can be read using just this one axis.
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
  

  aplot <- ggplot(data = amelt,aes(x=Time,y=Angle,group=Bus,colour=Bus))+geom_line()+theme(legend.position="none")+ xlab("Time (s)")#,axis.title.x=element_blank(),
  #   axis.text.x=element_blank(),
  #   axis.ticks.x=element_blank())#+scale_x_log10()
  arocplot <- ggplot(data = arocmelt,aes(x=Time,y=`Angle Rate Of Change`,group=Bus,colour=Bus))+geom_line()+theme(legend.position="none")+ xlab("Time (s)")#,axis.title.x=element_blank(),
  #   axis.text.x=element_blank(),
  #   axis.ticks.x=element_blank())#+scale_x_log10()
  afdiffplot <- ggplot(data = afdiffmelt,aes(x=Time,y=`Angle First Order`,group=Bus,colour=Bus))+geom_line()+theme(legend.position="none")+ xlab("Time (s)")#,axis.title.x=element_blank(),
                                                                                                          #   axis.text.x=element_blank(),
                                                                                                          #   axis.ticks.x=element_blank())#+scale_x_log10()
  asdiffplot <- ggplot(data = asdiffmelt,aes(x=Time,y=`Angle Second Order`,group=Bus,colour=Bus))+geom_line()+theme(legend.position="none") + xlab("Time (s)")
  
  #These 4 calls to postscript/multiplot/dev first set up the file to save (the eps files), call the multiplot function to display the different plots
  # on top of each other in the file, then turn off the display to save the file.
  postscript(file = paste(filename, 'raw_stacked.eps', sep=""))
  #png(paste(filename, 'raw_stacked.png', sep=""))
  multiplot(vplot,fplot,aplot)
  dev.off()
  
  postscript(file = paste(filename, 'firstorder_stacked.eps', sep=""))
  #png(paste(filename, 'firstorder_stacked.png', sep=""))
  multiplot(vfdiffplot,ffdiffplot,afdiffplot)
  dev.off()
  
  postscript(file = paste(filename, 'secondorder_stacked.eps', sep=""))
  #png(paste(filename, 'secondorder_stacked.png', sep=""))
  multiplot(vfdiffplot,ffdiffplot,afdiffplot)
  dev.off()
  
  postscript(file = paste(filename, 'rateofchange_stacked.eps', sep=""))
  #png(paste(filename, 'rateofchange_stacked.png', sep=""))
  multiplot(vrocplot,frocplot,arocplot)
  dev.off()
  
  #These are the breaks/labels to be used for the reverse log scale x-axis. Since all 3 variables share the same time points, we only have to do this once
  # for them all, using the Frequency in this case.
  xlog_breaks <- c(0.1,1,10)
  xlog_labs <- c((max(fmelt$Time)-0.1),(max(fmelt$Time)-1),(max(fmelt$Time)-10))
  
  #These are the same as the plots above, except that the x is measured by the reverse of the Time column (so that it displays it in descending rather than
  # ascending order), and the scale_x_log10 is called on these plots to convert them to log scale.
  fplot_xlog <- ggplot(data = fmelt,aes(x=rev(Time),y=Frequency,group=Bus,colour=Bus))+geom_line()+theme(legend.position="none",axis.title.x=element_blank(),
                                                                                               axis.text.x=element_blank(),
                                                                                               axis.ticks.x=element_blank())+ scale_x_log10(breaks=xlog_breaks,labels=xlog_labs)
  frocplot_xlog <- ggplot(data = frocmelt,aes(x=rev(Time),y=`Frequency Rate Of Change`,group=Bus,colour=Bus))+geom_line()+theme(legend.position="none",axis.title.x=element_blank(),
                                                                                                                      axis.text.x=element_blank(),
                                                                                                                      axis.ticks.x=element_blank())+ scale_x_log10(breaks=xlog_breaks,labels=xlog_labs)
  ffdiffplot_xlog <- ggplot(data = ffdiffmelt,aes(x=rev(Time),y=`Frequency First Order`,group=Bus,colour=Bus))+geom_line()+theme(legend.position="none",axis.title.x=element_blank(),
                                                                                                                       axis.text.x=element_blank(),
                                                                                                                       axis.ticks.x=element_blank())+ scale_x_log10(breaks=xlog_breaks,labels=xlog_labs)
  fsdiffplot_xlog <- ggplot(data = fsdiffmelt,aes(x=rev(Time),y=`Frequency Second Order`,group=Bus,colour=Bus))+geom_line()+theme(legend.position="none",axis.title.x=element_blank(),
                                                                                                                        axis.text.x=element_blank(),
                                                                                                                        axis.ticks.x=element_blank())+ scale_x_log10(breaks=xlog_breaks,labels=xlog_labs)

  
  vplot_xlog <- ggplot(data = vmelt,aes(x=rev(Time),y=Voltage,group=Bus,colour=Bus))+geom_line()+theme(legend.position="none",axis.title.x=element_blank(),
                                                                                             axis.text.x=element_blank(),
                                                                                             axis.ticks.x=element_blank())+scale_x_log10(breaks=xlog_breaks,labels=xlog_labs)
  vrocplot_xlog <- ggplot(data = vrocmelt,aes(x=rev(Time),y=`Voltage Rate Of Change`,group=Bus,colour=Bus))+geom_line()+theme(legend.position="none",axis.title.x=element_blank(),
                                                                                                                    axis.text.x=element_blank(),
                                                                                                                    axis.ticks.x=element_blank())+ scale_x_log10(breaks=xlog_breaks,labels=xlog_labs)
  vfdiffplot_xlog <- ggplot(data = vfdiffmelt,aes(x=rev(Time),y=`Voltage First Order`,group=Bus,colour=Bus))+geom_line()+theme(legend.position="none",axis.title.x=element_blank(),
                                                                                                                     axis.text.x=element_blank(),
                                                                                                                     axis.ticks.x=element_blank())+ scale_x_log10(breaks=xlog_breaks,labels=xlog_labs)
  vsdiffplot_xlog <- ggplot(data = vsdiffmelt,aes(x=rev(Time),y=`Voltage Second Order`,group=Bus,colour=Bus))+geom_line()+theme(legend.position="none",axis.title.x=element_blank(),
                                                                                                                      axis.text.x=element_blank(),
                                                                                                                      axis.ticks.x=element_blank())+ scale_x_log10(breaks=xlog_breaks,labels=xlog_labs)
  
  
  aplot_xlog <- ggplot(data = amelt,aes(x=rev(Time),y=Angle,group=Bus,colour=Bus))+geom_line()+theme(legend.position="none")+ scale_x_log10(breaks=xlog_breaks,labels=xlog_labs)+ xlab("Time (s)")

  arocplot_xlog <- ggplot(data = arocmelt,aes(x=rev(Time),y=`Angle Rate Of Change`,group=Bus,colour=Bus))+geom_line()+theme(legend.position="none")+ scale_x_log10(breaks=xlog_breaks,labels=xlog_labs)+ xlab("Time (s)")

  afdiffplot_xlog <- ggplot(data = afdiffmelt,aes(x=rev(Time),y=`Angle First Order`,group=Bus,colour=Bus))+geom_line()+theme(legend.position="none")+ scale_x_log10(breaks=xlog_breaks,labels=xlog_labs)+ xlab("Time (s)")

  asdiffplot_xlog <- ggplot(data = asdiffmelt,aes(x=rev(Time),y=`Angle Second Order`,group=Bus,colour=Bus))+geom_line()+theme(legend.position="none")+ scale_x_log10(breaks=xlog_breaks,labels=xlog_labs)+ xlab("Time (s)")

  #Again, we call these functions 4 times to make the different eps image files.
  postscript(file = paste(filename, 'raw_stacked_xlogrev.eps', sep=""))
  #png(paste(filename, 'raw_stacked_xlogrev.png', sep=""))
  multiplot(vplot_xlog,fplot_xlog,aplot_xlog)
  dev.off()
  
  postscript(file = paste(filename, 'firstorder_stacked_xlogrev.eps', sep=""))
  #png(paste(filename, 'firstorder_stacked_xlogrev.png', sep=""))
  multiplot(vfdiffplot_xlog,ffdiffplot_xlog,afdiffplot_xlog)
  dev.off()
  
 postscript(file = paste(filename, 'secondorder_stacked_xlogrev.eps', sep=""))
 # png(paste(filename, 'secondorder_stacked_xlogrev.png', sep=""))
  multiplot(vfdiffplot_xlog,ffdiffplot_xlog,afdiffplot_xlog)
  dev.off()
  
  postscript(file = paste(filename, 'rateofchange_stacked_xlogrev.eps', sep=""))
  #png(paste(filename, 'rateofchange_stacked_xlogrev.png', sep=""))
  multiplot(vrocplot_xlog,frocplot_xlog,arocplot_xlog)
  dev.off()
  
  
}

make_linear_plots_clust_stackmulvar <- function(clust_type,datafile){
  source(datafile)
  import_data()
  
  #setting up the data frames that will be used to make the graphs
  filename <- paste(gsub(pattern = "\ ","",name()),clust_type,"",sep = "_")
  firstroc_f <<- firstorder_roc(Freq)
  firstdiff_f <<- firstorder_diff(Freq)
  seconddiff_f <<- firstorder_diff(as.data.frame(firstdiff_f))
  #voltage
  firstroc_v <<- firstorder_roc(Volt)
  firstdiff_v <<- firstorder_diff(Volt)
  seconddiff_v <<- firstorder_diff(as.data.frame(firstdiff_v))
  #phase angle
  firstroc_a <<- firstorder_roc(Pangle)
  firstdiff_a <<- firstorder_diff(Pangle)
  seconddiff_a <<- firstorder_diff(as.data.frame(firstdiff_a))
  
  
  vplot_clust<- get_clustered_plot(clust_type,as.data.frame(Volt), "Voltage")
  fplot_clust<- get_clustered_plot(clust_type,as.data.frame(Freq), "Frequency")
  aplot_clust<- get_clustered_plot(clust_type,as.data.frame(Pangle), "Angle")
  
  postscript(file = paste(filename, 'raw_stacked_clustered.eps', sep=""),horizontal = TRUE)
  # png(paste(filename, 'raw_stacked_clustered.png', sep=""))
  multiplot(vplot_clust,fplot_clust,aplot_clust)
  dev.off()
  
  # vrocplot_clust <- get_clustered_plot(clust_type,as.data.frame(firstroc_v), "Voltage Rate of Change")
  # frocplot_clust <- get_clustered_plot(clust_type,as.data.frame(firstroc_f), "Frequency Rate of Change")
  # arocplot_clust <- get_clustered_plot(clust_type,as.data.frame(firstroc_a), "Angle Rate of Change")
  # 
  # postscript(file = paste(filename, 'rateofchange_stacked_clustered.eps', sep=""),horizontal = TRUE)
  # # png(paste(filename, 'rateofchange_stacked_clustered.png', sep=""))
  # multiplot(vrocplot_clust,frocplot_clust,arocplot_clust)
  # dev.off()
  # 
  # vfdiffplot_clust <- get_clustered_plot(clust_type,as.data.frame(firstdiff_v), "Voltage First Order")
  # ffdiffplot_clust <- get_clustered_plot(clust_type,as.data.frame(firstdiff_f), "Frequency First Order")
  # afdiffplot_clust <- get_clustered_plot(clust_type,as.data.frame(firstdiff_a), "Angle First Order")
  # 
  # postscript(file = paste(filename, 'firstdiff_stacked_clustered.eps', sep=""),horizontal = TRUE)
  # # png(paste(filename, 'firstdiff_stacked_clustered.png', sep=""))
  # multiplot(vfdiffplot_clust,ffdiffplot_clust,afdiffplot_clust)
  # dev.off()
  # 
  # vsdiffplot_clust <- get_clustered_plot(clust_type,as.data.frame(seconddiff_v), "Voltage Second Order")
  # fsdiffplot_clust <- get_clustered_plot(clust_type,as.data.frame(seconddiff_f), "Frequency Second Order")
  # asdiffplot_clust <- get_clustered_plot(clust_type,as.data.frame(seconddiff_a), "Angle Second Order")
  # 
  # postscript(file = paste(filename, 'seconddiff_stacked_clustered.eps', sep=""),horizontal = TRUE)
  # # png(paste(filename, 'seconddiff_stacked_clustered.png', sep=""))
  # multiplot(vsdiffplot_clust,fsdiffplot_clust,asdiffplot_clust)
  # dev.off()
}

get_clustered_plot <- function(clust_type, plot_data, plot_value){
  
 # plot_data <- as.data.frame(plot_data)
  #dissmat <- diss(SERIES =  plot_data[(nrow(plot_data)-10):nrow(plot_data),-1],METHOD = clust_type)
  # if(clust_type == "CID" || clust_type == "COR" || clust_type == "CORT" || clust_type == "DWT"){
  #   dissmat <- diss(SERIES =  plot_data[,-1],METHOD = clust_type)
  # } else {
  #   dissmat <- diss(SERIES =  plot_data[(nrow(plot_data)-10):nrow(plot_data),-1],METHOD = clust_type)
  # }
  dissmat <- diss(SERIES =  plot_data[,-1],METHOD = clust_type)
  hc <- hclust(dissmat,method = "average")
  
  cut_hc <- cutree(hc,k = 3)
  cut_names <- data.frame(cut_hc,names(cut_hc))
  colnames(cut_names) <- c("group","Name")
  
  g_1 <- cut_names[cut_names$group == 1,]
  g_2 <- cut_names[cut_names$group == 2,]
  g_3 <- cut_names[cut_names$group == 3,]
  # g_4 <- cut_names[cut_names$group == 4,]
  # g_5 <- cut_names[cut_names$group == 5,]
  grouped_means <- data.frame(plot_data[,1])
  colnames(grouped_means) <- c("Time")
  grouped_means$group1 <- 0
  grouped_means$group2 <- 0
  grouped_means$group3 <- 0
  # grouped_means$group4 <- 0
  # grouped_means$group5 <- 0
  for (n in 1:nrow(plot_data)) {
    grouped_means$group1[n] <- ifelse(nrow(g_1)==1,plot_data[n,g_1$Name[1]],rowMeans(plot_data[n,(colnames(plot_data) %in% g_1$Name)]))
    grouped_means$group2[n] <- ifelse(nrow(g_2)==1,plot_data[n,g_2$Name[1]],rowMeans(plot_data[n,(colnames(plot_data) %in% g_2$Name)]))
    grouped_means$group3[n] <- ifelse(nrow(g_3)==1,plot_data[n,g_3$Name[1]],rowMeans(plot_data[n,(colnames(plot_data) %in% g_3$Name)]))
    # grouped_means$group4[n] <- ifelse(nrow(g_4)==1,plot_data[n,g_4$Name[1]],rowMeans(plot_data[n,(colnames(plot_data) %in% g_4$Name)]))
    # grouped_means$group5[n] <- ifelse(nrow(g_5)==1,plot_data[n,g_5$Name[1]],rowMeans(plot_data[n,(colnames(plot_data) %in% g_5$Name)]))
  }
  groupsmelt <- melt(data = grouped_means,id.vars = "Time",variable.name = "Bus",value.name = "Value")
  # groupsmelt
  if(grepl("Angle",plot_value)==TRUE){
    plot_clust<- ggplot(data = groupsmelt,aes(x=Time,y=Value,group=Bus,colour=Bus))+geom_line()+ylab(plot_value)+theme(legend.position="none")
  } else{
    plot_clust<- ggplot(data = groupsmelt,aes(x=Time,y=Value,group=Bus,colour=Bus))+geom_line()+ylab(plot_value)+theme(legend.position="none",axis.title.x=element_blank(),axis.text.x=element_blank(),
                                                                                                                       axis.ticks.x=element_blank())
  }
  plot_clust
}

# Function to color branches
colbranches <- function(n, col)
{
  a <- attributes(n) # Find the attributes of current node
  # Color edges with requested color
  attr(n, "edgePar") <- c(a$edgePar, list(col=col, lwd=2))
  n # Don't forget to return the node!
}



#These are the data scenarios that I made images of. These are called manually (uncomment them to run, of course)
# make_linear_plots_stacked_mulvar("data/import_icestorm.R")
# make_linear_plots_stacked_mulvar("data/import_icestorm2.R")
# make_linear_plots_stacked_mulvar("data/import_mcnary.R")
# make_linear_plots_stacked_mulvar("data/import_openac.R")
# make_linear_plots_stacked_mulvar("data/import_opendc.R")
# make_linear_plots_stacked_mulvar("data/import_opengen.R")
# make_linear_plots_stacked_mulvar("data/import_opengen2.R")
# make_linear_plots_stacked_mulvar("data/import_ponderosa.R")
# make_linear_plots_stacked_mulvar("data/import_quake1.R")
# make_linear_plots_stacked_mulvar("data/import_quake2.R")
# make_linear_plots_stacked_mulvar("data/import_gmd.R")
# make_linear_plots_stacked_mulvar("data/import_gmd2.R")


make_linear_plots_clust_stackmulvar("EUCL","data/import_mcnary.R")
make_linear_plots_clust_stackmulvar("CORT","data/import_mcnary.R")
make_linear_plots_clust_stackmulvar("COR","data/import_mcnary.R")
make_linear_plots_clust_stackmulvar("DWT","data/import_mcnary.R")
make_linear_plots_clust_stackmulvar("DTWARP","data/import_mcnary.R")


check_if_oob <- function(rowinfo){

  chkfun <- function(x) ifelse((59.98 < x & 60.02 > x),-1,x)
  #chkfun <- function(x) ifelse((59.9 >= x ),-1,x)
  # oobs <- list()
  oobs <- lapply(rowinfo, chkfun)
  oob_buses <- list()
  for (n in 1:length(oobs[])) {
  #  print("checking")
    if(oobs[[n]] != -1){ #&& !(oobs[n] %in% buses_oob)){
     # print("adding")
      oob_buses <- c(oob_buses, oobs[n])
    }
  }
  
  oob_buses
}

make_linear_ooblabel <- function(){
  
  source("data/import_mcnary.R")
  import_data()
  filename <- paste(gsub(pattern = "\ ","",name()),clust_type,"",sep = "_")
  lowlim <- 59.98
  uplim <- 60.02
  buses_oob <- list()
  for (n in 1:nrow(Freq)) {
  #for (n in 1:4) {
    oob_list <- check_if_oob(Freq[n,-1])
    if(length(oob_list) > 0){
      buses_oob <- c(buses_oob, oob_list)
    }
  }
  buses_oob<- buses_oob[!duplicated(names(buses_oob))]
  buses_oob <- buses_oob[1:5]
  # buses_oob_dec <- order(unlist(buses_oob),decreasing = TRUE)
  # # problem_buses <- c(head(buses_oob_dec, n=3), tail(buses_oob_dec,n = 3))
  # problem_buses <- head()
  # 
  # get_busname <- function(x){names(buses_oob[x])}
  
  fmelt <- melt(data = Freq,id.vars = "Time",variable.name = "Bus",value.name = "Frequency")

  fmelt$Oob <- lapply(fmelt$Bus, function(x) ifelse(x %in% names(buses_oob),as.character(x),"within bounds"))
  fmelt$Oob <- unlist(fmelt$Oob)
  fplot <- ggplot(data = fmelt,aes(x=Time,y=Frequency,group=Bus,colour=Oob))+geom_line()+
    scale_fill_manual(c("red","green","blue","yellow","orange","black"))
    # theme(legend.position="none",axis.title.x=element_blank(),
    #       axis.text.x=element_blank(),
    #       axis.ticks.x=element_blank())#+scale_x_log10()
  
 # png(paste(filename, 'raw_stacked_legend.png', sep=""))
  postscript(file = paste(filename, 'raw_freq_ooblegend.eps', sep=""))
  print(fplot)
  dev.off()
  
  # vmelt <- melt(data = Volt,id.vars = "Time",variable.name = "Bus",value.name = "Voltage")
  # 
  # amelt <- melt(data = Pangle,id.vars = "Time",variable.name = "Bus",value.name = "Angle")
  # vplot <- ggplot(data = vmelt,aes(x=Time,y=Voltage,group=Bus,colour=Bus))+geom_line()+theme(legend.position="none",axis.title.x=element_blank(),
  #                                                                                            axis.text.x=element_blank(),
  #                                                                                            axis.ticks.x=element_blank())#+scale_x_log10()
  # 
  # 
  # aplot <- ggplot(data = amelt,aes(x=Time,y=Angle,group=Bus,colour=Bus))+geom_line()+theme(legend.position="none")+ xlab("Time (s)")#,axis.title.x=element_blank(),
  # 
  # 
  # 
  # 
  # 
  # buses_oob <- rapply(Freq[,-1], check_if_oob)
  
  #postscript(file = paste(filename, 'raw_stacked_clustered.eps', sep=""))
  # png(paste(filename, 'raw_stacked_legend.png', sep=""))
  # multiplot(vplot_clust,fplot_clust,aplot_clust)
  # dev.off()
  # 
  # dissmat_f <- diss(SERIES =  Freq[,-1],METHOD = "DWT")
  # hc_freq <- as.dendrogram(hclust(dissmat_f,method = "average"))
  # postscript(file = paste('mcnary_DWT_raw_freq_dendrogram.eps', sep=""),horizontal = TRUE)
  # plot(hc_freq)
  # dev.off()
  # 
  # dissmat_v <- diss(SERIES =  Volt[,-1],METHOD = "DWT")
  # hc_volt <- as.dendrogram(hclust(dissmat_v,method = "average"))
  # postscript(file = paste('mcnary_DWT_raw_volt_dendrogram.eps', sep=""),horizontal = TRUE)
  # plot(hc_volt)
  # dev.off()
  # 
  # dissmat_a <- diss(SERIES =  Pangle[,-1],METHOD = "DWT")
  # hc_angle <- as.dendrogram(hclust(dissmat_a,method = "average"))
  # postscript(file = paste('mcnary_DWT_raw_angle_dendrogram.eps', sep=""),horizontal = TRUE)
  # plot(hc_angle)
  # dev.off()
  # 
  # 
  # #hc_freq[[1]][[1]] = dendrapply(hc_freq[[1]][[1]], colbranches, "red")
  # # hc_freq[[1]][[2]] = dendrapply(hc_freq[[1]][[2]], colbranches, "orange")
  # #hc_freq[[2]] = dendrapply(hc_freq[[2]], colbranches, "blue")
  # hc_freq[[1]] = dendrapply(hc_freq[[1]], colbranches, "red")
  # hc_freq[[2]] = dendrapply(hc_freq[[2]], colbranches, "blue")
  # 
  # 
  # cut_hc_f <- cutree(hc_freq,k = 5)
  # cut_namesf <- data.frame(cut_hc_f,names(cut_hc_f))
  # colnames(cut_namesf) <- c("group","Name")
  # g_1 <- cut_namesf[cut_namesf$group == 1,]
  # g_2 <- cut_namesf[cut_namesf$group == 2,]
  # g_3 <- cut_namesf[cut_namesf$group == 3,]
  # g_4 <- cut_namesf[cut_namesf$group == 4,]
  # g_5 <- cut_namesf[cut_namesf$group == 5,]
  # 
  # vplot_clust<- get_clustered_plot(clust_type,as.data.frame(Volt), "Voltage")
  # fplot_clust<- get_clustered_plot(clust_type,as.data.frame(Freq), "Frequency")
  # aplot_clust<- get_clustered_plot(clust_type,as.data.frame(Pangle), "Angle")
  
  # postscript(file = paste(filename, 'raw_stacked_clustered.eps', sep=""))
  # # png(paste(filename, 'raw_stacked_clustered.png', sep=""))
  # multiplot(vplot_clust,fplot_clust,aplot_clust)
  # dev.off()
}




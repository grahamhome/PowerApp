#works in R 3.2.3

library(TTR)
library(grid)

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

#Plot the heatmap of the raw phase angle, at time <t>, and returns the ggplot object representing that plot
plot_heatmapangle<- function(t){
  if(!exists("autosc")){
    autosc <<- FALSE
  }
  if(!exists("is_zoom")){
    is_zoom <<- 0
  }
  bus_locs <- update_pangle(t)
  if (is_zoom!=0) {
    xmn <- min(bus_locs[(bus_locs$Longitude>=z_xmin),"Longitude"])
    xmx <- max(bus_locs[(bus_locs$Longitude<=z_xmax),"Longitude"])
    ymn <- min(bus_locs[(bus_locs$Latitude>z_ymin),"Latitude"])
    ymx <- max(bus_locs[(bus_locs$Latitude<z_ymax),"Latitude"])
  } else{
    xmn <- min(bus_locs$Longitude)
    xmx <- max(bus_locs$Longitude)
    ymn <- min(bus_locs$Latitude)
    ymx <- max(bus_locs$Latitude)
  }
  xstep <- (xmx-xmn)/80
  ystep <- (ymx-ymn)/80
  
  intp_coords <- interp(bus_locs$Longitude, bus_locs$Latitude, bus_locs$Angle, duplicate = "mean",
                        xo=seq(xmn,xmx, by=xstep),
                        yo=seq(ymn,ymx, by=ystep))
  r <- raster(intp_coords)
  rtp <- rasterToPolygons(r)
  
  rtp@data$id <- 1:nrow(rtp@data)   # add id column for join
  rtpFort <- fortify(rtp, data = rtp@data)
  rtpFortMer <- merge(rtpFort, rtp@data, by.x = 'id', by.y = 'id')  # join data
  
  if(autosc == TRUE){
    # amin <- min(b$Angle)
    # amax <- max(b$Angle)
    amin <- ifelse(min(bus_locs$Angle)<(-40),min(bus_locs$Angle),-40)
    amax <- ifelse(max(bus_locs$Angle)>40,max(bus_locs$Angle),40)
    #  adiff <- (amax-amin)
    #  a_lab <- c(amin,(amin+(adiff/4)),(amin+(adiff/2)),(amax-(adiff/4)),amax)
  } else{
    amin <- -40
    amax <- 40
    # a_lab <- c(-40,-20,0,20,40)
  }
  g <- g + geom_polygon(data = rtpFortMer, 
                        aes(x = long, y = lat, group = group, fill = layer), 
                        alpha = 1, 
                        size = 0,
                        color = NA) +  ## size = 0 to remove the polygon outlines
    scale_fill_gradientn("Angle",colours = c("red","yellow","green","blue","black"),limits=c(amin,amax))+
    theme(legend.position="right",legend.direction="vertical",legend.box="horizontal",axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  #   ggtitle(bquote(atop("Phase Angle at Time",atop(.(Pangle[t,1]),""))))
  g
}
#Plot the heatmap of the rate of change of the phase angle, at time <t>, and returns the ggplot object representing that plot
plot_heatmapangle_firstroc<- function(t){
  tf <- t(t(firstroc[t,-1]))
  tf <- cbind(rownames(tf),tf)
  colnames(tf) <- c("Bus.Name","Angle")
  bus_loc_angle_foroc <- merge(bus_locs[ , ! colnames(bus_locs) %in% c("Angle")],tf, by="Bus.Name")
  bus_locs$Angle <- as.numeric(as.character(bus_loc_angle_foroc$Angle))
  # bus_locs <- update_freq_firstroc(t)
  
  if(!exists("autosc")){
    autosc <<- FALSE
  }
  if(!exists("is_zoom")){
    is_zoom <<- 0
  }
  if (is_zoom!=0) {
    xmn <- min(bus_locs[(bus_locs$Longitude>=z_xmin),"Longitude"])
    xmx <- max(bus_locs[(bus_locs$Longitude<=z_xmax),"Longitude"])
    ymn <- min(bus_locs[(bus_locs$Latitude>z_ymin),"Latitude"])
    ymx <- max(bus_locs[(bus_locs$Latitude<z_ymax),"Latitude"])
  } else{
    xmn <- min(bus_locs$Longitude)
    xmx <- max(bus_locs$Longitude)
    ymn <- min(bus_locs$Latitude)
    ymx <- max(bus_locs$Latitude)
  }
  xstep <- (xmx-xmn)/80
  ystep <- (ymx-ymn)/80
  
  intp_coords <- interp(bus_locs$Longitude, bus_locs$Latitude, bus_locs$Angle, duplicate = "mean",
                        xo=seq(xmn,xmx, by=xstep),
                        yo=seq(ymn,ymx, by=ystep))
  r <- raster(intp_coords)
  rtp <- rasterToPolygons(r)
  
  rtp@data$id <- 1:nrow(rtp@data)   # add id column for join
  rtpFort <- fortify(rtp, data = rtp@data)
  rtpFortMer <- merge(rtpFort, rtp@data, by.x = 'id', by.y = 'id')  # join data
  
  amin <- ifelse(min(bus_locs$Angle)<(-0.001),min(bus_locs$Angle),(-0.001))
  amax <- ifelse(max(bus_locs$Angle)>0.001,max(bus_locs$Angle),0.001)
  g <- g + geom_polygon(data = rtpFortMer, 
                        aes(x = long, y = lat, group = group, fill = layer), 
                        alpha = 1, 
                        size = 0,
                        color = NA) +  ## size = 0 to remove the polygon outlines
    scale_fill_gradientn("Rate of Change",colours = c("red","yellow","green","blue","black"),limits=c(amin,amax))+
    theme(legend.position="right",legend.direction="vertical",legend.box="horizontal",axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  #   ggtitle(bquote(atop("Phase Angle at Time",atop(.(Pangle[t,1]),""))))
  g
}
#Plot the heatmap of the first order difference of the phase angle, at time <t>, and returns the ggplot object representing that plot
plot_heatmapangle_firstod<- function(t){
  tf <- t(t(firstdiff[t,-1]))
  tf <- cbind(rownames(tf),tf)
  colnames(tf) <- c("Bus.Name","Angle")
  bus_loc_angle_foroc <- merge(bus_locs[ , ! colnames(bus_locs) %in% c("Angle")],tf, by="Bus.Name")
  bus_locs$Angle <- as.numeric(as.character(bus_loc_angle_foroc$Angle))
  # bus_locs <- update_freq_firstroc(t)
  
  if(!exists("autosc")){
    autosc <<- FALSE
  }
  if(!exists("is_zoom")){
    is_zoom <<- 0
  }
  if (is_zoom!=0) {
    xmn <- min(bus_locs[(bus_locs$Longitude>=z_xmin),"Longitude"])
    xmx <- max(bus_locs[(bus_locs$Longitude<=z_xmax),"Longitude"])
    ymn <- min(bus_locs[(bus_locs$Latitude>z_ymin),"Latitude"])
    ymx <- max(bus_locs[(bus_locs$Latitude<z_ymax),"Latitude"])
  } else{
    xmn <- min(bus_locs$Longitude)
    xmx <- max(bus_locs$Longitude)
    ymn <- min(bus_locs$Latitude)
    ymx <- max(bus_locs$Latitude)
  }
  xstep <- (xmx-xmn)/80
  ystep <- (ymx-ymn)/80
  
  intp_coords <- interp(bus_locs$Longitude, bus_locs$Latitude, bus_locs$Angle, duplicate = "mean",
                        xo=seq(xmn,xmx, by=xstep),
                        yo=seq(ymn,ymx, by=ystep))
  r <- raster(intp_coords)
  rtp <- rasterToPolygons(r)
  
  rtp@data$id <- 1:nrow(rtp@data)   # add id column for join
  rtpFort <- fortify(rtp, data = rtp@data)
  rtpFortMer <- merge(rtpFort, rtp@data, by.x = 'id', by.y = 'id')  # join data
  
  
    # amin <- min(b$Angle)
    # amax <- max(b$Angle)
    # amin <- ifelse(min(bus_locs$Angle)<(-40),min(bus_locs$Angle),-40)
    # amax <- ifelse(max(bus_locs$Angle)>40,max(bus_locs$Angle),40)
    
  amin <- ifelse(min(bus_locs$Angle)<(-0.001),min(bus_locs$Angle),(-0.001))
  amax <- ifelse(max(bus_locs$Angle)>0.001,max(bus_locs$Angle),0.001)
  
  g <- g + geom_polygon(data = rtpFortMer, 
                        aes(x = long, y = lat, group = group, fill = layer), 
                        alpha = 1, 
                        size = 0,
                        color = NA) +  ## size = 0 to remove the polygon outlines
    scale_fill_gradientn("First Order",colours = c("red","yellow","green","blue","black"),limits=c(amin,amax))+
    theme(legend.position="right",legend.direction="vertical",legend.box="horizontal",axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  #   ggtitle(bquote(atop("Phase Angle at Time",atop(.(Pangle[t,1]),""))))
  g
}
#Plot the heatmap of the second order difference of the phase angle, at time <t>, and returns the ggplot object representing that plot
plot_heatmapangle_secondod<- function(t){
  tf <- t(t(seconddiff[t,-1]))
  tf <- cbind(rownames(tf),tf)
  colnames(tf) <- c("Bus.Name","Angle")
  bus_loc_angle_foroc <- merge(bus_locs[ , ! colnames(bus_locs) %in% c("Angle")],tf, by="Bus.Name")
  bus_locs$Angle <- as.numeric(as.character(bus_loc_angle_foroc$Angle))
  # bus_locs <- update_freq_firstroc(t)
  
  if(!exists("autosc")){
    autosc <<- FALSE
  }
  if(!exists("is_zoom")){
    is_zoom <<- 0
  }
  if (is_zoom!=0) {
    xmn <- min(bus_locs[(bus_locs$Longitude>=z_xmin),"Longitude"])
    xmx <- max(bus_locs[(bus_locs$Longitude<=z_xmax),"Longitude"])
    ymn <- min(bus_locs[(bus_locs$Latitude>z_ymin),"Latitude"])
    ymx <- max(bus_locs[(bus_locs$Latitude<z_ymax),"Latitude"])
  } else{
    xmn <- min(bus_locs$Longitude)
    xmx <- max(bus_locs$Longitude)
    ymn <- min(bus_locs$Latitude)
    ymx <- max(bus_locs$Latitude)
  }
  xstep <- (xmx-xmn)/80
  ystep <- (ymx-ymn)/80
  
  intp_coords <- interp(bus_locs$Longitude, bus_locs$Latitude, bus_locs$Angle, duplicate = "mean",
                        xo=seq(xmn,xmx, by=xstep),
                        yo=seq(ymn,ymx, by=ystep))
  r <- raster(intp_coords)
  rtp <- rasterToPolygons(r)
  
  rtp@data$id <- 1:nrow(rtp@data)   # add id column for join
  rtpFort <- fortify(rtp, data = rtp@data)
  rtpFortMer <- merge(rtpFort, rtp@data, by.x = 'id', by.y = 'id')  # join data
  
  
  # amin <- min(b$Angle)
  # amax <- max(b$Angle)
  # amin <- ifelse(min(bus_locs$Angle)<(-40),min(bus_locs$Angle),-40)
  # amax <- ifelse(max(bus_locs$Angle)>40,max(bus_locs$Angle),40)
  
  amin <- ifelse(min(bus_locs$Angle)<(-0.001),min(bus_locs$Angle),(-0.001))
  amax <- ifelse(max(bus_locs$Angle)>0.001,max(bus_locs$Angle),0.001)
  
  g <- g + geom_polygon(data = rtpFortMer, 
                        aes(x = long, y = lat, group = group, fill = layer), 
                        alpha = 1, 
                        size = 0,
                        color = NA) +  ## size = 0 to remove the polygon outlines
    scale_fill_gradientn("Second Order",colours = c("red","yellow","green","blue","black"),limits=c(amin,amax))+
    theme(legend.position="right",legend.direction="vertical",legend.box="horizontal",axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  #   ggtitle(bquote(atop("Phase Angle at Time",atop(.(Pangle[t,1]),""))))
  g
}

plot_heatmapvolt<- function(t){
  if(!exists("autosc")){
    autosc <<- FALSE
  }
  if(!exists("is_zoom")){
    is_zoom <<- 0
    z_xmin <<- 0
    z_xmax <<- 0
    z_ymin <<- 0
    z_ymax <<- 0
  }
  bus_locs <- update_volt(t)
  if (is_zoom!=0) {
    xmn <- min(bus_locs[(bus_locs$Longitude>=z_xmin),"Longitude"])
    xmx <- max(bus_locs[(bus_locs$Longitude<=z_xmax),"Longitude"])
    ymn <- min(bus_locs[(bus_locs$Latitude>z_ymin),"Latitude"])
    ymx <- max(bus_locs[(bus_locs$Latitude<z_ymax),"Latitude"])
  } else{
    xmn <- min(bus_locs$Longitude)
    xmx <- max(bus_locs$Longitude)
    ymn <- min(bus_locs$Latitude)
    ymx <- max(bus_locs$Latitude)
  }
  
  if(autosc == TRUE){
    vmin <- min(bus_locs$Voltage)
    vmax <- max(bus_locs$Voltage)
    #vmin <- ifelse(min(bus_locs$Voltage)<0.8,min(bus_locs$Voltage),0.8)
    # vmax <- ifelse(max(bus_locs$Voltage)>1.2,max(bus_locs$Voltage),1.2)
  } else{
    vmin <- 0.8
    vmax <- 1.2
  }
  xstep <- (xmx-xmn)/80
  ystep <- (ymx-ymn)/80
  intp_coords <- interp(bus_locs$Longitude, bus_locs$Latitude, bus_locs$Voltage, duplicate = "mean",
                        xo=seq(xmn,xmx, by=xstep),
                        yo=seq(ymn,ymx, by=ystep))
  r <- raster(intp_coords)
  
  rtp <- rasterToPolygons(r)
  rtp@data$id <- 1:nrow(rtp@data)   # add id column for join
  rtpFort <- fortify(rtp, data = rtp@data)
  rtpFortMer <- merge(rtpFort, rtp@data, by.x = 'id', by.y = 'id')  # join data
  
  if (vmin<0.8 & vmax <= 1.2) {
    v_cols <-c("red","yellow","orange","blue","green")
  } else if(vmax <1.2 & vmin >= 0.8){
    v_cols <-c("green","blue","orange","yellow","red")
  } else{
    v_cols <-c("red","yellow","green","blue","black")
  }
  
  g <- g + 
    geom_polygon(data = rtpFortMer,
                 aes(x = long, y = lat, group = group, fill = layer),
                 alpha = 1,
                 size = 0,
                 color = NA) +  ## size = 0 to remove the polygon outlines
    #geom_point(data=bus_locs,aes(x=Longitude,y=Latitude, colour=alarm,group=Sub.Name),size=5,alpha=0.7,shape=16) +
    #  scale_colour_gradientn("Alarm Status",colours = c("green","blue","orange","yellow"),limits=c(vmin,vmax)) +
    scale_fill_gradientn("Voltage",colours = v_cols,limits=c(vmin,vmax))+
    theme(legend.position="right",legend.direction="vertical",legend.box="horizontal",axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  #  ggtitle(bquote(atop("Voltage at Time",atop(.(Volt[t,1]),""))))
  g
}
plot_heatmapvolt_firstroc<- function(t){
  tf <- t(t(firstroc[t,-1]))
  tf <- cbind(rownames(tf),tf)
  colnames(tf) <- c("Bus.Name","Voltage")
  bus_loc_volt_foroc <- merge(bus_locs[ , ! colnames(bus_locs) %in% c("Voltage")],tf, by="Bus.Name")
  bus_locs$Voltage <- as.numeric(as.character(bus_loc_volt_foroc$Voltage))
  # bus_locs <- update_freq_firstroc(t)
  
  if(!exists("autosc")){
    autosc <<- FALSE
  }
  if(!exists("is_zoom")){
    is_zoom <<- 0
  }
  # if(autosc == TRUE){
  #   fmin <- ifelse(min(bus_locs$Frequency)<59.9,min(bus_locs$Frequency),59.9)
  #   fmax <- ifelse(max(bus_locs$Frequency)>60.1,max(bus_locs$Frequency),60.1)
  #   #  vdiff <- (vmax-vmin)
  #   #   v_lab <- c(vmin,(vmin+(vdiff/4)),(vmin+(vdiff/2)),(vmax-(vdiff/4)),vmax)
  # } else{
  #   fmin <- 59.8
  #   fmax <- 60.2
  # }
  xmn <- min(bus_locs$Longitude)
  xmx <- max(bus_locs$Longitude)
  ymn <- min(bus_locs$Latitude)
  ymx <- max(bus_locs$Latitude)


  
  vmin <- ifelse(min(bus_locs$Voltage)<(-0.001),min(bus_locs$Voltage),(-0.001))
  vmax <- ifelse(max(bus_locs$Voltage)>0.001,max(bus_locs$Voltage),0.001)

  xstep <- (xmx-xmn)/80
  ystep <- (ymx-ymn)/80
  intp_coords <- interp(bus_locs$Longitude, bus_locs$Latitude, bus_locs$Voltage, duplicate = "mean",
                        xo=seq(xmn,xmx, by=xstep),
                        yo=seq(ymn,ymx, by=ystep))
  r <- raster(intp_coords)
  
  rtp <- rasterToPolygons(r)
  rtp@data$id <- 1:nrow(rtp@data)   # add id column for join
  rtpFort <- fortify(rtp, data = rtp@data)
  rtpFortMer <- merge(rtpFort, rtp@data, by.x = 'id', by.y = 'id')  # join data
  
  if (vmin<0.8 & vmax <= 1.2) {
    v_cols <-c("red","yellow","orange","blue","green")
  } else if(vmax <1.2 & vmin >= 0.8){
    v_cols <-c("green","blue","orange","yellow","red")
  } else{
    v_cols <-c("red","yellow","green","blue","black")
  }
  
  g <- g + 
    geom_polygon(data = rtpFortMer,
                 aes(x = long, y = lat, group = group, fill = layer),
                 alpha = 1,
                 size = 0,
                 color = NA) +  ## size = 0 to remove the polygon outlines
    #geom_point(data=bus_locs,aes(x=Longitude,y=Latitude, colour=alarm,group=Sub.Name),size=5,alpha=0.7,shape=16) +
    #  scale_colour_gradientn("Alarm Status",colours = c("green","blue","orange","yellow"),limits=c(vmin,vmax)) +
    scale_fill_gradientn("Rate of Change",colours = v_cols,limits=c(vmin,vmax))+
    theme(legend.position="right",legend.direction="vertical",legend.box="horizontal",axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  #  ggtitle(bquote(atop("Voltage at Time",atop(.(Volt[t,1]),""))))
  g
}
plot_heatmapvolt_firstod<- function(t){
  tf <- t(t(firstdiff[t,-1]))
  tf <- cbind(rownames(tf),tf)
  colnames(tf) <- c("Bus.Name","Voltage")
  bus_loc_volt_foroc <- merge(bus_locs[ , ! colnames(bus_locs) %in% c("Voltage")],tf, by="Bus.Name")
  bus_locs$Voltage <- as.numeric(as.character(bus_loc_volt_foroc$Voltage))
  # bus_locs <- update_freq_firstroc(t)
  
  if(!exists("autosc")){
    autosc <<- FALSE
  }
  if(!exists("is_zoom")){
    is_zoom <<- 0
  }
  # if(autosc == TRUE){
  #   fmin <- ifelse(min(bus_locs$Frequency)<59.9,min(bus_locs$Frequency),59.9)
  #   fmax <- ifelse(max(bus_locs$Frequency)>60.1,max(bus_locs$Frequency),60.1)
  #   #  vdiff <- (vmax-vmin)
  #   #   v_lab <- c(vmin,(vmin+(vdiff/4)),(vmin+(vdiff/2)),(vmax-(vdiff/4)),vmax)
  # } else{
  #   fmin <- 59.8
  #   fmax <- 60.2
  # }
  xmn <- min(bus_locs$Longitude)
  xmx <- max(bus_locs$Longitude)
  ymn <- min(bus_locs$Latitude)
  ymx <- max(bus_locs$Latitude)
  
  
  
  vmin <- ifelse(min(bus_locs$Voltage)<(-0.001),min(bus_locs$Voltage),(-0.001))
  vmax <- ifelse(max(bus_locs$Voltage)>0.001,max(bus_locs$Voltage),0.001)
  
  xstep <- (xmx-xmn)/80
  ystep <- (ymx-ymn)/80
  intp_coords <- interp(bus_locs$Longitude, bus_locs$Latitude, bus_locs$Voltage, duplicate = "mean",
                        xo=seq(xmn,xmx, by=xstep),
                        yo=seq(ymn,ymx, by=ystep))
  r <- raster(intp_coords)
  
  rtp <- rasterToPolygons(r)
  rtp@data$id <- 1:nrow(rtp@data)   # add id column for join
  rtpFort <- fortify(rtp, data = rtp@data)
  rtpFortMer <- merge(rtpFort, rtp@data, by.x = 'id', by.y = 'id')  # join data
  
  if (vmin<0.8 & vmax <= 1.2) {
    v_cols <-c("red","yellow","orange","blue","green")
  } else if(vmax <1.2 & vmin >= 0.8){
    v_cols <-c("green","blue","orange","yellow","red")
  } else{
    v_cols <-c("red","yellow","green","blue","black")
  }
  
  g <- g + 
    geom_polygon(data = rtpFortMer,
                 aes(x = long, y = lat, group = group, fill = layer),
                 alpha = 1,
                 size = 0,
                 color = NA) +  ## size = 0 to remove the polygon outlines
    #geom_point(data=bus_locs,aes(x=Longitude,y=Latitude, colour=alarm,group=Sub.Name),size=5,alpha=0.7,shape=16) +
    #  scale_colour_gradientn("Alarm Status",colours = c("green","blue","orange","yellow"),limits=c(vmin,vmax)) +
    scale_fill_gradientn("First Order",colours = v_cols,limits=c(vmin,vmax))+
    theme(legend.position="right",legend.direction="vertical",legend.box="horizontal",axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  #  ggtitle(bquote(atop("Voltage at Time",atop(.(Volt[t,1]),""))))
  g
}
plot_heatmapvolt_secondod<- function(t){
  tf <- t(t(seconddiff[t,-1]))
  tf <- cbind(rownames(tf),tf)
  colnames(tf) <- c("Bus.Name","Voltage")
  bus_loc_volt_foroc <- merge(bus_locs[ , ! colnames(bus_locs) %in% c("Voltage")],tf, by="Bus.Name")
  bus_locs$Voltage <- as.numeric(as.character(bus_loc_volt_foroc$Voltage))
  # bus_locs <- update_freq_firstroc(t)
  
  if(!exists("autosc")){
    autosc <<- FALSE
  }
  if(!exists("is_zoom")){
    is_zoom <<- 0
  }
  # if(autosc == TRUE){
  #   fmin <- ifelse(min(bus_locs$Frequency)<59.9,min(bus_locs$Frequency),59.9)
  #   fmax <- ifelse(max(bus_locs$Frequency)>60.1,max(bus_locs$Frequency),60.1)
  #   #  vdiff <- (vmax-vmin)
  #   #   v_lab <- c(vmin,(vmin+(vdiff/4)),(vmin+(vdiff/2)),(vmax-(vdiff/4)),vmax)
  # } else{
  #   fmin <- 59.8
  #   fmax <- 60.2
  # }
  xmn <- min(bus_locs$Longitude)
  xmx <- max(bus_locs$Longitude)
  ymn <- min(bus_locs$Latitude)
  ymx <- max(bus_locs$Latitude)
  
  
  
  vmin <- ifelse(min(bus_locs$Voltage)<(-0.001),min(bus_locs$Voltage),(-0.001))
  vmax <- ifelse(max(bus_locs$Voltage)>0.001,max(bus_locs$Voltage),0.001)
  
  xstep <- (xmx-xmn)/80
  ystep <- (ymx-ymn)/80
  intp_coords <- interp(bus_locs$Longitude, bus_locs$Latitude, bus_locs$Voltage, duplicate = "mean",
                        xo=seq(xmn,xmx, by=xstep),
                        yo=seq(ymn,ymx, by=ystep))
  r <- raster(intp_coords)
  
  rtp <- rasterToPolygons(r)
  rtp@data$id <- 1:nrow(rtp@data)   # add id column for join
  rtpFort <- fortify(rtp, data = rtp@data)
  rtpFortMer <- merge(rtpFort, rtp@data, by.x = 'id', by.y = 'id')  # join data
  
  if (vmin<0.8 & vmax <= 1.2) {
    v_cols <-c("red","yellow","orange","blue","green")
  } else if(vmax <1.2 & vmin >= 0.8){
    v_cols <-c("green","blue","orange","yellow","red")
  } else{
    v_cols <-c("red","yellow","green","blue","black")
  }
  
  g <- g + 
    geom_polygon(data = rtpFortMer,
                 aes(x = long, y = lat, group = group, fill = layer),
                 alpha = 1,
                 size = 0,
                 color = NA) +  ## size = 0 to remove the polygon outlines
    #geom_point(data=bus_locs,aes(x=Longitude,y=Latitude, colour=alarm,group=Sub.Name),size=5,alpha=0.7,shape=16) +
    #  scale_colour_gradientn("Alarm Status",colours = c("green","blue","orange","yellow"),limits=c(vmin,vmax)) +
    scale_fill_gradientn("Second Order",colours = v_cols,limits=c(vmin,vmax))+
    theme(legend.position="right",legend.direction="vertical",legend.box="horizontal")
  #  ggtitle(bquote(atop("Voltage at Time",atop(.(Volt[t,1]),""))))
  g
}

plot_heatmapfreq<- function(t){
  bus_locs <- update_freq(t)
  if(!exists("autosc")){
    autosc <<- FALSE
  }
  if(!exists("is_zoom")){
    is_zoom <<- 0
  }
  if(autosc == TRUE){
    fmin <- ifelse(min(bus_locs$Frequency)<59.9,min(bus_locs$Frequency),59.9)
    fmax <- ifelse(max(bus_locs$Frequency)>60.1,max(bus_locs$Frequency),60.1)
    #  vdiff <- (vmax-vmin)
    #   v_lab <- c(vmin,(vmin+(vdiff/4)),(vmin+(vdiff/2)),(vmax-(vdiff/4)),vmax)
  } else{
    fmin <- 59.8
    fmax <- 60.2
  }
  if (is_zoom!=0) {
    xmn <- min(bus_locs[(bus_locs$Longitude>=z_xmin),"Longitude"])
    xmx <- max(bus_locs[(bus_locs$Longitude<=z_xmax),"Longitude"])
    ymn <- min(bus_locs[(bus_locs$Latitude>z_ymin),"Latitude"])
    ymx <- max(bus_locs[(bus_locs$Latitude<z_ymax),"Latitude"])
  } else{
    xmn <- min(bus_locs$Longitude)
    xmx <- max(bus_locs$Longitude)
    ymn <- min(bus_locs$Latitude)
    ymx <- max(bus_locs$Latitude)
  }
  xstep <- (xmx-xmn)/80
  ystep <- (ymx-ymn)/80
  intp_coords <- interp(bus_locs$Longitude, bus_locs$Latitude, bus_locs$Frequency, duplicate = "mean",
                        xo=seq(xmn,xmx, by=xstep),
                        yo=seq(ymn,ymx, by=ystep))
  r <- raster(intp_coords)
  rtp <- rasterToPolygons(r)
  
  rtp@data$id <- 1:nrow(rtp@data)   # add id column for join
  rtpFort <- fortify(rtp, data = rtp@data)
  rtpFortMer <- merge(rtpFort, rtp@data, by.x = 'id', by.y = 'id')  # join data
  # if (fmin<59.8 & fmax <= 60.2) {
  #   f_cols <-c("red","yellow","orange","blue","green")
  # } else if(fmax <60.2 & fmin >= 59.8){
  #   f_cols <-c("green","blue","orange","yellow","red")
  # } else{
  #   f_cols <-c("red","yellow","green","blue","black")
  # }
  f_cols <-c("red","yellow","green","blue","black")
  g <- g + geom_polygon(data = rtpFortMer, 
                        aes(x = long, y = lat, group = group, fill = layer), 
                        alpha = 1, 
                        size = 0, lwd = 0) +  ## size = 0 to remove the polygon outlines
    #geom_point(data=bus_locs,aes(x=Longitude,y=Latitude, colour=Frequency,group=Sub.Name),size=5,alpha=0.7,shape=16) +
    #scale_colour_gradientn("Bus Frequency",colours = c("green","blue","orange","yellow"),limits=c(fmin,fmax)) +
    #scale_fill_gradientn("Frequency",colours = topo.colors(255))+
    scale_fill_gradientn("Frequency",colours = f_cols,limits=c(fmin,fmax))+
    theme(legend.position="right",legend.direction="vertical",legend.box="horizontal",axis.title.x=element_blank(),
                                                                                             axis.text.x=element_blank(),
                                                                                             axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  
  #+
  #   ggtitle(bquote(atop("Frequency at Time",atop(.(Freq[t,1]),""))))
  g
}
plot_heatmapfreq_firstroc<- function(t){
  tf <- t(t(firstroc[t,-1]))
  tf <- cbind(rownames(tf),tf)
  colnames(tf) <- c("Bus.Name","Frequency")
  bus_loc_freq_foroc <- merge(bus_locs[ , ! colnames(bus_locs) %in% c("Frequency")],tf, by="Bus.Name")
  bus_locs$Frequency <- as.numeric(as.character(bus_loc_freq_foroc$Frequency))
 # bus_locs <- update_freq_firstroc(t)
 
  if(!exists("autosc")){
    autosc <<- FALSE
  }
  if(!exists("is_zoom")){
    is_zoom <<- 0
  }
  # if(autosc == TRUE){
  #   fmin <- ifelse(min(bus_locs$Frequency)<59.9,min(bus_locs$Frequency),59.9)
  #   fmax <- ifelse(max(bus_locs$Frequency)>60.1,max(bus_locs$Frequency),60.1)
  #   #  vdiff <- (vmax-vmin)
  #   #   v_lab <- c(vmin,(vmin+(vdiff/4)),(vmin+(vdiff/2)),(vmax-(vdiff/4)),vmax)
  # } else{
  #   fmin <- 59.8
  #   fmax <- 60.2
  # }
  fmin <- ifelse(min(bus_locs$Frequency)<(-0.001),min(bus_locs$Frequency),(-0.001))
  fmax <- ifelse(max(bus_locs$Frequency)>0.001,max(bus_locs$Frequency),0.001)
  xmn <- min(bus_locs$Longitude)
  xmx <- max(bus_locs$Longitude)
  ymn <- min(bus_locs$Latitude)
  ymx <- max(bus_locs$Latitude)
  
  xstep <- (xmx-xmn)/80
  ystep <- (ymx-ymn)/80
  intp_coords <- interp(bus_locs$Longitude, bus_locs$Latitude, bus_locs$Frequency, duplicate = "mean",
                        xo=seq(xmn,xmx, by=xstep),
                        yo=seq(ymn,ymx, by=ystep))
  r <- raster(intp_coords)
  rtp <- rasterToPolygons(r)
  
  rtp@data$id <- 1:nrow(rtp@data)   # add id column for join
  rtpFort <- fortify(rtp, data = rtp@data)
  rtpFortMer <- merge(rtpFort, rtp@data, by.x = 'id', by.y = 'id')  # join data
  if (fmin<59.8 & fmax <= 60.2) {
    f_cols <-c("red","yellow","orange","blue","green")
  } else if(fmax <60.2 & fmin >= 59.8){
    f_cols <-c("green","blue","orange","yellow","red")
  } else{
    f_cols <-c("red","yellow","green","blue","black")
  }
  g <- g + geom_polygon(data = rtpFortMer, 
                        aes(x = long, y = lat, group = group, fill = layer), 
                        alpha = 1, 
                        size = 0,
                        color = NA) +  
    scale_fill_gradientn("Rate of Change",colours = f_cols,limits=c(fmin,fmax))+
    theme(legend.position="right",legend.direction="vertical",legend.box="horizontal",axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  #   ggtitle(bquote(atop("Frequency at Time",atop(.(Freq[t,1]),""))))
  g
}
plot_heatmapfreq_firstod<- function(t){
  tf <- t(t(firstdiff[t,-1]))
  tf <- cbind(rownames(tf),tf)
  colnames(tf) <- c("Bus.Name","Frequency")
  bus_loc_freq_fod <- merge(bus_locs[ , ! colnames(bus_locs) %in% c("Frequency")],tf, by="Bus.Name")
  bus_locs$Frequency <- as.numeric(as.character(bus_loc_freq_fod$Frequency))
  # bus_locs <- update_freq_firstroc(t)
  
  if(!exists("autosc")){
    autosc <<- FALSE
  }
  if(!exists("is_zoom")){
    is_zoom <<- 0
  }
  # if(autosc == TRUE){
  #   fmin <- ifelse(min(bus_locs$Frequency)<59.9,min(bus_locs$Frequency),59.9)
  #   fmax <- ifelse(max(bus_locs$Frequency)>60.1,max(bus_locs$Frequency),60.1)
  #   #  vdiff <- (vmax-vmin)
  #   #   v_lab <- c(vmin,(vmin+(vdiff/4)),(vmin+(vdiff/2)),(vmax-(vdiff/4)),vmax)
  # } else{
  #   fmin <- 59.8
  #   fmax <- 60.2
  # }
  fmin <- ifelse(min(bus_locs$Frequency)<(-0.001),min(bus_locs$Frequency),(-0.001))
  fmax <- ifelse(max(bus_locs$Frequency)>0.001,max(bus_locs$Frequency),0.001)
  xmn <- min(bus_locs$Longitude)
  xmx <- max(bus_locs$Longitude)
  ymn <- min(bus_locs$Latitude)
  ymx <- max(bus_locs$Latitude)
  
  xstep <- (xmx-xmn)/80
  ystep <- (ymx-ymn)/80
  intp_coords <- interp(bus_locs$Longitude, bus_locs$Latitude, bus_locs$Frequency, duplicate = "mean",
                        xo=seq(xmn,xmx, by=xstep),
                        yo=seq(ymn,ymx, by=ystep))
  r <- raster(intp_coords)
  rtp <- rasterToPolygons(r)
  
  rtp@data$id <- 1:nrow(rtp@data)   # add id column for join
  rtpFort <- fortify(rtp, data = rtp@data)
  rtpFortMer <- merge(rtpFort, rtp@data, by.x = 'id', by.y = 'id')  # join data
  if (fmin<59.8 & fmax <= 60.2) {
    f_cols <-c("red","yellow","orange","blue","green")
  } else if(fmax <60.2 & fmin >= 59.8){
    f_cols <-c("green","blue","orange","yellow","red")
  } else{
    f_cols <-c("red","yellow","green","blue","black")
  }
  g <- g + geom_polygon(data = rtpFortMer, 
                        aes(x = long, y = lat, group = group, fill = layer), 
                        alpha = 1, 
                        size = 0,
                        color = NA) +  
    scale_fill_gradientn("First Order",colours = f_cols,limits=c(fmin,fmax))+
    theme(legend.position="right",legend.direction="vertical",legend.box="horizontal",axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  #   ggtitle(bquote(atop("Frequency at Time",atop(.(Freq[t,1]),""))))
  #g
  #ggsave(filename = "SolarFlare_904_freq_firstorder.eps",plot = g,device = "eps")
  #ggsave("SolarFlare_904_freq_firstorder.pdf", g, width = 5.16, height = 6.61)
  g
  # postscript(file = "SolarFlare_904_freq_firstorder.eps", horizontal = FALSE, onefile = FALSE, paper = "special")
  # print(g)
  # dev.off()
}
plot_heatmapfreq_secondod<- function(t){
  tf <- t(t(seconddiff[t,-1]))
  tf <- cbind(rownames(tf),tf)
  colnames(tf) <- c("Bus.Name","Frequency")
  bus_loc_freq_fod <- merge(bus_locs[ , ! colnames(bus_locs) %in% c("Frequency")],tf, by="Bus.Name")
  bus_locs$Frequency <- as.numeric(as.character(bus_loc_freq_fod$Frequency))
  # bus_locs <- update_freq_firstroc(t)
  
  if(!exists("autosc")){
    autosc <<- FALSE
  }
  if(!exists("is_zoom")){
    is_zoom <<- 0
  }
  # if(autosc == TRUE){
  #   fmin <- ifelse(min(bus_locs$Frequency)<59.9,min(bus_locs$Frequency),59.9)
  #   fmax <- ifelse(max(bus_locs$Frequency)>60.1,max(bus_locs$Frequency),60.1)
  #   #  vdiff <- (vmax-vmin)
  #   #   v_lab <- c(vmin,(vmin+(vdiff/4)),(vmin+(vdiff/2)),(vmax-(vdiff/4)),vmax)
  # } else{
  #   fmin <- 59.8
  #   fmax <- 60.2
  # }
  fmin <- ifelse(min(bus_locs$Frequency)<(-0.001),min(bus_locs$Frequency),(-0.001))
  fmax <- ifelse(max(bus_locs$Frequency)>0.001,max(bus_locs$Frequency),0.001)
  xmn <- min(bus_locs$Longitude)
  xmx <- max(bus_locs$Longitude)
  ymn <- min(bus_locs$Latitude)
  ymx <- max(bus_locs$Latitude)
  
  xstep <- (xmx-xmn)/80
  ystep <- (ymx-ymn)/80
  intp_coords <- interp(bus_locs$Longitude, bus_locs$Latitude, bus_locs$Frequency, duplicate = "mean",
                        xo=seq(xmn,xmx, by=xstep),
                        yo=seq(ymn,ymx, by=ystep))
  r <- raster(intp_coords)
  rtp <- rasterToPolygons(r)
  
  rtp@data$id <- 1:nrow(rtp@data)   # add id column for join
  rtpFort <- fortify(rtp, data = rtp@data)
  rtpFortMer <- merge(rtpFort, rtp@data, by.x = 'id', by.y = 'id')  # join data
  if (fmin<59.8 & fmax <= 60.2) {
    f_cols <-c("red","yellow","orange","blue","green")
  } else if(fmax <60.2 & fmin >= 59.8){
    f_cols <-c("green","blue","orange","yellow","red")
  } else{
    f_cols <-c("red","yellow","green","blue","black")
  }
  g <- g + geom_polygon(data = rtpFortMer, 
                        aes(x = long, y = lat, group = group, fill = layer), 
                        alpha = 1, 
                        size = 0,
                        color = NA) +  
    scale_fill_gradientn("Second Order",colours = f_cols,limits=c(fmin,fmax))+
    theme(legend.position="right",legend.direction="vertical",legend.box="horizontal")
  #   ggtitle(bquote(atop("Frequency at Time",atop(.(Freq[t,1]),""))))
  g
}

plot_heatmaps_ind <- function(datafile,interestpoint){
  source(datafile)
  import_data()
  
  firstdiff <<- firstorder_diff(Freq)
  firstroc <<- firstorder_roc(Freq)
  seconddiff <<- firstorder_diff(as.data.frame(firstdiff))
  filename <- paste(gsub(pattern = "\ ","",name()),interestpoint,"freq",sep = "_")

  # plot_heatmapfreq(interestpoint)
  # ggsave(paste(filename,".png",sep = ""))
  
  postscript(file = paste(filename, '_stacked.png', sep=""))
  print(multiplot(plot_heatmapfreq(1055),plot_heatmapfreq_firstroc(1055),plot_heatmapfreq_firstod(1055),plot_heatmapfreq_secondod(1055)))
  dev.off()
  
  #ggsave(filename = paste(filename,"_stacked.png",sep = ""),plot = plot_heatmapangle_firstod(1055),device = "png")

  postscript(file = paste(filename, '.eps', sep=""))
  print(plot_heatmapfreq(interestpoint))
  dev.off()

  postscript(file = paste(filename, '_firstorder.eps', sep=""))
  print(plot_heatmapfreq_firstod(interestpoint))
  dev.off()
  
  postscript(file = paste(filename, '_rateofchange.eps', sep=""))
  print(plot_heatmapfreq_firstroc(interestpoint))
  dev.off()
  
  postscript(file = paste(filename, '_secondorder.eps', sep=""))
  print(plot_heatmapfreq_secondod(interestpoint))
  dev.off()
  
  
  firstdiff <<- firstorder_diff(Volt)
  firstroc <<- firstorder_roc(Volt)
  seconddiff <<- firstorder_diff(as.data.frame(firstdiff))
  filename <- paste(gsub(pattern = "\ ","",name()),interestpoint,"volt",sep = "_")
  
  postscript(file = paste(filename, '.eps', sep=""))
  print(plot_heatmapvolt(interestpoint))
  dev.off()
  
  # plot_heatmapvolt(interestpoint)
  # ggsave(paste(filename,".png",sep = ""))
  postscript(file = paste(filename, '_firstorder.eps', sep=""))
  print(plot_heatmapvolt_firstod(interestpoint))
  dev.off()

  postscript(file = paste(filename, '_rateofchange.eps', sep=""))
  print(plot_heatmapvolt_firstod(interestpoint))
  dev.off()
  
  postscript(file = paste(filename, '_secondorder.eps', sep=""))
  print(plot_heatmapangle_secondod(interestpoint))
  dev.off()

  firstdiff <<- firstorder_diff(Pangle)
  firstroc <<- firstorder_roc(Pangle)
  seconddiff <<- firstorder_diff(as.data.frame(firstdiff))
  filename <- paste(gsub(pattern = "\ ","",name()),interestpoint,"angle",sep = "_")
# 
#   setEPS()
#   postscript(paste(filename,".eps",sep = ""))
#   plot_heatmapangle(interestpoint)
#   dev.off()
#   
  postscript(file = paste(filename, '.eps', sep=""))
  print(plot_heatmapangle(interestpoint))
  dev.off()
  # 
  # plot_heatmapangle(interestpoint)
  # ggsave(paste(filename,".png",sep = ""))
  postscript(file = paste(filename, '_firstorder.eps', sep=""))
  print(plot_heatmapangle_firstod(interestpoint))
  dev.off()
  
  postscript(file = paste(filename, '_secondorder.eps', sep=""))
  print(plot_heatmapangle_secondod(interestpoint))
  dev.off()
  
  postscript(file = paste(filename, '_rateofchange.eps', sep=""))
  print(plot_heatmapangle_firstroc(interestpoint))
  dev.off()
 # ggsave(filename = paste(filename,"_rateofchange.png",sep = ""),plot = plot_heatmapangle_firstod(interestpoint),device = "png")
 # dev.off()
}

plot_heatmaps_stacked <- function(datafile,interestpoint){
  #heatmaps are in order of raw/rate of change/first order/second order
  source(datafile)
  import_data()
  
  firstdiff <<- firstorder_diff(Freq)
  firstroc <<- firstorder_roc(Freq)
  seconddiff <<- firstorder_diff(as.data.frame(firstdiff))
  filename <- paste(gsub(pattern = "\ ","",name()),interestpoint,"freq",sep = "_")
  
  # plot_heatmapfreq(interestpoint)
  # ggsave(paste(filename,".png",sep = ""))
  
  
  postscript(file = paste(filename, '_heat_stacked.eps', sep=""))
  print(multiplot(plot_heatmapfreq(interestpoint),
                  plot_heatmapfreq_firstod(interestpoint),
                  plot_heatmapfreq_firstroc(interestpoint),
                  plot_heatmapfreq_secondod(interestpoint)))
  dev.off()
  
  
  firstdiff <<- firstorder_diff(Volt)
  firstroc <<- firstorder_roc(Volt)
  seconddiff <<- firstorder_diff(as.data.frame(firstdiff))
  filename <- paste(gsub(pattern = "\ ","",name()),interestpoint,"volt",sep = "_")
  
  postscript(file = paste(filename, '_heat_stacked.eps', sep=""))
  print(multiplot(plot_heatmapvolt(interestpoint),
                  plot_heatmapvolt_firstroc(interestpoint),
                  plot_heatmapvolt_firstod(interestpoint),
                  plot_heatmapvolt_secondod(interestpoint)))
  dev.off()
  
  firstdiff <<- firstorder_diff(Pangle)
  firstroc <<- firstorder_roc(Pangle)
  seconddiff <<- firstorder_diff(as.data.frame(firstdiff))
  filename <- paste(gsub(pattern = "\ ","",name()),interestpoint,"angle",sep = "_")
  
  postscript(file = paste(filename, '_heat_stacked.eps', sep=""))
  print(multiplot(plot_heatmapangle(interestpoint),
                  plot_heatmapangle_firstroc(interestpoint),
                  plot_heatmapangle_firstod(interestpoint),
                  plot_heatmapangle_secondod(interestpoint)))
  dev.off()
}

plot_heatmaps_stacked("data/import_icestorm.R",100)
plot_heatmaps_stacked("data/import_icestorm2.R",100)
plot_heatmaps_stacked("data/import_mcnary.R",485)
plot_heatmaps_stacked("data/import_openac.R",153)
plot_heatmaps_stacked("data/import_opendc.R",255)
plot_heatmaps_stacked("data/import_opengen.R",220)
plot_heatmaps_stacked("data/import_opengen2.R",265)
plot_heatmaps_stacked("data/import_ponderosa.R",604)
plot_heatmaps_stacked("data/import_quake1.R",153)
plot_heatmaps_stacked("data/import_quake2.R",2426)
plot_heatmaps_stacked("data/import_gmd2.R",1055)


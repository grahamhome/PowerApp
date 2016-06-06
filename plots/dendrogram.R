
#library(gputools)
library(dendextend)
source("data/import_ts.R")
import_data()

fnames <- function(){
  n <- list(Dendrogram="dendrogram",
            Voltage="plot_dendvolt",
            Frequency="plot_dendfreq")
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

plot_dendvolt <- function(t){
  update_volt(t)
  b <- subset(bus_locs, select = c("Bus.Name","Voltage"))
  b <- b[order(b$Voltage),]
  rownames(b) <- b$Bus.Name
  #highlight <<-c(rownames(tail(b,5)))
  
  gp <- gpuDist(b$Voltage)
  gphc <- gpuHclust(gp)
  gphc$labels <- b$Bus.Name
 # gpc <- gpuDistClust(b$Voltage,clustmethod = "ward")
 # gpc$labels <- b$Bus.Name

  #dd <- dendrapply(as.dendrogram(gpc), colorLeafs)
  #dd$labels <- b$Bus.Name
  dd <- dendrapply(as.dendrogram(gphc), colorLeafs)
  
  plot(dd,main=paste("Voltage at time",t,sep = " "))
}

plot_dendfreq <- function(t){
  update_freq(t)
  b <- subset(bus_locs, select = c("Bus.Name","Frequency"))
  #gp <- gpuDist(b$Frequency)
 # gphc <- gpuHclust(gp)
 # gphc$labels <- b$Bus.Name
  gpc <- gpuDistClust(b$Frequency)
  gpc$labels <- b$Bus.Name
  
  plot(gpc,cex=0.5,main=paste("Frequency at time",t,sep = " "))
}
## function to change color etc. of a leaf
colorLeafs <- function(x) {
  if (is.leaf(x) && attr(x, "label") %in% highlight) {
    attr(x, "nodePar") <- list(lab.col="red", pch=NA)
  }
  return(x)
}

# gpc <- gpuDistClust(b$Voltage)

# mycl <- cutree(gphc, h=max(gphc$height/1.5))
# clusterCols <- rainbow(length(unique(mycl)))
# myClusterSideBar <- clusterCols[mycl]
# myheatcol <- rev(col(75))
# heatmap(as.matrix(b), main="Hierarchical Cluster", Rowv=as.dendrogram(gphc), Colv=NA, dendrogram="row", scale="row", col=myheatcol, density.info="none", trace="none", RowSideColors= myClusterSideBar)





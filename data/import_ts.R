library(ggmap)

#reads in all the data from the csv files
#dpath is the path to the data folder (in case we are calling this from somewhere outside of the data folder)
get_csvdata_ts <- function(dpath){
  buses <<- read.csv(paste(dpath,"rawdata/tenn150_buses.csv",sep = ""))
  substat <<- read.csv(paste(dpath,"rawdata/tenn150_substations.csv",sep = ""))
  loads <<- read.csv(paste(dpath,"rawdata/tenn150_loads.csv",sep = ""))
  generators <<- read.csv(paste(dpath,"rawdata/tenn150_generators.csv",sep = ""))
  linesb <<- read.csv(paste(dpath,"rawdata/tenn150_lines.csv",sep = ""))
  trans <<- read.csv(paste(dpath,"rawdata/tenn150_transformers.csv",sep = ""))
  Volt <<- read.csv(paste(dpath,"rawdata/tenn150ts_bus_voltage.csv",sep = ""))
  Freq <<- read.csv(paste(dpath,"rawdata/tenn150ts_bus_frequency.csv",sep = ""))
}
#Cleans up the names for various parts of the data so that it all matches up
clean_names_ts  <- function(){
  bn <- buses$Bus.Name
  bn <- gsub(" ",".",bn)
  buses$Bus.Name <<- bn
  bn <- linesb$To.Bus.Name
  bn <- gsub(" ",".",bn)
  linesb$To.Bus.Name <<- bn
  bn <- linesb$From.Bus.Name
  bn <- gsub(" ",".",bn)
  linesb$From.Bus.Name <<- bn
  
  #replace names in Frequency table with just bus names 
  cn <- colnames(Freq[,-1])
  cn <- gsub("Bus[.]","",cn)
  cn <- gsub("[.]Frequency","",cn)
  cn <- gsub("[.][.]",".",cn)
  cn <- gsub("Gallatin[.]TN[.]","Gallatin.(TN)",cn)
  colnames(Freq) <<- c("Time",cn)
  #replace names in Voltage table with just bus names 
  cn <- colnames(Volt[,-1])
  cn <- gsub("Bus[.]","",cn)
  cn <- gsub("[.]V[.]pu","",cn)
  cn <- gsub("[.][.]",".",cn)
  cn <- gsub("Gallatin[.]TN[.]","Gallatin.(TN)",cn)
  colnames(Volt) <<- c("Time",cn)
}
#Sets up all of the data structures containing parts of multiple input files, and adds the 
# info to the appropriate data structure
get_merged_data_ts  <- function(){
  sub_buses <<- merge(buses,substat,by = "Sub.ID")
  
  #Set up dataframe with each bus name, bus number, the name of the substation it's at,
  # the lat/long of the station, the frequency (at time=0), and the voltage (at time=0)
  bus_locs <<- data.frame(sub_buses$Bus.Num,sub_buses$Bus.Name,sub_buses$Sub.Name.x,sub_buses$Latitude,sub_buses$Longitude,"Frequency","Voltage")
  colnames(bus_locs) <<- c("Bus.Num","Bus.Name","Sub.Name", "Latitude","Longitude","Frequency","Voltage")
  bus_locs$Frequency <<- 0
  bus_locs$Voltage <<- 0 
  
  
  #Add the from/to bus latitudes/longitudes to the line data
  linesb <<- merge(linesb,bus_locs[,c("Bus.Num","Latitude","Longitude")], by.x = "From.Bus.Num", by.y = "Bus.Num")
  linesb <<- merge(linesb,bus_locs[,c("Bus.Num","Latitude","Longitude")], by.x = "To.Bus.Num", by.y = "Bus.Num")
  cn <- colnames(linesb)
  cn <- gsub("Latitude[.]x","From.Latitude",cn)
  cn <- gsub("Longitude[.]x","From.Longitude",cn)
  cn <- gsub("Latitude[.]y","To.Latitude",cn)
  cn <- gsub("Longitude[.]y","To.Longitude",cn)
  colnames(linesb) <<- cn
  linesb$Variance <<- 0
  #linesb$Line.id <- seq_len(nrow(linesb))
}
#Set up the map and ggmap object
get_map_data_ts  <- function(){
  #Create the map to use as the background for the ggplot
  mapten <<- get_map(location = c(lon = mean(bus_locs$Longitude), lat = mean(bus_locs$Latitude)), 
                     zoom = 6, maptype = "roadmap", scale = 2)
  g <<- ggmap(mapten)+ scale_x_continuous(limits = c(-90.6, -81.3), expand = c(0, 0)) + scale_y_continuous(limits = c(34.5, 37), expand = c(0, 0))
  
}
#Set up the various data structures for the covariance matrix stuff
get_covvariance_data_ts  <- function(){
  #Set up the variables for the voltage covariance matrix
  Xv <<- as.matrix(Volt[,-1])
  Sv <<- cov(Xv[1:2,])
  curr_sv <<- 3
  xvbar <<- colMeans(Xv[1:2,])
  #Set up these variables for the frequency covariance matrix
  Xf <<- as.matrix(Freq[,-1])
  Sf <<- cov(Xf[1:2,])
  curr_sf <<- 3
  xfbar <<- colMeans(Xf[1:2,])
}



import_data <- function(){
#import_ts <- function(dpath){
  #source(paste(dpath,"import_ts.R",sep = ""))
  get_csvdata_ts("data/")
  clean_names_ts()
  get_merged_data_ts()
  get_covvariance_data_ts()
  get_map_data_ts()
}

#Returns a list of the plots that this data can be used to create
use_plots <- function(){
  list('linear.R','map.R','heatmap.R','correlation.R','bar.R')
}

name <- function(){
  n <- "Line and Generator Down"
  n
}


n_samples <- function(){
  nrow(Freq)
}




#This finds the min/max covariances over the whole dataset, for labeling purposes
#mincovf <- 1
#maxcovf <- 0
#for (t in 3:nrow(F)) {
#  get_covbus_freq(t)
#  mincovf <- ifelse(min(Sf[,])<mincovf,min(Sf[,]),mincovf)
#  maxcovf <- ifelse(max(Sf[,])>maxcovf,max(Sf[,]),maxcovf)
#}
#mincovv <- 1
#maxcovv <- 0
#for (t in 3:nrow(V)) {
#  get_covbus_volt(t)
#  mincovv <- ifelse(min(Sv[,])<mincovv,min(Sv[,]),mincovv)
#  maxcovv <- ifelse(max(Sv[,])>maxcovv,max(Sv[,]),maxcovv)
#}
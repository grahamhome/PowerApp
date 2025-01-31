library(ggmap)

#Read in all of the csv data files
get_csvdata_wecc <- function(dpath){
  branch_loads <<- read.csv(paste(dpath,"rawdata/wecc2000may2014_branch_percentloading.csv",sep = ""))
  buses <<- read.csv(paste(dpath,"rawdata/wecc2000may2014_buses.csv",sep = ""))
  substations <<- read.csv(paste(dpath,"rawdata/wecc2000may2014_substations.csv",sep = ""))
  Freq <<- read.csv(paste(dpath,"rawdata/wecc2000may2014_bus_frequency.csv",sep = ""),stringsAsFactors = FALSE)
  Pangle <<- read.csv(paste(dpath,"rawdata/wecc2000may2014_bus_vang.csv",sep = ""),stringsAsFactors = FALSE)
  Volt <<- read.csv(paste(dpath,"rawdata/wecc2000may2014_bus_vpu.csv",sep = ""),stringsAsFactors = FALSE)
}


#Change up the names so that they all match
clean_names_wecc <- function(){
  # ndn <- as.matrix(branches[1,])
  # colnames(branches) <<- ndn
  # branches <<- branches[-1,]
  
  
  fn <- colnames(Freq)
  fn <- gsub("Bus[.]","",fn)
  fn <- gsub("[.][.]Frequency","",fn)
  colnames(Freq) <<- fn
  
  pn <- colnames(Pangle)
  pn <- gsub("Bus[.]","",pn)
  pn <- gsub("[.][.]V[.]angle","",pn)
  colnames(Pangle) <<- pn
  
  vn <- colnames(Volt)
  vn  <- gsub("Bus[.]","",vn)
  vn  <- gsub("[.][.]V[.]pu","",vn)
  colnames(Volt) <<- vn
  
  bus_locs <<- data.frame()
}

#Create all of the merged data frames that will be used by the plotting functions
get_merged_data_wecc <- function(){
  sub_buses <<- merge(buses,substations,by = c("Sub.Num","Sub.ID","Sub.Name"))
  
  bus_locs_full <<- data.frame(sub_buses$Bus.Num,sub_buses$Bus.Name,sub_buses$Sub.Name,sub_buses$Latitude,sub_buses$Longitude,sub_buses$Nominal.kV.max.,"Frequency","Voltage","Angle")
  colnames(bus_locs_full) <<- c("Bus.Num","Bus.Name","Sub.Name", "Latitude","Longitude","Nominal.kV.max","Frequency","Voltage","Angle")
  bus_locs_full$Frequency <<- 0
  bus_locs_full$Voltage <<- 0 
  bus_locs_full$Angle <<- 0
  #Remove buses with no long/lat coordinates
  bus_locs_full<<-bus_locs_full[!(bus_locs_full$Latitude=="" | bus_locs_full$Longitude=="" |is.na(bus_locs_full$Longitude)|is.na(bus_locs_full$Latitude)),]
  bus_locs_full$Longitude <<- as.numeric(as.character(bus_locs_full$Longitude))
  bus_locs_full$Latitude <<- as.numeric(as.character(bus_locs_full$Latitude))
  
  bus_locs_full$New.Bus.Name <<- paste(bus_locs_full$Bus.Num,bus_locs_full$Bus.Name)
  
  
  # missing_pmu <- colnames(Volt)[!(colnames(Volt) %in% bus_locs_full$Bus.Name)][-1]

  #Add new column with name formatted like it is in the Freq/Volt/Pangle tables
  bus_locs_full$New.Bus.Name <<- ""
  for (x in 1:nrow(bus_locs_full[,])) {
    cr <- bus_locs_full[x,]
    tbn <- cr$Bus.Name
    tbn <- gsub(" ",".",tbn)
    bus_locs_full[x,"New.Bus.Name"] <<- as.character(paste(cr$Bus.Num,tbn,sep = ".."))
  }
  #bus_locs contains just the buses that have PMU readings
  bus_locs <<- bus_locs_full[bus_locs_full$New.Bus.Name %in% colnames(Freq),]
  bus_locs$Old.Bus.Name <<- bus_locs$Bus.Name
  bus_locs$Bus.Name <<- bus_locs$New.Bus.Name
#  bus_locs <<- subset(bus_locs,select=c("Bus.Num","Bus.Name","Sub.Name","Latitude","Longitude","Nominal.kV","Frequency",
 #                                       "Voltage","Angle","Old.Bus.Name"))
  
}


#Create the map and ggmap to be used by the plot functions (the map ones at least)
get_map_data_wecc  <- function(){
  #Create the map to use as the background for the ggplot
  mapten <<- get_map(location = c(lon = mean(bus_locs$Longitude), lat = mean(bus_locs$Latitude)), zoom = 4, maptype = "roadmap", scale = 2)
  #maplocs <<- get_map(location = c(min(bus_locs$Longitude), min(bus_locs$Latitude),
  #                                 max(bus_locs$Longitude),max(bus_locs$Latitude)),
  #                     maptype = "roadmap")
  map_lims <<- c(-124, -104,31, 50) #xmin,xmax,ymin,ymax
  m_ratio <<- abs(map_lims[2]-map_lims[1])/abs(map_lims[4]-map_lims[3])
  g <<- ggmap(mapten) +
    # coord_fixed(xlim = c(map_lims[1], map_lims[2]),ylim = c(map_lims[3], map_lims[4]),expand = FALSE)
    scale_x_continuous(limits = c(-124, -104), expand = c(0, 0)) +
    scale_y_continuous(limits = c(31, 50), expand = c(0, 0))
}

import_data <- function(){
  get_csvdata_wecc("data/")
  clean_names_wecc()
  get_merged_data_wecc()
  get_map_data_wecc()
}

#Name of the data set
name <- function(){
  n <- "May 2014 Scenario"
  n
}
#How many time points is the data
nsamples <- function(){
  nrow(Freq)
}

#Returns a list of the plots that this data can be used to create
use_plots <- function(){
  list('linear.R','map.R','heatmap.R','bar.R','histogram.R')
  #list()
}

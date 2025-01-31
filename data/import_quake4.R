library(ggmap)


#Read in all of the csv data files
get_csvdata_quake4 <- function(){
  #branches <<- read.csv(paste(dpath,"data/rawdata/wecc2000may2014_branch_percentloading.csv",sep = ""))
  buses <<- read.csv("data/rawdata/weccBPA138_quake4_buses.csv")
  pmus <<- read.csv("data/rawdata/weccBPA138_quake4_pmus.csv")
  substations <<- read.csv("data/rawdata/weccBPA138_quake4_subs.csv")
  Freq <<- read.csv("data/rawdata/weccBPA138_quake4_bus_frequency.csv",stringsAsFactors = FALSE)
  Pangle <<- read.csv("data/rawdata/weccBPA138_quake4_bus_vangle.csv",stringsAsFactors = FALSE)
  Volt <<- read.csv("data/rawdata/weccBPA138_quake4_bus_vmag.csv",stringsAsFactors = FALSE)
  pmus <<- pmus[!(pmus$Latitude < 40),]
}


#Change up the names so that they all match
clean_names_quake4 <- function(){

  fn <- colnames(Freq)
  fn <- gsub("Bus[.]","",fn)
  fn <- gsub("[.]Frequency","",fn)
  colnames(Freq) <<- fn
  
  pn <- colnames(Pangle)
  pn <- gsub("Bus[.]","",pn)
  pn <- gsub("[.]V[.]angle","",pn)
  colnames(Pangle) <<- pn
  
  vn <- colnames(Volt)
  vn  <- gsub("Bus[.]","",vn)
  vn  <- gsub("[.]V[.]pu","",vn)
  colnames(Volt) <<- vn
  
  bus_locs <<- data.frame()
}


#Create all of the merged data frames that will be used by the plotting functions
get_merged_data_quake4 <- function(){
  sub_buses <<- merge(buses,substations,by = c("Sub.ID","Sub.Name"))
  
  bus_locs_full <<- data.frame(sub_buses$Bus.Number,sub_buses$Bus.Name,sub_buses$Sub.Name,sub_buses$Sub.ID,sub_buses$X..of.Buses,sub_buses$Latitude,sub_buses$Longitude,"Frequency","Voltage","Angle")
  colnames(bus_locs_full) <<- c("Bus.Num","Bus.Name","Sub.Name","Sub.ID","Sub.Num.Buses", "Latitude","Longitude","Frequency","Voltage","Angle")
  bus_locs_full$Frequency <<- 0
  bus_locs_full$Voltage <<- 0 
  bus_locs_full$Angle <<- 0
  #Remove buses with no long/lat coordinates
 # bus_locs_full<<-bus_locs_full[!(bus_locs_full$Latitude=="" | bus_locs_full$Longitude==""),]
  bus_locs_full$Longitude <<- as.numeric(as.character(bus_locs_full$Longitude))
  bus_locs_full$Latitude <<- as.numeric(as.character(bus_locs_full$Latitude))
  
  
  
  #missing_pmu <- colnames(Volt)[!colnames(Volt) %in% bus_locs_full$New.Bus.Name][-1]
  #Removing these pmu columns because there is no bus corresponding to these
  # Freq$`40598 (KEELER E)` <<- NULL
  # Freq$`44284 (LONGHORN)` <<- NULL
  # Volt$`40598 (KEELER E)` <<- NULL
  # Volt$`44284 (LONGHORN)` <<- NULL
  # Pangle$`40598 (KEELER E)` <<- NULL
  # Pangle$`44284 (LONGHORN)` <<- NULL
  
  #Add new column with name formatted like it is in the Freq/Volt/Pangle tables
  bus_locs_full$New.Bus.Name <<- ""
  for (x in 1:nrow(bus_locs_full[,])) {
    cr <- bus_locs_full[x,]
    tn <- cr$Bus.Name
    tn <- gsub(" ",".",tn)
    #cr$Bus.Name <- as.character(paste(cr$Bus.Num,paste("(",cr$Bus.Name,")",sep = ""),sep = " "))
    bus_locs_full[x,"New.Bus.Name"] <<- as.character(tn)
    # pmu_buses <- rbind(pmu_buses[1:])
  }
  #bus_locs contains just the buses that have PMU readings
  bus_locs <<- bus_locs_full[bus_locs_full$Bus.Num %in% pmus$Bus.Number,]
  bus_locs$Old.Bus.Name <<- bus_locs$Bus.Name
  bus_locs$Bus.Name <<- bus_locs$New.Bus.Name
#  bus_locs <<- subset(bus_locs,select=c("Bus.Num","Bus.Name","Sub.Name","Latitude","Longitude","Frequency",
 #                                       "Voltage","Angle","Old.Bus.Name"))
 
}

#Create the map and ggmap to be used by the plot functions (the map ones at least)
get_map_data_quake4  <- function(){
  #Create the map to use as the background for the ggplot
  mapten <<- get_map(location = c(lon = mean(bus_locs$Longitude), lat = mean(bus_locs$Latitude)), zoom = 4, maptype = "toner",source = "stamen", scale = 2)
  
  map_lims <<- c(-124, -112,40, 50) #xmin,xmax,ymin,ymax
  m_ratio <<- abs(map_lims[2]-map_lims[1])/abs(map_lims[4]-map_lims[3])
  g <<- ggmap(mapten) +
    # coord_fixed(xlim = c(map_lims[1], map_lims[2]),ylim = c(map_lims[3], map_lims[4]),expand = FALSE)
    scale_x_continuous(limits = c(-124, -112), expand = c(0, 0)) +
    scale_y_continuous(limits = c(40, 50), expand = c(0, 0))
}

#Call all the functions in order
import_data <- function(){
  get_csvdata_quake4()
  clean_names_quake4()
  get_merged_data_quake4()
  get_map_data_quake4()
}

#Name of the data set
name <- function(){
  n <- "WECC Earthquake 4"
  n
}
#How many time points is the data
nsamples <- function(){
  nrow(Freq)
}

#Returns a list of the plots that this data can be used to create
use_plots <- function(){
  list('linear.R','map.R','heatmap.R','bar.R','histogram.R')
}



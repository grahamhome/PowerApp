library(ggmap)


#Read in all of the csv data files
get_csvdata_droploads2 <- function(){
  buses <<- read.csv("data/rawdata/BPA_230kV_data/buses.csv")
  substations <<- read.csv("data/rawdata/BPA_230kV_data/subs.csv")
  bus_info <<- read.csv("data/rawdata/BPA_230kV_data/bus_info.csv")
  pmus <<- read.csv("data/rawdata/BPA_230kV_data/pmu_buses.csv")
  gens <<- read.csv("data/rawdata/BPA_230kV_data/gens.csv")
  Freq <<- read.csv("data/rawdata/BPA_230kV_data/weccbpaabove230_drop_load2_pmu_freq_bad.csv")
  Pangle <<- read.csv("data/rawdata/BPA_230kV_data/weccbpaabove230_drop_load2_pmu_vang_bad.csv")
  Volt <<- read.csv("data/rawdata/BPA_230kV_data/weccbpaabove230_drop_load2_pmu_vmag_bad.csv")
}


#Change up the names so that they all match
clean_names_droploads2  <- function(){
  
  fn <- colnames(Freq)
  fn <- gsub("X","",fn)
  #  fn <- gsub("[.]Frequency","",fn)
  colnames(Freq) <<- fn
  
  pn <- colnames(Pangle)
  pn <- gsub("X","",pn)
  #pn <- gsub("[.]V[.]angle","",pn)
  colnames(Pangle) <<- pn
  
  vn <- colnames(Volt)
  vn  <- gsub("X","",vn)
  #  vn  <- gsub("[.]V[.]pu","",vn)
  colnames(Volt) <<- vn
  
  
  bus_locs <<- data.frame()
}


#Create all of the merged data frames that will be used by the plotting functions
get_merged_data_droploads2  <- function(){
  # sub_buses <<- merge(buses,substations,by = c("Sub.ID","Sub.Name"))
  
  bus_locs_full <<- data.frame(pmus$Bus.Number,pmus$Bus.Name,pmus$Sub.Name,pmus$Latitude,pmus$Longitude,"Frequency","Voltage","Angle")
  colnames(bus_locs_full) <<- c("Bus.Name","Bus.Name.unused","Sub.Name", "Latitude","Longitude","Frequency","Voltage","Angle")
  bus_locs_full$Frequency <<- 0
  bus_locs_full$Voltage <<- 0 
  bus_locs_full$Angle <<- 0
  #Remove buses with no long/lat coordinates
  bus_locs_full<<-bus_locs_full[!(bus_locs_full$Latitude=="" | bus_locs_full$Longitude=="" | is.na(bus_locs_full$Latitude) | is.na(bus_locs_full$Longitude)),]
  bus_locs_full$Longitude <<- as.numeric(as.character(bus_locs_full$Longitude))
  bus_locs_full$Latitude <<- as.numeric(as.character(bus_locs_full$Latitude))
  
  
  
  # missing_pmu <- colnames(Volt)[!colnames(Volt) %in% bus_locs_full$Bus.Num][-1]
  
  #bus_locs contains just the buses that have PMU readings
  #bus_locs <<- bus_locs_full[bus_locs_full$Bus.Num %in% pmus$Bus.Number,]
  bus_locs <<- bus_locs_full[bus_locs_full$Bus.Name %in% colnames(Freq),]
  bus_locs$Longitude <<- as.numeric(as.character(bus_locs$Longitude))
  bus_locs$Latitude <<- as.numeric(as.character(bus_locs$Latitude))
  
}

#Create the map and ggmap to be used by the plot functions (the map ones at least)
get_map_data_droploads2  <- function(){
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

#Call all the functions in order
import_data <- function(){
  get_csvdata_droploads2()
  clean_names_droploads2()
  get_merged_data_droploads2()
  get_map_data_droploads2()
}

#Name of the data set
name <- function(){
  n <- "Dropped Load 2"
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
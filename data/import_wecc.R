library(ggmap)

get_csvdata_wecc <- function(dpath){
  branches <<- read.csv(paste(dpath,"rawdata/WECC_2000Eqv_Network_Data_branches.csv",sep = ""))
  buses <<- read.csv(paste(dpath,"rawdata/WECC_2000Eqv_Network_Data_buses.csv",sep = ""))
  substations <<- read.csv(paste(dpath,"rawdata/WECC_2000Eqv_Network_Data_substations.csv",sep = ""))
  Freq <<- read.csv(paste(dpath,"rawdata/WECC_2000Eqv_Scenario_Results.csv",sep = ""))
  Pangle <<- read.csv(paste(dpath,"rawdata/WECC_2000Eqv_Scenario_Results_1.csv",sep = ""))
  Volt <<- read.csv(paste(dpath,"rawdata/WECC_2000Eqv_Scenario_Results_2.csv",sep = ""))
  sr_3 <<- read.csv(paste(dpath,"rawdata/WECC_2000Eqv_Scenario_Results_3.csv",sep = ""))
  sr_4 <<- read.csv(paste(dpath,"rawdata/WECC_2000Eqv_Scenario_Results_4.csv",sep = ""))
  sr_5 <<- read.csv(paste(dpath,"rawdata/WECC_2000Eqv_Scenario_Results_5.csv",sep = ""))
  sr_6 <<- read.csv(paste(dpath,"rawdata/WECC_2000Eqv_Scenario_Results_6.csv",sep = ""))
}

clean_names_wecc <- function(){
  ndn <- as.matrix(branches[1,])
  colnames(branches) <<- ndn
  branches <<- branches[-1,]
  
  bn <- as.matrix(buses[1,])
  colnames(buses) <<- bn
  buses <<- buses[-1,]
  
  sn <- as.matrix(substations[1,])
  colnames(substations) <<- sn
  substations <<- substations[-1,]
  
  fn <- as.matrix(Freq[1,])
  fn <- gsub("Bus ","",fn)
  fn <- gsub(" Frequency","",fn)
  colnames(Freq) <<- fn
  Freq <<- Freq[-1,]
  
  pn <- as.matrix(Pangle[1,])
  pn <- gsub("Bus ","",pn)
  pn <- gsub(" V angle","",pn)
  colnames(Pangle) <<- pn
  Pangle <<- Pangle[-1,]
  
  vn <- as.matrix(Volt[1,])
  vn  <- gsub("Bus ","",vn)
  vn  <- gsub(" V pu","",vn)
  colnames(Volt) <<- vn
  Volt <<- Volt[-1,]
  
  sn <- as.matrix(sr_3[1,])
  colnames(sr_3) <<- sn
  sr_3 <<- sr_3[-1,]
  
  sn <- as.matrix(sr_4[1,])
  colnames(sr_4) <<- sn
  sr_4 <<- sr_4[-1,]
  
  sn <- as.matrix(sr_5[1,])
  colnames(sr_5) <<- sn
  sr_5 <<- sr_5[-1,]
  
  sn <- as.matrix(sr_6[1,])
  colnames(sr_6) <<- sn
  sr_6 <<- sr_6[-1,]
  
  bus_locs <<- data.frame()
}

get_merged_data_wecc <- function(){
  sub_buses <<- merge(buses,substations,by = c("Sub Num","Sub ID","Sub Name"))
  
  bus_locs_full <<- data.frame(sub_buses$Number,sub_buses$Name,sub_buses$`Sub Name`,sub_buses$Latitude,sub_buses$Longitude,sub_buses$`Nom kV`,"Frequency","Voltage","Angle")
  colnames(bus_locs_full) <<- c("Bus.Num","Bus.Name","Sub.Name", "Latitude","Longitude","Nominal.kV","Frequency","Voltage","Angle")
  bus_locs_full$Frequency <<- 0
  bus_locs_full$Voltage <<- 0 
  bus_locs_full$Angle <<- 0
  #Remove buses with no long/lat coordinates
  bus_locs_full<<-bus_locs_full[!(bus_locs_full$Latitude=="" | bus_locs_full$Longitude==""),]
  bus_locs_full$Longitude <<- as.numeric(as.character(bus_locs_full$Longitude))
  bus_locs_full$Latitude <<- as.numeric(as.character(bus_locs_full$Latitude))
  
  #missing_pmu <- colnames(Volt)[!colnames(Volt) %in% bus_locs$Bus.Name][-1]
  #Removing these pmu columns because there is no bus corresponding to these
  Freq$`40598 (KEELER E)` <<- NULL
  Freq$`44284 (LONGHORN)` <<- NULL
  Volt$`40598 (KEELER E)` <<- NULL
  Volt$`44284 (LONGHORN)` <<- NULL
  Pangle$`40598 (KEELER E)` <<- NULL
  Pangle$`44284 (LONGHORN)` <<- NULL
  
  #Add new column with name formatted like it is in the Freq/Volt/Pangle tables
  bus_locs_full$New.Bus.Name <<- ""
  for (x in 1:nrow(bus_locs_full[,])) {
    cr <- bus_locs_full[x,]
    #cr$Bus.Name <- as.character(paste(cr$Bus.Num,paste("(",cr$Bus.Name,")",sep = ""),sep = " "))
    bus_locs_full[x,"New.Bus.Name"] <<- as.character(paste(cr$Bus.Num,paste("(",cr$Bus.Name,")",sep = ""),sep = " "))
    # pmu_buses <- rbind(pmu_buses[1:])
  }
  #bus_locs contains just the buses that have PMU readings
  bus_locs <<- bus_locs_full[bus_locs_full$New.Bus.Name %in% colnames(Freq),]
  bus_locs$Old.Bus.Name <<- bus_locs$Bus.Name
  bus_locs$Bus.Name <<- bus_locs$New.Bus.Name
  bus_locs <<- subset(bus_locs,select=c("Bus.Num","Bus.Name","Sub.Name","Latitude","Longitude","Nominal.kV","Frequency",
                                        "Voltage","Angle","Old.Bus.Name"))
  #Take all the lines from the branches table
  linesb_full <<- merge(branches[(branches$`Branch Device Type`=="Line"),],bus_locs_full[,c("Bus.Num","Bus.Name","Latitude","Longitude","New.Bus.Name")],by.x = c("From Number","From Name"),by.y = c("Bus.Num","Bus.Name"))
  ln <- colnames(linesb_full)
  ln <- gsub("Latitude","From.Latitude",ln)
  ln <- gsub("Longitude","From.Longitude",ln)
  colnames(linesb_full) <<- ln
  
  linesb_full <<- merge(linesb_full,bus_locs_full[,c("Bus.Num","Bus.Name","Latitude","Longitude","New.Bus.Name")],by.x = c("To Number","To Name"),by.y = c("Bus.Num","Bus.Name"))
  ln <- colnames(linesb_full)
  ln <- gsub("Latitude","To.Latitude",ln)
  ln <- gsub("Longitude","To.Longitude",ln)
  ln <- gsub("From.To.Latitude","From.Latitude",ln,fixed = TRUE)
  ln <- gsub("From.To.Longitude","From.Longitude",ln,fixed = TRUE)
 # ln <- gsub("Latitude","To Latitude",ln)
  ln <- gsub("New.Bus.Name.x","From.Bus.Name",ln)
  ln <- gsub("New.Bus.Name.y","To.Bus.Name",ln)
  colnames(linesb_full) <<- ln
  #linesb_full$Correlation <<- 0
  #linesb contains just the lines that go from one of the buses containing a PMU
  linesb <<- linesb_full[(linesb_full$To.Bus.Name %in% colnames(Freq))&(linesb_full$From.Bus.Name %in% colnames(Freq)),]
  #linesb <<- linesb_full[linesb_full$From.Bus.Name %in% colnames(Freq),]
  linesb$Correlation <<- 0
  

}

get_map_data_wecc  <- function(){
  #Create the map to use as the background for the ggplot
  maplocs <<- get_map(location = c(lon = mean(bus_locs$Longitude), lat = mean(bus_locs$Latitude)), zoom = 4, maptype = "roadmap", scale = 2)
  #maplocs <<- get_map(location = c(min(bus_locs$Longitude), min(bus_locs$Latitude),
  #                                 max(bus_locs$Longitude),max(bus_locs$Latitude)),
 #                     maptype = "roadmap")
  g <<- ggmap(maplocs) +
    scale_x_continuous(limits = c(-124, -104), expand = c(0, 0)) +
    scale_y_continuous(limits = c(31, 50), expand = c(0, 0))
}

get_covvariance_data_wecc  <- function(){
  #Set up the variables for the voltage covariance matrix
  Xv <<- data.matrix(Volt[,-1])
  Sv <<- cov(Xv[1:2,])
  curr_sv <<- 3
  xvbar <<- colMeans(Xv[1:2,])
  #Set up these variables for the frequency covariance matrix
  Xf <<- data.matrix(Freq[,-1])
  Sf <<- cov(Xf[1:2,])
  curr_sf <<- 3
  xfbar <<- colMeans(Xf[1:2,])
  #Set up these variables for the phase angle covariance matrix
  Xa <<- data.matrix(Pangle[,-1])
  Sa <<- cov(Xa[1:2,])
  curr_sa <<- 3
  xabar <<- colMeans(Xa[1:2,])
}


import_data <- function(){
  get_csvdata_wecc("data/")
  clean_names_wecc()
  get_merged_data_wecc()
  get_map_data_wecc()
  get_covvariance_data_wecc()
}

name <- function(){
  n <- "May 2014 Scenario"
  n
}

nsamples <- function(){
  nrow(Freq)
}

#Returns a list of the plots that this data can be used to create
use_plots <- function(){
  list('linear.R','map.R','heatmap.R','correlation.R','bar.R')
}



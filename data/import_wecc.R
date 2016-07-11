
dpath <- "data/"
get_csvdata_wecc <- function(dpath){
  network_data <<- read.csv(paste(dpath,"rawdata/WECC_2000Eqv_Network_Data.csv",sep = ""))
  Freq <<- read.csv(paste(dpath,"rawdata/WECC_2000Eqv_Scenario_Results.csv",sep = ""))
  Pangle <<- read.csv(paste(dpath,"rawdata/WECC_2000Eqv_Scenario_Results_1.csv",sep = ""))
  Volt <<- read.csv(paste(dpath,"rawdata/WECC_2000Eqv_Scenario_Results_2.csv",sep = ""))
  sr_3 <<- read.csv(paste(dpath,"rawdata/WECC_2000Eqv_Scenario_Results_3.csv",sep = ""))
  sr_4 <<- read.csv(paste(dpath,"rawdata/WECC_2000Eqv_Scenario_Results_4.csv",sep = ""))
  sr_5 <<- read.csv(paste(dpath,"rawdata/WECC_2000Eqv_Scenario_Results_5.csv",sep = ""))
  sr_6 <<- read.csv(paste(dpath,"rawdata/WECC_2000Eqv_Scenario_Results_6.csv",sep = ""))
}

clean_names_wecc <- function(){
  ndn <- as.matrix(network_data[1,])
  colnames(network_data) <<- ndn
  network_data <<- network_data[-1,]
  
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
}

import_data <- function(){
  get_csvdata_wecc("data/")
  clean_names_wecc()
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
  list('linear.R','bar.R')
}



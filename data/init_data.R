
#This is for the gmd data
#dpath: path to the data folder (should end with '/'; i.e. ../data/)
get_gmd_data <- function(dpath){
  source(paste(dpath,"import_gmd.R",sep = ""))
  get_csvdata_gmd(dpath)
  clean_names_gmd()
  get_merged_data_gmd()
  get_covvariance_data_gmd()
  get_map_data_gmd()
}
#This is for the ts data
#dpath: path to the data folder (should end with '/'; i.e. ../data/)
get_ts_data <- function(dpath){
  source(paste(dpath,"import_ts.R",sep = ""))
  get_csvdata_ts(dpath)
  clean_names_ts()
  get_merged_data_ts()
  get_covvariance_data_ts()
  get_map_data_ts()
}
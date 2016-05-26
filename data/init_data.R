
#This is for the gmd data
get_gmd_data <- function(){
  source("import_gmd.R")
  get_csvdata_gmd()
  clean_names_gmd()
  get_merged_data_gmd()
  get_covvariance_data_gmd()
  get_map_data_gmd()
}
#This is for the gmd data
get_ts_data <- function(){
  source("import_ts.R")
  get_csvdata_ts()
  clean_names_ts()
  get_merged_data_ts()
  get_covvariance_data_ts()
  get_map_data_ts()
}
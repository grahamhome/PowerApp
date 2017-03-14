
source("data/import_quake1.R")
import_data()
x <- nsamples()
setEPS()
postscript("quake1_voltage_roc_full.eps")
plot_voltage_diff(1,x)
dev.off()
setEPS()
postscript("quake1_voltage_raw_full.eps")
plot_voltage(1,x)
dev.off()
setEPS()
postscript("quake1_freq_roc_full.eps")
plot_freq_diff(1,x)
dev.off()
setEPS()
postscript("quake1_freq_raw_full.eps")
plot_frequency(1,x)
dev.off()
setEPS()
postscript("quake1_angle_roc_full.eps")
plot_angle_diff(1,x)
dev.off()
setEPS()
postscript("quake1_angle_raw_full.eps")
plot_pangle(1,x)
dev.off()

Dv <- as.matrix(Volt[,-1])
Cv <- Volt[1,-1]
currt_voltdiff <- 1
get_voltdiff <- function(time){
  currt_voltdiff <- time
  xt <- Dv[time,]
  xt1 <- ifelse(time == 1, Dv[1,], Dv[time-1,])
  numerator <- ifelse((xt-1)==0,xt,(xt-1))
  Cv[time,] <<- (xt-xt1)/numerator
}

update_voltdiff <- function(time){
  if(currt_voltdiff < time && currt_voltdiff > 1){
    tv <- currt_voltdiff
  }
  else{
    Cv <- Volt[1,]
    tv <- 1
  }
  for (n in tv:time) {
    xt <- Dv[n,]
    xt1 <- ifelse(n == 1, Dv[1,], Dv[n-1,])
    numerator <- ifelse((xt-1)==0,xt,(xt-1))
    Cv[n,] <- (xt-xt1)/numerator
  }
  assign("currt_voltdiff",time,envir = .GlobalEnv)
  assign("Cv",Cv,envir = .GlobalEnv)
}

update_voltdiff <- function(time){
  if(currt_voltdiff < time && currt_voltdiff >= 1){
    tv <- currt_voltdiff
  }
  else{
    Cv <- Volt[1,]
    tv <- 1
  }
  for (n in tv:time) {
    xt <- Dv[n,]
    if(n==1){
      xt1 <- Dv[1,]
    } else{
      xt1 <- Dv[n-1,]
    }
    #xt1 <- ifelse(n == 1, Dv[1,], Dv[n-1,])
    numerator <- ifelse((xt-1)==0,xt,(xt-1))
  #  print(xt)
   # print(xt1)
  #  print(numerator)
    Cv[n,-1] <- (xt-xt1)/numerator
    Cv[n,1] <- Volt[n,1]
  }
  assign("currt_voltdiff",time,envir = .GlobalEnv)
  assign("Cv",Cv,envir = .GlobalEnv)
}



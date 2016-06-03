#Tools for creating files.

library(foreach)
library(doParallel)
library(doSNOW)

#Sequentially creates a set of plot images for the given method in the given directory over the given range.
makeFiles1 <- function(start, stop, method) {
	path <- paste("plots/img/", method, "/", name(), "/", sep="")
	#Create directory for image files if it does not exist
	dir.create(file.path("plots/", "img"), showWarnings=FALSE)
	dir.create(file.path("plots/img/", method), showWarnings=FALSE)
	dir.create(file.path(paste("plots/img/", method, sep=""), name()), showWarnings=FALSE)
	#Create any image files that do not yet exist
	for (t in start:stop) {
		if (!(file.exists(paste(path, t, ".png", sep="")))) {
			plotpng(eval(parse(text=paste(method, "(", t, ")", sep=""))), t, path)
		}
	}
	#print("All files generated")
}

#Sequentially creates a set of plot images for the given method in the given directory over the given range.
#Uses a modified plotting plugin which creates PNGs instead of plot objects.
makeFiles2 <- function(start, stop, method) {
	path <- paste("plots/img/", method, "/", name(), "/", sep="")
	#Create directory for image files if it does not exist
	dir.create(file.path("plots/", "img"), showWarnings=FALSE)
	dir.create(file.path("plots/img/", method), showWarnings=FALSE)
	dir.create(file.path(paste("plots/img/", method, sep=""), name()), showWarnings=FALSE)
	#Create any image files that do not yet exist
	for (t in start:stop) {
		if (!(file.exists(paste(path, t, ".png", sep="")))) {
			eval(parse(text=paste(method, "(", t, ",", '"', path, t, ".png", '"', ")", sep="")))
		}
	}
	#print("All files generated")
}

#Uses parallel processing to create a set of plot images for the given method in the given directory over the given range.
makeFilesInParallel1 <- function(start, stop, method) {
	path <- paste("plots/img/", method, "/", name(), "/", sep="")
	#Create directory for image files if it does not exist
	dir.create(file.path("plots/", "img"), showWarnings=FALSE)
	dir.create(file.path("plots/img/", method), showWarnings=FALSE)
	dir.create(file.path(paste("plots/img/", method, sep=""), name()), showWarnings=FALSE)
	#Create any image files that do not yet exist


	#Setup parallel backend to use all 4 processors
	cl<-makeCluster(4)
	registerDoParallel(cl)
	foreach(t=start:stop, .packages=c('shiny', 'ggplot2')) %dopar% {
		#TODO: Figure out how to not have to do this every time, and also make it reactive
		source("plots/heatmap.r")
		source("data/import_gmd.r")
		import_data()
		if (!(file.exists(paste(path, t, ".png", sep="")))) {
			ggsave(file=paste(path, t, ".png", sep=""), plot=eval(parse(text=paste(method, "(", t, ")", sep=""))), width=10, height=4, units="in")
		}
	}
	stopCluster(cl)
	#print("All files generated")
}

#Uses parallel processing to create a set of plot images for the given method in the given directory over the given range.
#Uses a modified plotting plugin which creates PNGs instead of plot objects.
makeFilesInParallel2 <- function(start, stop, method) {
	path <- paste("plots/img/", method, "/", name(), "/", sep="")
	#Create directory for image files if it does not exist
	dir.create(file.path("plots/", "img"), showWarnings=FALSE)
	dir.create(file.path("plots/img/", method), showWarnings=FALSE)
	dir.create(file.path(paste("plots/img/", method, sep=""), name()), showWarnings=FALSE)
	#Create any image files that do not yet exist


	#Setup parallel backend to use all 4 processors
	cl<-makeCluster(4)
	registerDoParallel(cl)
	foreach(t=start:stop, .packages=c('shiny', 'ggplot2')) %dopar% {
		#TODO: Figure out how to not have to do this every time, and also make it reactive
		source("plots/heatmap.r")
		source("data/import_gmd.r")
		import_data()
		if (!(file.exists(paste(path, t, ".png", sep="")))) {
			eval(parse(text=paste(method, "(", t, ",", '"', path, t, ".png", '"', ")", sep="")))
		}
	}
	stopCluster(cl)
	#print("All files generated")
}

#Uses parallel processing to create a set of plot images for the given method in the given directory over the given range.
#Uses a modified plotting plugin which creates PNGs instead of plot objects.
#Loads dependancies outside of the foreach loop.
makeFilesInParallel3 <- function(start, stop, method) {
	path <- paste("plots/img/", method, "/", name(), "/", sep="")
	#Create directory for image files if it does not exist
	dir.create(file.path("plots/", "img"), showWarnings=FALSE)
	dir.create(file.path("plots/img/", method), showWarnings=FALSE)
	dir.create(file.path(paste("plots/img/", method, sep=""), name()), showWarnings=FALSE)
	#Create any image files that do not yet exist


	#Setup parallel backend to use all 4 processors
	cl<-makeCluster(4)
	registerDoParallel(cl)
	foreach(t=start:stop, .packages=c("ggplot2", "ggmap", "rgdal", "raster", "akima", "sp"), .export=c("bus_locs", "update_volt", "Volt", "g", "plot_heatmapvolt_png")) %dopar% {
		if (!(file.exists(paste(path, t, ".png", sep="")))) {
			eval(parse(text=paste(method, "(", t, ",", '"', path, t, ".png", '"', ")", sep="")))
		}
	}
	stopCluster(cl)
	#print("All files generated")
}

#Uses MPI cluster processing to create a set of plot images for the given method in the given directory over the given range.
#Uses a modified plotting plugin which creates PNGs instead of plot objects.
#Loads dependancies outside of the foreach loop.
makeFilesInCluster1 <- function(start, stop, method) {
	path <- paste("plots/img/", method, "/", name(), "/", sep="")
	#Create directory for image files if it does not exist
	dir.create(file.path("plots/", "img"), showWarnings=FALSE)
	dir.create(file.path("plots/img/", method), showWarnings=FALSE)
	dir.create(file.path(paste("plots/img/", method, sep=""), name()), showWarnings=FALSE)
	#Create any image files that do not yet exist


	#Setup parallel backend to use all 4 processors
	cl<-makeCluster(20, type="MPI")
	registerDoSNOW(cl)
	foreach(t=start:stop, .packages=c("ggplot2", "ggmap", "rgdal", "raster", "akima", "sp"), .export=c("bus_locs", "update_volt", "Volt", "g", "plot_heatmapvolt_png")) %dopar% {
		if (!(file.exists(paste(path, t, ".png", sep="")))) {
			eval(parse(text=paste(method, "(", t, ",", '"', path, t, ".png", '"', ")", sep="")))
		}
	}
	stopCluster(cl)
	#print("All files generated")
}
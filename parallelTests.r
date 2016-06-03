#Functions to compare the execution times of parallel and non-parallel file saving methods.
source("data/import_gmd.R")
import_data()
source("plots/heatmap.R")
source("utils/fileTools.r")
source("utils/plotTools.r")

testBasic1 <- function(start, stop, method) {
	startTime <- Sys.time()

	makeFiles1(start, stop, method)

	print(paste("Basic sequential method: ", toString((Sys.time()-startTime)), sep=""))
	
}

testBasic2 <- function(start, stop, method) {
	startTime <- Sys.time()

	makeFiles2(start, stop, paste(method, "_png", sep=""))

	print(paste("Sequential method with PNG device: ", toString((Sys.time()-startTime)), sep=""))
	
}

testParallel1 <- function(start, stop, method) {
	startTime <- Sys.time()

	makeFilesInParallel1(start, stop, method)

	print(paste("Basic parallel method: ", toString((Sys.time()-startTime)), sep=""))
	
}

testParallel2 <- function(start, stop, method) {
	startTime <- Sys.time()

	makeFilesInParallel2(start, stop, paste(method, "_png", sep=""))

	print(paste("Parallel method with PNG device: ", toString((Sys.time()-startTime)), sep=""))
	
}

testParallel3 <- function(start, stop, method) {
	startTime <- Sys.time()

	makeFilesInParallel3(start, stop, paste(method, "_png", sep=""))

	print(paste("Parallel method with PNG device and dependency pre-loading: ", toString((Sys.time()-startTime)), sep=""))
	
}

testCluster1 <- function(start, stop, method) {
	startTime <- Sys.time()

	makeFilesInCluster1(start, stop, paste(method, "_png", sep=""))

	print(paste("Cluster method with PNG device and dependency pre-loading: ", toString((Sys.time()-startTime)), sep=""))
	
}

runTests <- function() {
	method <- "plot_heatmapvolt" #Heatmap plot methods have the longest execution time
	testBasic1(1, 30, method)
	testBasic2(31, 60, method)
	testParallel1(61, 90, method)
	testParallel2(91, 120, method)
	testParallel3(121, 150, method)
	testCluster1(151, 180, method)
}	
runTests()

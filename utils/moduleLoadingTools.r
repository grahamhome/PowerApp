#Tools for loading "modules" (scripts) into PowerViewer.
#Created by Graham Home <grahamhome333@gmail.com>

#Load all data import modules into memory
loadDataModules <- function() {
	#Find module filenames
	importFiles <- list.files(path="data/", pattern="import_*")
	for (filename in importFiles) {
		#Create a new environment
		env <- new.env()
		#Import the module into the new environment
		sys.source(file=paste("data/", filename, sep=""), envir=env)
		#Import data
		eval(parse(text="import_data()"), envir=env)
		#Get the module's proper name
		name <- eval(parse(text="name()"), envir=env)[[1]]
		#Store module filename under proper name
		modules$data[[name]] <- filename
		#Get the list of compatible plot modules
		plots <- eval(parse(text="use_plots()"), envir=env)
		#Add compatible plot modules to compatible plot set. This is done here, rather than after a data set is selected, so that it will not slow down the UI.
		for (fname in plots) {
			if (!(fname %in% modules$compatPlots)) {
				pname <- getPlotName(fname)
				modules$compatPlots[pname] <- fname
			}
		}
	}
}

#Returns the proper name of the plot module with the given filename.
getPlotName <- function(filename) {
	#Create a new environment
	env <- new.env()
	#Import the file into the new environment
	sys.source(file=paste("plots/", filename, sep=""), envir=env)
	#Get the module's name list
	fnames <- (eval(parse(text="fnames()"), envir=env))
	#Get the module's proper name
	name <- names(fnames)[[1]]
	return(name)
}

#Updates the list of compatible plot modules after a data module has been imported.
updateCompatiblePlots <- function() {
	#Get the list of compatible plot module filenames
	compatible <- use_plots()
	#Remove all non-compatible plots from the compatible plots list
	for (name in names(modules$compatPlots)) {
		if (!(modules$compatPlots[name] %in% compatible)) {
			modules$compatPlots[name] <- NULL
		}
	}
}

#Updates the list of compatible display modules for the given plot module.
updateCompatibleDisplays <- function() {
	#Remove all displays not compatible with selected plot module
	for (name in names(modules$displays)) {
		if((modules$selectedPlot) %in% modules$displays[name][[1]][[2]]) {
			print(paste("Plot match with", name))
			#Map display module proper name to filename in compatible displays list
			modules$compatDisplays[name] <- modules$displays[name][[1]]
		}
	}
}

#Load all display modules into memory
loadDisplayModules <- function() {
	#Find module filenames
	displayFiles <- list.files(path="displays/", pattern="*.r")
	for (filename in displayFiles) {
		#Create a new environment
		env <- new.env()
		#Import the file into the new environment
		sys.source(file=paste("displays/", filename, sep=""), envir=env)
		#Get the module's proper name
		name <- (eval(parse(text="name()"), envir=env))
		#Get the module's compatible plots list
		supportedPlots <- (eval(parse(text="use_plots()"), envir=env))
		#Store display module proper name mapped to filename and compatible plots list
		modules$displays[[name]] <- list(filename, supportedPlots)
	}
}

#Load all data, plot, and display modules into memory efficiently
loadModules <- function() {
	loadDataModules()
	loadDisplayModules()
	print("All imports finished")
}

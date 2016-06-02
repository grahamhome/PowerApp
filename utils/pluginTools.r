#Tools for loading "plugins" (scripts) into PowerViewer.
#Created by Graham Home <grahamhome333@gmail.com>

#Load all data import plugins into memory
loadDataPlugins <- function() {
	#Find plugin filenames
	importFiles <- list.files(path="data/", pattern="import_*")
	for (filename in importFiles) {
		#Create a new environment
		env <- new.env()
		#Import the plugin into the new environment
		sys.source(file=paste("data/", filename, sep=""), envir=env)
		#Get the plugin's proper name
		name <- eval(parse(text="name()"), envir=env)
		#Store plugin filename under proper name
		plugins$data[[name]] <- filename
		#Get the list of compatible plot plugins
		plots <- eval(parse(text="use_plots()"), envir=env)
		#Add compatible plot plugins to compatible plot set. This is done here, rather than after a data set is selected, so that it will not slow down the UI.
		for (fname in plots) {
			if (!(fname %in% plugins$compatPlots)) {
				pname <- getPlotName(fname)
				plugins$compatPlots[pname] <- fname
			}
		}
	}
}

#Returns the proper name of the plot plugin with the given filename.
getPlotName <- function(filename) {
	print(filename)
	#Create a new environment
	env <- new.env()
	#Import the file into the new environment
	sys.source(file=paste("plots/", filename, sep=""), envir=env)
	#Get the plugin's name list
	fnames <- (eval(parse(text="fnames()"), envir=env))
	#Get the plugin's proper name
	name <- names(fnames)[[1]]
	return(name)
}

#Updates the list of compatible plot plugins after a data plugin has been imported.
updateCompatiblePlots <- function() {
	#Get the list of compatible plot plugin filenames
	compatible <- use_plots()
	#Remove all non-compatible plots from the compatible plots list
	for (name in names(plugins$compatPlots)) {
		if (!(plugins$compatPlots[name] %in% compatible)) {
			plugins$compatPlots[name] <- NULL
		}
	}
}

#Updates the list of compatible display plugins for the given plot plugin.
updateCompatibleDisplays <- function() {
	#Remove all displays not compatible with selected plot plugin
	for (name in names(plugins$displays)) {
		if((plugins$selectedPlot) %in% plugins$displays[name][[1]][[2]]) {
			#Map display plugin proper name to filename in compatible displays list
			plugins$compatDisplays[name] <- plugins$displays[name][[1]]
		}
	}
}

#Load all display plugins into memory
loadDisplayPlugins <- function() {
	#Find plugin filenames
	displayFiles <- list.files(path="displays/", pattern="*.r")
	for (filename in displayFiles) {
		#Create a new environment
		env <- new.env()
		#Import the file into the new environment
		sys.source(file=paste("displays/", filename, sep=""), envir=env)
		#Get the plugin's proper name
		name <- (eval(parse(text="dispName()"), envir=env))
		#Get the plugin's compatible plots list
		supportedPlots <- (eval(parse(text="use_plots()"), envir=env))
		#Store display plugin proper name mapped to filename and compatible plots list
		plugins$displays[[name]] <- list(filename, supportedPlots)
	}
}

#Load all data, plot, and display plugins into memory efficiently
loadplugins <- function() {
	loadDataPlugins()
	loadDisplayPlugins()
}

	#Load all data modules into memory
loadDataModules <- function() {
	#Find module filenames
	importFiles <- list.files(path="data/", pattern="import_*")
	for (filename in importFiles) {
		#Create a new environment
		env <- new.env()
		#Import the module into the new environment
		sys.source(file=paste("data/", filename, sep=""), envir=env)
		#Get the module's proper name
		name <- eval(parse(text="name()"), envir=env)[[1]]
		#Get the list of compatible plots
		plots <- eval(parse(text="use_plots()"), envir=env)
		#Store module data under proper name
		modules$data[[name]] <- list(filename, env, plots)
	}
}

#Load all plot modules into memory
loadPlotModules <- function() {
	#Find module filenames
	plotFiles <- list.files(path="plots/", pattern="*.R")
	for (filename in plotFiles) {
		#Create a new environment
		env <- new.env()
		#Import the file into the new environment
		sys.source(file=paste("plots/", filename, sep=""), envir=env)
		#Get the module's name list
		fnames <- (eval(parse(text="fnames()"), envir=env))
		#Get the module's proper name
		#name <- names[[gsub(".R", "", filename)]]
		name <- names(fnames)[[1]]
		#Remove the module's proper name from the name list
		fnames[[1]] <- NULL
		#Store module data under file name
		modules$plots[[filename]] <- list(name, env, fnames)
		#Store module file name under proper name
		modules$plotNames[[name]] <- filename
	}
}

#Load all display modules into memory
loadDisplayModules <- function() {
	#Find module filenames
	displayFiles <- list.files(path="displays/", pattern="*.R")
	for (filename in displayFiles) {
		#Create a new environment
		env <- new.env()
		#Import the file into the new environment
		sys.source(file=paste("plots/", filename, sep=""), envir=env)
		#Get the module's proper name
		name <- (eval(parse(text="name()"), envir=env))
		#Get the module's compatible plots list
		supportedPlots <- (eval(parse(text="use_plots()"), envir=env))
		#Store module data under proper name
		modules$displays[[name]] <- list(filename, env, supportedPlots)
	}

}

#Load all data, plot, and display modules into memory efficiently
loadModules <- function() {
	loadDataModules()
	loadPlotModules()
	loadDisplayModules()
	print("All imports finished")
}

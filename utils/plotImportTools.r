#Returns the names of the plot files compatible with the dataset selected by the user TODO: speed up
getCompatiblePlots <- function() {
	print("Got compatible plots")
	#Return filenames of supported plots
	eval(parse(text="use_plots()"), envir=files$selectedImport)
}

#Returns the names of the plot functions defined in the plot file selected by the user.
getPlotFunctions <- function() {
	functions <- ls.str(plots$env, "function")
	prefix <- "plot_"	#Functions that return a plot begin with this TODO: Fix this, reference names() instead
	plotFunctions <- list()
	i <- 1
	for (f in functions) {
		if (substring(f, 1, nchar(prefix)) == prefix) {
			plotFunctions[i] = f
			i <- i + 1
		}
	}
	plotFunctions
}

#Returns a list of tabs, one for each available plot function. TODO: remove this
makeFunctionTabs <- function() {
	tabs <- list()
	functs <- getPlotFunctions()
	for (i in 1:length(functs)) {
		tabs[[i]] = tabPanel(title=functs[[i]], value=i, plotOutput("plot", height="400px", width="100%")) #TODO: Size reactively based on window size
	}
	tabs
}

#Create an environment for each data import file.
getDataFiles <- function() {
	#Get the names of all the data import files
	importFiles <- list.files(path="data/", pattern="import_*")
	for (file in importFiles) {
		#Create a new environment
		env <- new.env()
		#Import the file into the new environment
		sys.source(file=paste("data/", file, sep=""), envir=env)
		#Map the new environment to the file's proper name
		name <- eval(parse(text=gsub(".R", "_names()", file)), envir=env)[[1]]
		files$imports[[name]] <- env
	}
}

#Create an environment for each named plot file.
getPlotFiles <- function() {
	plotFiles <- list.files(path="plots/", pattern="*.R")
	for (file in plotFiles) {
		#Create a new environment
		env <- new.env()
		#Import the file into the new environment
		sys.source(file=paste("plots/", file, sep=""), envir=env)
		#Map the new environment to the file's name
		files$plots[[file]] <- env
		properName <- (eval(parse(text=gsub(".R", "_names()", file)), envir=env)[[gsub(".R", "", file)]])
		files$plotNames[[properName]] <- file
	}
}
#Utility functions for loading modules and UI panes in Power Viewer.

#List of previously launched modules
modules <- list()

#Launches the Shiny module contained in the named file.
launchDisplayModule <- function(moduleFilename) {
	mods <- modules
	print(mods)
	print(moduleFilename)
	#Create a unique ID for the module
	id <- gsub("[.]r", "", moduleFilename)
	print(id)
	if (!(id %in% mods)) {
		print("launching module for the first time")
		mods[length(mods)+1] <- id
		modules <<- mods
		#Import and run selected module						
		source(paste("displays/", moduleFilename, sep=""))
		print("sourced module OK")
		print(gsub("[.]r", paste('UI("', id, '")', sep=""), moduleFilename))
		launchUI(gsub("[.]r", paste('UI("', id, '")', sep=""), moduleFilename))
		print("Window content set")
		callModule(eval(parse(text=gsub("[.]r", "", moduleFilename))), id)
		print("module called")
	} else {
		#Switch window content back to module output
		print("switching back to display module")
		launchUI(gsub("[.]r", paste('UI("', id, '")', sep=""), moduleFilename))
	}
}			

#Runs a UI generation function given the function name.
launchUI <- function(fname) {
	window$content <- fname
}
#Utility functions for loading modules and UI panes in Power Viewer. 
#Modules and UI panes are ways of encapsulating different activities
#of the main application into separate files in order to keep the 
#code from becoming a big plate of spaghetti.

#Author: Graham Home <grahamhome333@gmail.com>

#Launches the Shiny module contained in the named file.
launchDisplayModule <- function(moduleFilename) {
	#Create a unique ID for the module
	id <- gsub("[.]r", "", moduleFilename)
	if (!(id %in% plugins$launchedDisplays)) {
		plugins$launchedDisplays[[length(plugins$launchedDisplays)+1]] <- id
		#Import and run selected module						
		source(paste("displays/", moduleFilename, sep=""))
		launchUI(gsub("[.]r", paste('UI("', id, '")', sep=""), moduleFilename))
		callModule(eval(parse(text=gsub("[.]r", "", moduleFilename))), id)
	} else {
		#Switch window content back to module output
		launchUI(gsub("[.]r", paste('UI("', id, '")', sep=""), moduleFilename))
	}
}			

#Runs a UI generation function given the function name.
launchUI <- function(fname) {
	window$content <- fname
}
#Utility functions for loading modules and UI panes in Power Viewer.

#Launches the Shiny module contained in the named file.
launchModule <- function(moduleFilename) {							#TODO: Make this work with UI pane modules
	window$content <- gsub("[.]r", 'UI("display")', moduleFilename)
	callModule(eval(parse(text=gsub("[.]r", "", moduleFilename))), "display")
}			

#Launches a UI psuedo-module given a function name.
launchUI <- function(fname) {
	window$content <- fname
}
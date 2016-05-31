#PowerViewer: A tool for visualizing power grid data.
#Author: Graham Home <grahamhome333@gmail.com>

#Dependencies
library(shiny)
library(shinythemes)

#Basic imports needed for the app to function
baseImports <- function() {

	#Import UI render functions
	source("ui/intro.r")
	source("ui/dataPicker.r")
	source("ui/plotPicker.r")
	source("ui/display.r")
	source("displays/timeSeriesDisplay.r")

	#Import module loading tools
	source("utils/moduleLoadingTools.r")
}
baseImports()

#Reactive values for modules. Modules are function collections for collecting, plotting or displaying data
modules <- reactiveValues()
modules$data <- list() 		#List of import module proper names linked to list(filename, environment, plots). plots = compatible plot module file names
modules$plots <- list() 	#List of plot module file names linked to list(propername, environment, names). names = function proper names linked to function names.
modules$displays <- list() 	#List of display module proper names linked to list(filename, environment, plots). plots = compatible plot module function names
modules$plotNames <- list() #List of plot module proper names mapped to file names

#Initialize module reactive values once on app startup
isolate(loadModules())

#Reactive value for current UI function 
window <- reactiveValues()
window$content <- "intro()" #Name of the current user interface function

#Application UI function
ui <- fluidPage(

	theme=shinytheme("spacelab"),
	includeCSS("styles/blue.css"), #Stylesheet for custom divs and other elements
  	uiOutput("content") #All UI elements are rendered reactively
)

#R functionality
server <- function(input, output, session) {
	#Run the current UI function
	output$content <- renderUI({ eval(parse(text=window$content)) })

	#Respond to button presses by changing UI function

	#"Next" button
	observeEvent(input$forward, {
		if (window$content == "intro()") {
			#Switch to data picker activity
			window$content <- "dataPicker()" 
		} else if (window$content == "dataPicker()") {
			#Import data module chosen by user and switch to plot picker activity.
			source(paste("data/", modules$data[[input$data]][[1]], sep=""))
			print("imported dataset")
			window$content <- "plotPicker()"
		} else if (window$content == "plotPicker()") {
			#Import plot module chosen by user and switch to display activity.
			source(paste("plots/", input$plot, sep=""))
			window$content <- "display()"
			callModule(timeSeriesDisplay, "tsDisplay")
			#window$content <- "timeSeriesDisplay(input, output, session)" #TODO: Choose display reactively by checking plots lists of display modules
		} 
	})

	#"Back" button
	observeEvent(input$back, {
		if (window$content == "dataPicker()") {
			window$content <- "intro()"
		} else if (window$content == "plotPicker()") {
			window$content <- "dataPicker()" 
		} else {
			window$content <- "plotPicker()" #TODO: Delete environment and start over?
		}
	})
}

#Start the app
mainApp <- shinyApp(ui=ui, server=server)
runApp(mainApp, port=5678)
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
	source("ui/displayPicker.r")

	#Import tools
	source("utils/pluginTools.r")
	source("utils/moduleTools.r")
	source("utils/plotTools.r")
}
baseImports()

#Import for testing purposes
source("displays/timeSeriesDisplay.r")

#Reactive values for plugins. plugins are function collections for collecting, plotting or displaying data
plugins <- reactiveValues()
plugins$data <- list() 		#List of import plugin proper names mapped to filenames
plugins$displays <- list() 	#List of display plugin proper names linked to list(filename, plots). plots = compatible plot plugin filenames
plugins$compatPlots <- list() #Set of plot plugins compatible with the available data import plugins. Maps plugin filenames to proper names.
plugins$compatDisplays <- list()

#Initialize plugin reactive values once on app startup
isolate(loadplugins())

#Reactive value for current UI function 
window <- reactiveValues()
window$content <- NULL #Name of the current user interface function

window$activity <- "selection" #Name of the current activity

launchUI("intro()")

#Application UI function
ui <- fluidPage(

	theme=shinytheme("spacelab"),
	includeCSS("styles/blue.css"), #Stylesheet for custom divs and other elements
	conditionalPanel(condition='window$activity == "selection"',
  		uiOutput("content") #All UI elements are rendered reactively
  	),
  	conditionalPanel(condition='window$activity == "tsDisplay"',
  		timeSeriesDisplayUI("display")	
  	)
)

#R functionality
server <- function(input, output, session) {
	#Run the current UI function
	output$content <- renderUI({ eval(parse(text=window$content)) })

	#Start by calling display module?
	callModule(timeSeriesDisplay, "display")

	#Respond to button presses by changing UI function

	#"Next" button
	observeEvent(input$forward, {
		if (window$content == "intro()") {
			launchUI("dataPicker()")
		} else if (window$content == "dataPicker()") {
			#Import data plugin chosen by user and switch to plot picker activity.
			source(paste("data/", input$data, sep=""))#plugins$data[[input$data]][[1]], sep=""))
			#Import the data
			import_data()
			#Update compatible plots list now that a dataset has been selected
			updateCompatiblePlots()
			launchUI("plotPicker()")
		} else if (window$content == "plotPicker()") {
			#Import plot plugin chosen by user
			source(paste("plots/", input$plot, sep=""))
			#Set "selected plot" variable
			plugins$selectedPlot <- input$plot

			#Update display list
			updateCompatibleDisplays()

			#Switch to display picker activity if the number of compatible displays is >1, otherwise load the compatible display.
			if (length(plugins$compatDisplays) == 1) {
				#Set "selected display" variable
				plugins$selectedDisplay <- plugins$compatDisplays[[1]]
				#Import display plugin
				#source(paste("displays/", plugins$selectedDisplay, sep=""))
				#Launch display activity
				#isolate(launchModule(plugins$selectedDisplay))

				#Show display panel
				window$activity <- "tsDisplay"


			} else {
				#Launch display chooser activity
				window$content <- "displayPicker()"
			}
		} else if (window$content == "displayPicker()") {
			#Set "selected display" variable
			plugins$selectedDisplay <- input$display
			#Import display plugin
			#source(paste("displays/", plugins$selectedDisplay, sep=""))
			#Launch display activity
			#isolate(launchModule(plugins$selectedDisplay))

			#Show display panel
			window$activity <- "tsDisplay"
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
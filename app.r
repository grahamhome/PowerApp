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

	#Import module loading tools
	source("utils/moduleLoadingTools.r")
}
baseImports()

#Reactive values for modules. Modules are function collections for collecting, plotting or displaying data
modules <- reactiveValues()
modules$data <- list() 		#List of import module proper names mapped to filenames
modules$displays <- list() 	#List of display module proper names linked to list(filename, plots). plots = compatible plot module filenames
modules$compatPlots <- list() #Set of plot modules compatible with the available data import modules. Maps module filenames to proper names.
modules$compatDisplays <- list()

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
			source(paste("data/", input$data, sep=""))#modules$data[[input$data]][[1]], sep=""))
			print("imported dataset")
			#Update compatible plots list now that a dataset has been selected
			updateCompatiblePlots()
			window$content <- "plotPicker()"
		} else if (window$content == "plotPicker()") {
			#Import plot module chosen by user
			source(paste("plots/", input$plot, sep=""))
			#Set "selected plot" variable
			modules$selectedPlot <- input$plot

			#Update display list
			updateCompatibleDisplays()

			#TODO: add display picker activity here for plots with >1 compatible display
			#In the meantime, the application will simply choose the first compatible display in the list
			if (length(modules$compatDisplays) == 1) {
				#Set "selected display" variable
				modules$selectedDisplay <- modules$compatDisplays[[1]]
				#Import display module
				source(paste("displays/", modules$selectedDisplay, sep=""))
				#Launch display activity
				window$content <- gsub("[.]r", 'UI("display")', modules$selectedDisplay)
				print(gsub("[.]r", "", modules$selectedDisplay))
				callModule(eval(parse(text=gsub("[.]r", "", modules$selectedDisplay))), "display")


			} else {
				#Launch display chooser activity
				window$content <- "displayPicker()"
			}
		} else if (window$content == "displayPicker()") {
			#Set "selected display" variable
			modules$selectedDisplay <- input$display
			#Import display module
			source(paste("displays/", modules$selectedDisplay, sep=""))
			#Launch display activity
			window$content <- gsub("[.]r", 'UI("display")', modules$selectedDisplay)
			print(gsub("[.]r", "", modules$selectedDisplay))
			callModule(eval(parse(text=gsub("[.]r", "", modules$selectedDisplay))), "display")
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
#Main app of PowerViewer application.
#Author: Graham Home <grahamhome333@gmail.com>

#Dependencies
library(shiny)
library(shinythemes)

#Import UI generation functions
source("ui/intro.r")
source("ui/dataPicker.r")
source("ui/plotPicker.r")
source("displays/timeSeriesDisplay.r")

#Import file importing tools
source("utils/fileImportTools.r")

#Reactive values for user's progress
progress <- reactiveValues()
progress$stage <- 1

#Reactive values for user's chosen files
files <- reactiveValues()
files$imports <- list() #List of import files names linked to environments
files$plots <- list() #List of plot files linked to environments

#App UI is defined here - inputs & outputs
ui <- fluidPage(
	theme=shinytheme("spacelab"),	#Space is cool

	includeCSS("styles/blue.css"), #Stylesheet for custom divs and other elements

  	uiOutput("content") #All UI elements go here
)

#R functionality is defined here - how inputs affect outputs
server <- function(input, output, session) {

	#Window changes state based on user activity
	output$content <- renderUI({

		#Intro screen
		if (progress$stage == 1) {
			intro()
  		# Dataset picker
		} else if (progress$stage == 2) {
			dataSelection()
  		#Plot type picker	
		} else if (progress$stage == 3) {
			plotSelection()
  		#Plot display
		} else if (progress$stage == 4) {
			timeSeriesDisplay()				#TODO: add logic to pick display based on plot type
		}
	})

	#Plot window is rendered using currently active plotting function
	output$plot <- renderPlot({
		eval(parse(text="plot_mapvolt(1)"), envir=files$selectedImport)	#TODO: Make reactive based on selected plot functions
	})

	#System actions to prepare for activity transitions TODO: combine with UI rendering logic, use generic "next" button for all screens

	#When "Start" button is clicked
	observeEvent(input$start, {
		#Switch to dataset picker activity
		progress$stage <- 2
	})

	#When "Import Data" button is clicked
	observeEvent(input$selectData, {
		#Store the environment of the user's data choice
		files$selectedImport <- files$imports[[input$dataset]]
		#Import dataset chosen by user
		eval(parse(text="import_data()"), envir=files$imports[[input$dataset]])
		print("imported dataset")
		#Load compatible plot names
		getCompatiblePlots()				#TODO: Figure out how to call this on app start instead of here
		#Switch to plot picker activity.
		progress$stage <- 3

	})

	#When "Select Plot" button is clicked
	observeEvent(input$selectPlot, {
		#Store the user's plot choice
		files$selectedPlot <- input$plottype
		#Import the selected plot file into the selected dataset environment
		#Get filenames of supported plots
		print(input$plottype)
		sys.source(file=paste("plots/", input$plottype, sep=""), envir=files$selectedImport)
		#Switch to display activity
		progress$stage <- 4
	})

	#When the "Back" button is clicked
	observeEvent(input$back, {
		print(progress$stage)
		if (progress$stage > 0) {	#Just in case I forget and put it on the home page by mistake
			progress$stage <- progress$stage-1
		}
	})
}

#Import data and plot files once when app starts
isolate(getDataFiles())
isolate(getPlotFiles())

#Start the app
mainApp <- shinyApp(ui=ui, server=server)
runApp(mainApp, port=5678)
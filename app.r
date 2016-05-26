#Main app of PowerViewer application.
#Author: Graham Home <grahamhome333@gmail.com>

#Dependencies
library(shiny)
library(shinythemes)

#Create reactive values for user's progress
progress <- reactiveValues()
progress$stage <- 1

#Reactive values for user's chosen files
files <- reactiveValues()

#Reactive values for user's selected plot type
plots <- reactiveValues()

#Data file selection utility function - returns filenames of all data import files.
getDataFileNames <- function() {
	print("getting file names")
	files <- list.files(path="data/", pattern="*.r")
}

#Returns the names of the plot files compatible with the dataset selected by the user
getCompatiblePlots <- function() {
	eval(parse(text=gsub(".r", "_plots()", files$data)))
}

#Returns the names of the plot functions defined in the plot file selected by the user.
getPlotFunctions <- function() {
	functions <- ls.str(plots$env, "function")
	prefix <- "plot_"	#Functions that return a plot begin with this
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

#App UI is defined here - inputs & outputs
ui <- fluidPage(
	theme=shinytheme("spacelab"),	#Space is cool

  	absolutePanel(top="25%", height="50%", left="30%", width="40%", fixed=TRUE, style="background-color:#e6e6e6",
  		uiOutput("content")
  	)
)

#R functionality is defined here - how inputs affect outputs
server <- function(input, output, session) {

	#Content window changes state based on user activity
	output$content <- renderUI({

		#Intro screen
		if (progress$stage == 1) {
			fluidRow(
  				column(8, offset=2,
  					h1("Welcome to Power Viewer!", style="text-align:center")
  				),
  				column(12,
  					h3("Power Viewer is a tool for viewing power grid data with a library of plotting methods.
  						The next two screens will allow you to choose from the available data sets and plot styles.
  						Ready to begin?", style="line-height:150%; text-align:center")
  				),
  				column(4, offset=4,
  				actionButton("start", "Start", width="100%", style="margin-top:10%")
  		 		),
  		 		column(8, offset=2,
  		 			uiOutput("dataPicker")
  		 		)
  			)

  		# Dataset picker
		} else if (progress$stage == 2) {
			fluidRow(
				column(2,
  					actionLink("back", "Previous", icon=icon("arrow-left"))
  				),
  				column(8,
  					h1("Choose Data Set", style="text-align:center"),
  					br(),
  					selectInput("dataset", "Data Set", getDataFileNames()),
  					actionButton("selectData", "Import Data")
  				)
  			)

  		#Plot type picker	
		} else if (progress$stage == 3) {
			fluidRow(
				column(2,
  					actionLink("back", "Previous", icon=icon("arrow-left"))
  				),
  				column(8, offset=2,
  					h1("Choose Plot Type", style="text-align:center"),
  					br(),
  					selectInput("plottype", "Plot Type", getCompatiblePlots()),
  					actionButton("selectPlot", "Select Plot")
  				)
  			)

  		#Plot display
		} else if (progress$stage == 4) {
			div(
				fluidRow(
					column(2,
  						actionLink("back", "Previous", icon=icon("arrow-left"))
  					),
  					column(10,
  						tabsetPanel(	#TODO: Get these tabs to work!
  							tabPanel("Tab One", imageOutput("plot", height="600px", width="1500px")),
  							tabPanel("Tab Two", imageOutput("plot2", height="600px", width="1500px"))
  						)
  					)
  				),
  				fluidRow(
  					column(12,
  						sliderInput("time", "Time range to examine",  min = 1, max = 845, value = c(500, 750), width = "75%"),
  						radioButtons("speed", "Animation speed", c("Very slow"=10, "Normal speed"=1, "Very fast"=0.01), selected=1, inline=TRUE),
  						actionButton("go", "Go!")
  					)
  				)
  			)
		}
	})

	#When "Start" button is clicked
	observeEvent(input$start, {
		#Switch to dataset picker activity
		progress$stage <- 2
	})

	#When "Import Data" button is clicked
	observeEvent(input$selectData, {
		#Switch to plot picker activity.
		progress$stage <- 3
		#Import dataset chosen by user
		source(file=paste("data/", input$dataset, sep=""))
		eval(parse(text=gsub(".r", "()", input$dataset)))
		#Store name of data import file
		files$data <- input$dataset

	})

	#When "Select Plot" button is clicked
	observeEvent(input$selectPlot, {
		#Switch to display activity
		progress$stage <- 4

		#Create new environment for plot functions
		plots$env <- new.env()
		#Import plot functions from file chosen by user into new environment
		sys.source(file=paste("plots/", input$plottype, sep=""), envir=plots$env)
		#Store name of plot function file
		files$plot <- input$plottype
	})

	#When the "Previous" button is clicked
	observeEvent(input$back, {
		if (progress$stage > 0) {	#Just in case I forget and put it on the home page by mistake
			progress$stage <- progress$stage-1
		}
	})
}

#Start the app
mainApp <- shinyApp(ui=ui, server=server)
runApp(mainApp, port=5678)
#Main app of PowerViewer application.
#Author: Graham Home <grahamhome333@gmail.com>

#Dependencies
library(shiny)
library(shinythemes)

#Reactive values for user's progress
progress <- reactiveValues()
progress$stage <- 1

#Reactive values for user's chosen files
files <- reactiveValues()

#Reactive values for user's selected plot type
plots <- reactiveValues()

#App UI is defined here - inputs & outputs
ui <- fluidPage(
	theme=shinytheme("spacelab"),	#Space is cool

	includeCSS("style.css"), #Stylesheet for custom divs and other elements

  	uiOutput("content")
)

#R functionality is defined here - how inputs affect outputs
server <- function(input, output, session) {

	#Content window changes state based on user activity
	output$content <- renderUI({

		#Intro screen
		if (progress$stage == 1) {
			makeIntro()
  		# Dataset picker
		} else if (progress$stage == 2) {
			makeDataChooser()
  		#Plot type picker	
		} else if (progress$stage == 3) {
			print("making plot chooser")
			makePlotChooser()
  		#Plot display
		} else if (progress$stage == 4) {
			makePlotDisplay()
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

	#When the "Back" button is clicked
	observeEvent(input$back, {
		print(progress$stage)
		if (progress$stage > 0) {	#Just in case I forget and put it on the home page by mistake
			progress$stage <- progress$stage-1
		}
	})
}

#UI functions - generate different windows
#Introduction screen
makeIntro <- function() {
	fixedPanel(class="mainwindow_inactive",
		fixedPanel(class="popup",

			fluidRow(
		  		column(8, offset=2,
		  			h1("Welcome to Power Viewer!", class="windowtitle")
		  		)
		  	),
		  	fluidRow(
				column(12,
		  			h3("Power Viewer is a tool for viewing power grid data with a library of plotting methods.
		  				The next two screens will allow you to choose from the available data sets and plot styles.
		  				Ready to begin?", style="line-height:150%; text-align:center")
		  		)
			),
			actionButton("start", "Start", class="next")
		)
	)
}

#Data set chooser
makeDataChooser <- function() {
	fixedPanel(class="mainwindow_inactive",
		fixedPanel(class="popup",
			fluidRow(
				column(2,
					actionLink("back", "", icon=icon("arrow-left", "fa-2x"), class="icon")
				),
				column(8,
					h1("Data Set", class="windowtitle")
				)
			),
			fluidRow(
				column(8, offset=2,
					h3("Select a data set:"),
					br(),
					selectInput("dataset", "", getDataFileNames())
				)
			),
			actionButton("selectData", "Import Data", class="next")
		)
	)
}

#Plot type chooser
makePlotChooser <- function() {
	fixedPanel(class="mainwindow_inactive",
		fixedPanel(class="popup",
			fluidRow(
				column(2,
					actionLink("back", "", icon=icon("arrow-left", "fa-2x"), class="icon")
				),
				column(8,
					h1("Plot Type", class="windowtitle")
				)
			),
			fluidRow(
				column(8, offset=2,
					h3("Select a plot type:"),
					br(),
					selectInput("plottype", "Plot Type", getCompatiblePlots())
				)
			),
			actionButton("selectPlot", "Select Plot", class="next")
		)
	)
}

#Plot display window
makePlotDisplay <- function() {
	fixedPanel(class="mainwindow",
		fluidRow(
			column(2,
				actionLink("back", "", icon=icon("arrow-left", "fa-2x"), class="icon")
			),
			column(8,
				h1("Plot", class="windowtitle") #TODO: Replace with a reactive title based on plot type & data set
			)
		),
		fluidRow(
			column(1,
				p("Function"),
				p("List"),
				p("Here")
			),
			column(11,
				#do.call(tabsetPanel, makeFunctionTabs()),
				sliderInput("time", "Time range to examine",  min = 1, max = 100, value = 1, width = "100%"), #TODO: set max/min reactively
				br(),
				column(4, offset=4,
					div(style="text-align:center", actionLink("play", "", icon=icon("play", "fa-2x"), class="icon"))
				)
			)
		)
	)
}

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

#Returns a list of tabs, one for each available plot function.
makeFunctionTabs <- function() {
	tabs <- list()
	functs <- getPlotFunctions()
	for (i in 1:length(functs)) {
		tabs[[i]] = tabPanel(title=functs[[i]], value=i, imageOutput("plot", height="400px", width="100%")) #TODO: Size reactively based on window size
	}
	tabs
}


#Start the app
mainApp <- shinyApp(ui=ui, server=server)
runApp(mainApp, port=5678)
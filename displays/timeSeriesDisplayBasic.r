#A Shiny plugin which creates a window for displaying time series plots.
#Created by Graham Home <grahamhome333@gmail.com>

#Proper Name
dispName <- function() {
	"Basic Time Series Display"
}

#Compatible plot plugins
use_plots <- function() {
	list('map.R','heatmap.R','correlation.R')
}

#UI
timeSeriesDisplayBasicUI <- function(id) {
	#Create namespace function from id
	ns <- NS(id)
	#Enclose UI contents in a tagList
	tagList(
		fixedPanel(class="mainwindow",
			fluidRow(

				column(2,
					actionLink(ns("back"), "", icon=icon("arrow-left", "fa-2x"), class="icon")
				)
			),
			fluidRow(
				column(12,

					plotOutput(ns("plot"), height="400px", width="100%"), #TODO: Size reactively based on window size
					radioButtons(ns("activeMethod"), "Function:", fnames()[c(2, length(fnames()))], inline=TRUE),
					sliderInput(ns("time"), "Time range to examine",  min = 1, max = nsamples(), value = 1, width = "100%")
				)
			)	
		)
	)
}

#Server logic
timeSeriesDisplayBasic <- function(input, output, session) {
	output$plot <- renderPlot(eval(parse(text=paste(input$activeMethod, "(", input$time, ")", sep=""))))
	observeEvent(input$back, {
		#Did the display picker launch?
		if (length(plugins$compatDisplays) == 1) {
			launchUI("plotPicker()")
		} else {
			launchUI("displayPicker()")
		}
	})
	return
}

#TODO: Add real-time animation view back (rolling buffer)
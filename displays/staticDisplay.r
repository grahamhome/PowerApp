#A Shiny plugin which creates a window for displaying static plots.
#Created by Graham Home <grahamhome333@gmail.com>

#Proper Names
name <- function() {
	"Static Display"
}

#Compatible plot plugins
use_plots <- function() {
	list('linear.R')
}

#UI function
staticDisplayUI <- function(id) {

	#Create namespace function
	ns <- NS(id)

	#Return UI elements in a tagList
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
					radioButtons(ns("activeMethod"), "Function:", fnames()[c(2, length(fnames()))], inline=TRUE)
				)
			)	
		)
	)
}

#Server function
staticDisplay <- function(input, output, session) {
	output$plot <- renderPlot(eval(parse(text=paste(input$activeMethod, "()", sep=""))))
	return

}
#TODO: Add real-time animation view back (rolling buffer)
#A Shiny plugin which creates a window for displaying static plots.
#Created by Graham Home <grahamhome333@gmail.com>

#Proper Names
dispName <- function() {
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
					plotOutput(ns("plot"), height="400px", width="1000px"), #TODO: Size reactively based on window size
					sliderInput(ns("time"), "Sample range to examine",  min = 1, max = nsamples(), value = c(1, 10), width = "100%"),
					radioButtons(ns("activeMethod"), "Function:", fnames()[c(2, length(fnames()))], inline=TRUE)
				)
			)	
		)
	)
}

#Server function
staticDisplay <- function(input, output, session) {
	output$plot <- renderPlot({
		start <- input$time[[1]]
		stop <- input$time[[2]]
		print(paste("Start: ", start, " Stop: ", stop, sep=""))
		eval(parse(text=paste(input$activeMethod, "(", as.numeric(start), ",", as.numeric(stop), ")", sep="")))
		#eval(parse(text=paste(input$activeMethod, "(", as.numeric(input$time[[1]]), as.numeric(input$time[[2]]), ")", sep="")))
		})
	return
}
#TODO: Add real-time animation view back (rolling buffer)
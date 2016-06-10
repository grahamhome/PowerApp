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
				),
				column(8, 
					h1(name())
				),
				column(2, 
					div(class="helpiconbox", actionLink(ns("help"), "", icon=icon("question", "fa-2x"), class="icon"))
				)
			),
			fluidRow(
				column(10, offset=1,
					plotOutput(ns("plot"), height="400px", width="100%") 
				)
			),
			fluidRow(
				column(10, offset=1,
					radioButtons(ns("activeMethod"), "Function:", fnames()[2:length(fnames())], inline=TRUE),
					sliderInput(ns("time"), "Sample range to examine",  min = 1, max = nsamples(), value = c(1, 10), width = "100%")
				)
			)	
		),
		uiOutput(ns("helpbox"))
	)
}

#Server function
staticDisplay <- function(input, output, session) {
	#Reactive values related to window state
	state <- reactiveValues()
	#Show/hide help text
	state$showHelp <- FALSE
	output$plot <- renderPlot({
		start <- input$time[[1]]
		stop <- input$time[[2]]
		eval(parse(text=paste(input$activeMethod, "(", as.numeric(start), ",", as.numeric(stop), ")", sep="")))
		})
	observeEvent(input$back, {
		#Did the display picker launch?
		if (length(plugins$compatDisplays) == 1) {
			launchUI("plotPicker()")
		} else {
			launchUI("displayPicker()")
		}
	})

	#Help text popup
	observeEvent(input$help, {
		state$showHelp <- !state$showHelp
	})

	#Help text
	output$helpbox <- renderUI({
		if (state$showHelp) {
			div(class="helptextbox",
				p("Use the double-ended slider to select a range of values to view in the linear plot. ", br(), br(),
					"Use the radio buttons below the graph display to change the plotting method used ", br(),
					"to create the graph.", br(), br(),
					"Use the back button in the top left corner of the display to choose a different plot type or data set.")
			)
		}
	})

	return
}
#TODO: Add real-time animation view back (rolling buffer)
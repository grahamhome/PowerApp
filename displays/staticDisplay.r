#A Shiny plugin which creates a window for displaying static plots.

#Author: Graham Home <grahamhome333@gmail.com>

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
				column(1, 
					div(
						div(style="height:100px"),
						div(style="position:relative;float:right", actionLink(ns("incLeft"), "", icon=icon("step-forward", "fa-2x"), class="icon")),
						div(style="position:relative;float:right;margin-right:30px", actionLink(ns("decLeft"), "", icon=icon("step-backward", "fa-2x"), class="icon"))
					)
				),
				column(10,
					radioButtons(ns("activeMethod"), "Function:", fnames()[2:length(fnames())], inline=TRUE),
					sliderInput(ns("time"), "Sample range to examine",  min = 1, max = nsamples(), value = c(1, 10), width = "100%")
				),
				column(1, 
					div(
						div(style="height:100px"),
						div(style="position:relative;float:left;margin-right:30px", actionLink(ns("decRight"), "", icon=icon("step-backward", "fa-2x"), class="icon")),
						div(style="position:relative;float:left", actionLink(ns("incRight"), "", icon=icon("step-forward", "fa-2x"), class="icon"))
					)
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
				p("Use the double-ended slider to select a range of values to view in the linear plot. ", br(),
					"You can move each end of the slider independently and use the forward and back buttons ", br(),
					"on each side of the slider for finer control over each end of the slider. You can also ", br(),
					"move the entire range by clicking and dragging between the slider ends. ", br(), br(),
					"Use the radio buttons below the graph display to change the plotting method used ", br(),
					"to create the graph.", br(), br(),
					"Use the back button in the top left corner of the display to choose a different plot type or data set.")
			)
		}
	})

	#Slider fine tuning
	observeEvent(input$decLeft, {
		updateSliderInput(session, "time", value=c(input$time[[1]]-1, input$time[[2]]))
	})
	observeEvent(input$incLeft, {
		updateSliderInput(session, "time", value=c(input$time[[1]]+1, input$time[[2]]))
	})
	observeEvent(input$decRight, {
		updateSliderInput(session, "time", value=c(input$time[[1]], input$time[[2]]-1))
	})
	observeEvent(input$incRight, {
		updateSliderInput(session, "time", value=c(input$time[[1]], input$time[[2]]+1))
	})


	return
}
#TODO: Add real-time animation view back (rolling buffer)
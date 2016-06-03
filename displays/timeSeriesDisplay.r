#A Shiny plugin which creates a window for displaying time series plots.
#Created by Graham Home <grahamhome333@gmail.com>

#Proper Name
dispName <- function() {
	"Time Series Display"
}

#Compatible plot plugins
use_plots <- function() {
	list('map.R','heatmap.R','correlation.R', 'bar.R')
}

#UI
timeSeriesDisplayUI <- function(id) {
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
				column(8, offset=2, 
					imageOutput(ns("image"), height="auto", width="100%")
				)
			),
			fluidRow(
				column(4, offset=4,
					radioButtons(ns("activeMethod"), "Function:", fnames()[2:length(fnames())], inline=TRUE)
				)
			),
			fluidRow(
				column(1, 
					div(class="iconbox", actionLink(ns("frameBwd"), "", icon=icon("step-backward", "fa-2x"), class="icon"))
				),
				column(10,
					sliderInput(ns("time"), "Sample to examine",  min = 1, max = nsamples(), value = 1, width = "100%")
				),
				column(1,
					div(class="iconbox", actionLink(ns("frameFwd"), "", icon=icon("step-forward", "fa-2x"), class="icon"))
				) 
			),
			fluidRow(
				column(2, offset=1, style="padding-top:2%;text-align:right",
					p("Sample range to animate:", style="font-weight:bold")
				),
				column(2,
					uiOutput(ns("startContainer"))
				),
				column(2, 
					uiOutput(ns("stopContainer"))
				),
				column(2,
					selectInput(ns("speed"), "Animation Speed:", choices=list("Half Speed"=0.5, "Normal Speed"=1, "Double Speed"=2), selected=1)
				)
			),
			fluidRow(
				column(12,
					div(style="width:100%;color:red;text-align:center", textOutput(ns("result")))
				)
			),
			fluidRow(
				column(4, offset=4,
					br(),
					div(class="backiconbox", uiOutput(ns("toggle")))
				)
			)
		)
	)
}

#Server logic
timeSeriesDisplay <- function(input, output, session) {
	#Get namespace function
	ns <- session$ns

	#Counter variable
	counter <- 0

	state <- reactiveValues()
	#Play/pause variable
	state$playing <- FALSE
	#Range variables
	state$start <- 0
	state$stop <- 0
	#Speed variable
	state$speed <- 0

	output$toggle <- renderUI({ actionLink(ns("play"), "", icon=icon("play", "fa-2x"), class="icon") })

	#Create range start selector
	output$startContainer <- renderUI({
		ns <- session$ns
		numericInput(ns("start"), "Start", value=1, min=1, max=nsamples()) #TODO: set max reactively based on value of "stop" (use updateNumericInput)
	})
	#Create range end selector
	output$stopContainer <- renderUI({
		ns <- session$ns
		numericInput(ns("stop"), "Stop", value=2, min=1, max=nsamples()) #TODO: set min reactively based on value of "start" (use updateNumericInput)
	})
	#Switch to index-based display mode
	observeEvent(c(input$time, input$activeMethod), {
		if (!state$playing) {
			makeFiles2(input$time, input$time, paste(input$activeMethod, "_png", sep=""))
			output$image <- renderImage({
				list(src = paste("plots/img/", paste(input$activeMethod, "_png", sep=""), "/", name(), "/", input$time, ".png", sep=""), height="100%", width="100%")
			}, deleteFile=FALSE)
		}	
	})
	#Switch to animation display mode
	observeEvent(input$play, {
		if (!state$playing) {
			if ((input$start > input$stop) | (input$start < 1) | (input$stop > nsamples())) {
				output$result <- renderText("Invalid range")
			} else {
				output$toggle <- renderUI({ actionLink(ns("play"), "", icon=icon("pause", "fa-2x"), class="icon") })
				output$result <- renderText("")
				#Read start and stop values one time only
				state$start <- isolate(input$start)
				state$stop <- isolate(input$stop)
				state$speed <- isolate(as.numeric(input$speed))
				makeFiles2(state$start, state$stop, paste(input$activeMethod, "_png", sep=""))
				state$playing <- !state$playing
			}
		} else {
			state$playing <- !state$playing
			output$toggle <- renderUI({ actionLink(ns("play"), "", icon=icon("play", "fa-2x"), class="icon") })
		}
		
	})

	#Play animation
	observeEvent(state$playing, {
		if (state$playing) {
			method <- isolate(input$activeMethod)
			output$image <- renderImage({
				invalidateLater(100*state$speed)
				if ((state$start+counter) >= state$stop) {
					counter <<- 0 # this will restart the animation, or I could turn off the scheduled invalidation to end it
				} else {
					counter <<- counter + 1
				}
				updateSliderInput(session, "time", value=state$start+counter)
				list(src = paste("plots/img/", paste(method, "_png", sep=""), "/", name(), "/", input$time, ".png", sep=""), height="100%", width="100%")
			}, deleteFile=FALSE)
		}
	})

	#Seek backward one frame
	observeEvent(input$frameBwd, {
		updateSliderInput(session, "time", value=input$time-1)
	})
	#Seek forward one frame
	observeEvent(input$frameFwd, {
		updateSliderInput(session, "time", value=input$time+1)
	})
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
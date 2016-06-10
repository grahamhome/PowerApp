#A Shiny plugin which creates a window for displaying time series plots.
#Created by Graham Home <grahamhome333@gmail.com>

#Proper Name
dispName <- function() {
	"Correlation Time Series Display"
}

#Compatible plot plugins
use_plots <- function() {
	list('correlation.R')
}

#UI
corrTimeSeriesDisplayUI <- function(id) {
	#Create namespace function from id
	ns <- NS(id)
	#Enclose UI contents in a tagList
	tagList(
		fixedPanel(class="mainwindow",
			fluidRow(
				column(1,
					actionLink(ns("back"), "", icon=icon("arrow-left", "fa-2x"), class="icon"),
					div(style="padding-top:40%;padding-left:10%", radioButtons(ns("activeMethod"), "Function:", fnames()[2:length(fnames())]))
				),
				column(10,
					h2(name()),
					imageOutput(ns("image"), height="auto", width="100%")
				),
				column(1, 
					div(class="helpiconbox", actionLink(ns("help"), "", icon=icon("question", "fa-2x"), class="icon"))
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
					selectInput(ns("speed"), "Animation Speed:", choices=list("Slow"=0.1, "Normal Speed"=1, "Double Speed"=2), selected=1)
				),
				column(2,
					div(style="color:red;padding-top:30px", textOutput(ns("result")))
				)
			),
			fluidRow(
				column(4, offset=4,
					div(class="backiconbox", uiOutput(ns("toggle")))
				)
			)
		),
		uiOutput(ns("helpbox"))
	)
}

#Server logic
corrTimeSeriesDisplay <- function(input, output, session) {
	#Get namespace function
	ns <- session$ns

	#Counter variable
	counter <- 0
	
	#Reactive values related to window state
	state <- reactiveValues()
	#Play/pause variable
	state$playing <- FALSE
	#Range variables
	state$start <- 0
	state$stop <- 0
	#Speed variable
	state$speed <- 0
	#Show/hide help text
	state$showHelp <- FALSE

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
			makeFilesProgress(input$time, input$time, input$activeMethod)
			output$image <- renderImage({
				list(src = paste("plots/img/", input$activeMethod, "/", name(), "/", input$time, ".png", sep=""), height="100%", width="100%")
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
				makeFilesProgress(state$start, state$stop, input$activeMethod)
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
				invalidateLater(100/state$speed)
				updateSliderInput(session, "time", value=state$start+counter)
				if ((state$start+counter) == state$stop) {
					counter <<- 0 # this will restart the animation, or I could turn off the scheduled invalidation to end it
				} else {
					counter <<- counter + 1
				}
				list(src = paste("plots/img/", method, "/", name(), "/", input$time, ".png", sep=""), height="100%", width="100%")
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
		state$playing <- FALSE
		output$toggle <- renderUI({ actionLink(ns("play"), "", icon=icon("play", "fa-2x"), class="icon") })
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
				p("Use the slider or the forward/back buttons to view individual frames of the power data. ", br(), br(),
					"To view an animated sequence of power data values, specify the start and end frames of the ", br(), 
					"sequence in the 'Start' and 'Stop' boxes, select an animation speed and press the play icon. ", br(),  
					"Longer animations may have longer rendering times before they can be played. To change the ", br(), 
					"animation parameters, first pause the currently playing animation, then click the play icon ", br(),
					"again after changing the start, stop, or speed values. ", br(), br(),
					"Use the radio buttons on the left side of the graph display to change the plotting method used ", br(),
					"to create the graph.", br(), br(),
					"Use the back button in the top left corner of the display to choose a different plot type or data set.")
			)
		}
	})

	#Sequentially creates a set of plot images for the given method in the given directory over the given range.
	#Uses a proxy method that saves the resulting plot objects to PNG files.
	makeFilesProgress <- function(start, stop, method) {
		path <- paste("plots/img/", method, "/", name(), "/", sep="")
		#Create directory for image files if it does not exist
		dir.create(file.path("plots/", "img"), showWarnings=FALSE)
		dir.create(file.path("plots/img/", method), showWarnings=FALSE)
		dir.create(file.path(paste("plots/img/", method, sep=""), name()), showWarnings=FALSE)
		#Create any image files that do not yet exist
		output$image <- renderImage({
			withProgress(message="Creating Plot", detail="", value=0, {
				for (t in start:stop) {
					if (!(file.exists(paste(path, t, ".png", sep="")))) {
						plotCorr2png(paste(method, "(", t, ")", sep=""), paste(path, t, ".png", sep=""))
					}
					incProgress(1/(stop-start))
				}
			})
			list(src = paste("plots/img/", method, "/", name(), "/", start, ".png", sep=""), height="100%", width="100%")
		}, deleteFile=FALSE)
	return
	}
}
#A Shiny plugin which creates a window for displaying time series plots.
#Created by Graham Home <grahamhome333@gmail.com>

#Proper Name
dispName <- function() {
	"Interactive Voltage Display"
}

#Compatible plot plugins
use_plots <- function() {
	list('map.R')	#TODO: change to contour plot only
}

#UI
interactiveVoltageDisplayUI <- function(id) {
	#Create namespace function from id
	ns <- NS(id)

	#Enclose UI contents in a tagList
	tagList(
		fixedPanel(class="mainwindow",
			fluidRow(
				column(1,
					actionLink(ns("back"), "", icon=icon("arrow-left", "fa-2x"), class="icon")
				),
				column(10, 
					h2(name())
				),
				column(1, 
					div(class="helpiconbox", actionLink(ns("help"), "", icon=icon("question", "fa-2x"), class="icon"))
				)
			),
			uiOutput(ns("display")),
			fluidRow(
				column(2, offset=5, 
					uiOutput(ns("zoomBtnBox"))
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
			uiOutput(ns("lowerDisplay"))
		),
		uiOutput(ns("helpbox"))
	)
}

#Server logic
interactiveVoltageDisplay <- function(input, output, session) {
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

	#Plotting method
	state$method <- "plot_bus_volt"	#TODO: Change to contour plot method

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
	observeEvent(c(input$time), priority=1, {
		if (!state$playing) {
			#makeFilesProgress(input$time, input$time)
			showPlot()
			output$plot <- renderPlot({
				eval(parse(text=paste(state$method, "(", input$time, ",", FALSE, ")", sep="")))
			})
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
				makeFilesProgress(state$start, state$stop)
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
			#Only default scale used in this viewer
			scale = "defaultScale" 
			showImage()

			output$image <- renderImage({
				invalidateLater(100/state$speed)
				updateSliderInput(session, "time", value=state$start+counter)
				if ((state$start+counter) == state$stop) {
					counter <<- 0 #This will restart the animation
				} else {
					counter <<- counter + 1
				}
				list(src = paste("plots/img/", scale, "/", state$method, "/", name(), "/", input$time, ".png", sep=""), height="100%", width="100%")
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

	#Image display
	showImage <- function() {
		output$display <- renderUI({
			imageOutput(ns("image"), height="auto", width="100%")
		})
	}

	#Plot display
	showPlot <- function() {
		output$display <- renderUI({
			div(style="width:100%; height:auto; position:relative; float:left",
				plotOutput(ns("plot"), click=ns("pltClk"))
			)
		})
	}

	#Animation controls
	showControls <- function() {
		output$lowerDisplay <- renderUI({
			tagList(
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
		})
	}

	#Show animation controls by default
	showControls()

	#Shows details about the current buses instead of the animation controls (used when plot zooms)
	showDetails <- function() {
		output$lowerDisplay <- renderUI({
			#TODO: Details go here
		})
	}

	#Show zoom button
	showZoom <- function() {
		output$zoomBtnBox <- renderUI({
			actionButton(ns("resetPlot"), "Zoom Out")
		})
	}

	#Hide zoom button
	hideZoom <- function() {
		output$zoomBtnBox <- renderUI({
		})
	}

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
					"When the graph is paused (not playing an animation), click near a bus to zoom in on the region.", br(),
					"Click 'Zoom Out' to view the entire graph and restore the animation controls.", br(), br(),
					"Use the back button in the top left corner of the display to choose a different plot type or data set.")
			)
		}
	})

	#Click detection
	observeEvent(input$pltClk, {

		#Get x and y values of click
		point <- c(input$pltClk$x, input$pltClk$y)

		#Get map lat/long ratio
		ratio <- (max(bus_locs$Latitude)-min(bus_locs$Latitude))/(max(bus_locs$Longitude)-min(bus_locs$Longitude))

		#Get closest bus cluster
		busses <- bus_locs[with(bus_locs, (Latitude >= point[2]-0.2 & Latitude <= point[2]+0.2) & (Longitude >= point[1]-0.2 & Longitude <= point[1]+0.2)),]
		#print(busses)

		#Zoom
		output$plot <- renderPlot ({
			eval(parse(text=paste(state$method, "(", input$time, ",", TRUE, ",", point, ")", sep="")))
		})




		#Zoom to area around click
		# output$plot <- renderPlot({
	 	# 		zoomPlot(x-1, y-1, x+1, y+1)
		# 	eval(parse(text=paste(state$method, "(", input$time, ")", sep="")))
	 	# 	})

		showDetails()
	 	showZoom()
	})

	#Zoom out function
	observeEvent(input$resetPlot, {
		hideZoom()
		output$plot <- renderPlot({
			eval(parse(text=paste(state$method, "(", input$time, ")", sep="")))
		})
		showControls()
	})

	#Uses parallel processing to create a set of plot images over the given range.
	makeFilesProgress <- function(start, stop) {
		#Only default scale used in this viewer
		scale = "defaultScale"
		path <- paste("plots/img/", scale, "/", state$method, "/", name(), "/", sep="")
		#Create directory for image files if it does not exist
		dir.create(file.path("plots/", "img"), showWarnings=FALSE)
		dir.create(file.path("plots/img/", scale), showWarnings=FALSE)
		dir.create(file.path(paste("plots/img/", scale, "/", sep=""), state$method), showWarnings=FALSE)
		dir.create(file.path(paste("plots/img/", scale, "/", state$method, "/", sep=""), name()), showWarnings=FALSE)
		#Create list of image files that do not yet exist
		output$image <- renderImage({
			withProgress(message="Creating Plot", detail="", value=0, {
				for (t in start:stop) {
					if (!(file.exists(paste(path, t, ".png", sep="")))) {
						plot2png(paste(state$method, "(", t, ")", sep=""), paste(path, t, ".png", sep=""))
					}
					incProgress(1/(stop-start))
				}
			})
			list(src = paste("plots/img/", scale, "/", state$method, "/", name(), "/", start, ".png", sep=""), height="100%", width="100%")
		}, deleteFile=FALSE)
		return
	}
}
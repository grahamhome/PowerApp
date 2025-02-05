#A Shiny plugin which creates a window for displaying time series plots, using parallel processing to generate plot images.

#Author: Graham Home <grahamhome333@gmail.com>

#Dependencies for parallel processing
library(foreach)
library(doParallel)

#Proper Name
dispName <- function() {
	"Time Series Display"
}

#Compatible plot plugins
use_plots <- function() {
	list('map.R','heatmap.R', 'bar.R', 'histogram.R')
}

#UI
timeSeriesDisplayUI <- function(id) {
	#Create namespace function from id
	ns <- NS(id)
	#Enclose UI contents in a tagList
	tagList(
		fixedPanel(class="mainwindow",
			fluidRow(
				column(1,
					actionLink(ns("back"), "", icon=icon("arrow-left", "fa-2x"), class="icon")
				),
				column(1, offset=10,
					div(class="helpiconbox", actionLink(ns("help"), "", icon=icon("question", "fa-2x"), class="icon"))
				)
			),
			fluidRow(
				column(2, offset=1,
					h2(name())
				),
				column(2, offset=1,
						selectInput(ns("activeMethod"), "Function:", fnames()[2:length(fnames())])
				),
				column(2,
					div(style="padding-top:23px",
						checkboxInput(ns("rescale"), "Auto-Scale Plot", TRUE)
					)
				)
			),
			fluidRow(
				column(10, offset=1,
					div(style="padding-left:10%",
						imageOutput(ns("image"), height="auto", width="90%")
					)
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
		),
		uiOutput(ns("helpbox"))
	)
}

#Server logic
timeSeriesDisplay <- function(input, output, session) {
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

	#Create "play" button
	output$toggle <- renderUI({ actionLink(ns("play"), "", icon=icon("play", "fa-2x"), class="icon") })

	#Create range start selector
	output$startContainer <- renderUI({
		ns <- session$ns
		numericInput(ns("start"), "Start", value=1, min=1, max=nsamples())
	})
	#Create range end selector
	output$stopContainer <- renderUI({
		ns <- session$ns
		numericInput(ns("stop"), "Stop", value=2, min=1, max=nsamples())
	})

	#Switch to index-based display mode
	observeEvent(c(input$time, input$activeMethod, input$rescale), priority=1, {
		#If an animation is not currently playing
		if (!state$playing) {
			#Generate image for selected frame
			makeFiles(input$time, input$time, input$activeMethod)
			if (input$rescale) {
				scale = "autoScale"
			} else {
				scale = "defaultScale"
			}
			#Display generated image
			output$image <- renderImage({
				list(src = paste("plots/img/", scale, "/", input$activeMethod, "/", name(), "/", input$time, ".png", sep=""), height="100%", width="100%")
			}, deleteFile=FALSE)
		}	
	})

	#Switch to animation display mode
	observeEvent(input$play, {
		#If an animation is not currently playing
		if (!state$playing) {
			#Check for valid range (1 <= start < stop <= total # of frames in dataset)
			if ((input$start > input$stop) | (input$start < 1) | (input$stop > nsamples())) {
				output$result <- renderText("Invalid range")
			} else {
				#Show "pause" icon when animation starts
				output$toggle <- renderUI({ actionLink(ns("play"), "", icon=icon("pause", "fa-2x"), class="icon") })
				#Read start and stop values one time only
				state$start <- isolate(input$start)
				state$stop <- isolate(input$stop)
				state$speed <- isolate(as.numeric(input$speed))
				#Show progress indicator so user knows that the images are being rendered, even though it doesn't update until the end
				withProgress(message="Creating Plot, Please Wait...", detail="", value=0, {
					#Create images for selcted frames
					makeFiles(state$start, state$stop, input$activeMethod)
					#Update progress bar when done
					incProgress(1)
				})
				#Clear any displayed message and play animation
				output$result <- renderText("")
				state$playing <- !state$playing
				if (input$rescale) {
					scale = "autoScale"
				} else {
					scale = "defaultScale"
				}
				method <- isolate(input$activeMethod)
				#Display image for current frame and update current frame
				output$image <- renderImage({
					if(state$playing) {	#This is needed because invalidateLater() causes this function to be called even after the pause button is pushed.
						invalidateLater(100/state$speed)
						updateSliderInput(session, "time", value=state$start+counter)
						if ((state$start+counter) == state$stop) {
							counter <<- 0 #This will restart the animation
						} else {
							counter <<- counter + 1
						}
					}
					list(src = paste("plots/img/", scale, "/", method, "/", name(), "/", input$time, ".png", sep=""), height="100%", width="100%")
				}, deleteFile=FALSE)
			}
		} else {
			#Stop the currently playing animation
			state$playing <- !state$playing
			#Show "play" icon
			output$toggle <- renderUI({ actionLink(ns("play"), "", icon=icon("play", "fa-2x"), class="icon") })
		}
		
	})

	#Play animation
	observeEvent(state$playing, {
		#If an animation is playing
		
	})

	#Seek backward one frame
	observeEvent(input$frameBwd, {
		updateSliderInput(session, "time", value=input$time-1)
	})

	#Seek forward one frame
	observeEvent(input$frameFwd, {
		updateSliderInput(session, "time", value=input$time+1)
	})

	#Go back to plot or display picker
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
					"Use the drop-down menu above the graph display to change the plotting method used ", br(),
					"to create the graph.", br(), br(),
					"Clicking 'Auto-Scale Plot' will adjust the range of potential values to match the values of the current sample.", br(), br(),
					"Use the back button in the top left corner of the display to choose a different plot type or data set.")
			)
		}
	})

	#Scale adjustment
	observeEvent(input$rescale, priority=0, {
		autoscale()
	})

	#Uses parallel processing to create a set of plot images for the given method in the given directory over the given range.
	makeFiles <- function(start, stop, method) {
		if (input$rescale) {
			scale = "autoScale"
		} else {
			scale = "defaultScale"
		}
		#Path to image directory
		path <- paste("plots/img/", scale, "/", method, "/", name(), "/", sep="")
		#Create directory for image files if it does not exist
		dir.create(file.path("plots/", "img"), showWarnings=FALSE)
		dir.create(file.path("plots/img/", scale), showWarnings=FALSE)
		dir.create(file.path(paste("plots/img/", scale, "/", sep=""), method), showWarnings=FALSE)
		dir.create(file.path(paste("plots/img/", scale, "/", method, "/", sep=""), name()), showWarnings=FALSE)
		#Create list of image files that do not yet exist
		files <- list()
		for (i in start:stop) {
			if (!(file.exists(paste(path, i, ".png", sep="")))) {
				files[[length(files)+1]] <- i
			}
		}
		#Create any image files that do not yet exist
		#Only use parallel processing if >= (#cores-1) images need to be made, otherwise there's no advantage to using parallel processing.
		if (length(files) >= (detectCores()-1)) {
			#Set up parallel backend to use all but 1 of the available processors
			cl<-makeCluster(detectCores()-1)
			registerDoParallel(cl)
			foreach(t=1:length(files), .packages=c("ggplot2", "ggmap", "rgdal", "raster", "akima", "sp"), .export=c(ls(globalenv()), "start", "stop")) %dopar% { #TODO: Figure out how to not hardcode packages
				plot2png(paste(method, "(", files[[t]], ")", sep=""), paste(path, files[[t]], ".png", sep=""))
			}
			stopCluster(cl)
		}
		else {
			#Use sequential processing if < (#cores-1) images are to be made
			for (file in files) {
				plot2png(paste(method, "(", file, ")", sep=""), paste(path, file, ".png", sep=""))
			}
		}
		return
	}
}
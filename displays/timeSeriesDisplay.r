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
				column(12,
					uiOutput(ns("pltWin")),
					radioButtons(ns("activeMethod"), "Function:", fnames()[c(2, length(fnames()))], inline=TRUE),
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
						column(2, offset=2, style="padding-top:2%;text-align:right",
							p("Sample range to animate:")
						),
						column(2,
							uiOutput(ns("startContainer"))
						),
						column(2, 
							uiOutput(ns("stopContainer"))
						),
						column(2, style="padding-top:50px",
							textOutput(ns("result"))
						)
					),
					br(),
					column(4, offset=4,
						div(class="backiconbox", actionLink(ns("play"), "", icon=icon("play", "fa-2x"), class="icon"))
					)
				)
			)	
		)
	)
}

#Server logic
timeSeriesDisplay <- function(input, output, session) {
	#Get namespace function
	ns <- session$ns

	#Create counter variable
	counter <- 0

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
	observeEvent(input$time, {
		output$pltWin <- renderUI({plotOutput(ns("plot"), height="400px", width="1000px")}) #TODO: Size reactively based on window size})
		output$plot <- renderPlot({eval(parse(text=paste(input$activeMethod, "(", input$time, ")", sep="")))})
	})
	#Switch to animation display mode
	observeEvent(input$play, {
		output$pltWin <- renderUI({imageOutput(ns("image"), height="400px", width="1000px")}) #TODO: Size reactively based on window size, and center horizontally
		if (input$start > input$stop) {
			output$result <- renderText("Invalid range")
		} else {
			#Read start and stop values one time only
			start <- isolate(input$start)
			stop <- isolate(input$stop)
			output$result <- renderText("Generating animation...")
			#Create directory for image files if it does not exist
			dir.create(file.path("plots/", "img"), showWarnings=FALSE)
			dir.create(file.path("plots/img/", input$activeMethod), showWarnings=FALSE)
			dir.create(file.path(paste("plots/img/", input$activeMethod, sep=""), name()), showWarnings=FALSE)
			#Create string representing directory 
			dir <- paste("plots/img/", input$activeMethod, "/", name(), "/", sep="")
			#Create any image files that do not yet exist
			for (t in start:stop) {
				if (!(file.exists(paste(dir, t, ".png", sep="")))) {
					plotpng(eval(parse(text=paste(input$activeMethod, "(", t, ")", sep=""))), t, dir)
				}
			}
			print("all files generated")
			#Play animation
			output$result <- renderText("")
			output$image <- renderImage({
				invalidateLater(100)
				if ((start+counter) >= stop) {
    				counter <<- 0 # this will restart the animation, or I could turn off the scheduled invalidation to end it
    			} else {
    				counter <<- counter + 1
    			}
 				list(src = paste(dir, start+counter, ".png", sep=""), height="100%", width="100%")
  			}, deleteFile=FALSE)
		}

		# output$image <- renderImage({
		# 	list(src = paste("plots/img/plot_mapvolt/Solar Flare/", "1.png", sep=""), height="100%", width="100%")
		# }, deleteFile=FALSE)
	})

	#Seek backward one frame
	observeEvent(input$frameBwd, {
		updateSliderInput(session, "time", value=input$time-1)
	})
	#Seek forward one frame
	observeEvent(input$frameFwd, {
		updateSliderInput(session, "time", value=input$time+1)
	})
	# observeEvent(input$play, {
	# 	if (input$start > input$stop) {
	# 		output$result <- renderText("Invalid range")
	# 	} else {
	# 		output$result <- renderText("Generating animation...")
	# 		#Create directory for image files if it does not exist
	# 		dir.create(file.path("plots/", "img"), showWarnings=FALSE)
	# 		dir.create(file.path("plots/img/", input$activeMethod), showWarnings=FALSE)
	# 		dir.create(file.path(paste("plots/img/", input$activeMethod, sep=""), name()), showWarnings=FALSE)
	# 		#Create string representing directory 
	# 		dir <- paste("plots/img/", input$activeMethod, "/", name(), "/", sep="")
	# 		#Create any image files that do not yet exist
	# 		for (t in input$start:input$stop) {
	# 			if (!(file.exists(paste(dir, t, ".png", sep="")))) {
	# 				plotpng(eval(parse(text=paste(input$activeMethod, "(", t, ")", sep=""))), t, dir)
	# 			}
	# 		}
	# 		#Play animation
	# 		print("all files generated")
	# 		output$result <- renderText("")
	# 		for (t in input$start:input$stop) {
	# 			Sys.sleep(1)
	# 			print("displaying next image")
	# 			output$plot <- renderImage({
	# 				list(src = paste(dir, t, ".png", sep=""), height="100%", width="100%")
	# 			}, deleteFile=FALSE)
	# 		}
	# 	}
	# })

	# playing <- FALSE #Indicates if an animation is currently playing
	# output$plotWindow <- renderUI({
	# 	if (!playing) {
	# 		plotOutput(ns("plot"))
	# 	} else {
	# 		imageOutput(ns("plot"))
	# 	}
	# })
	# if (!playing) {
	# 	output$plot <- renderPlot(eval(parse(text=paste(activeMethod, "(", input$time, ")", sep=""))))
	# } else {
	
	# 	observeEvent(input$play, {
	# 		if (!(playing)) {
	# 			#Create directory for image files if it does not exist
	# 			dir.create(file.path("plots/", "img"), showWarnings=FALSE)
	# 			dir.create(file.path("plots/img/", input$activeMethod), showWarnings=FALSE)
	# 			dir.create(file.path(paste("plots/img/", input$activeMethod, sep=""), name()), showWarnings=FALSE)
	# 			#Create string representing directory 
	# 			dir <- paste("plots/img/", input$activeMethod, "/", name(), "/", sep="")
	# 			#Create any image files that do not yet exist
	# 			for (t in input$start:input$stop) {
	# 				if (!(file.exists(paste(dir, t, ".png", sep="")))) {
	# 					plotpng(eval(parse(text=paste(input$activeMethod, "(", t, ")", sep=""))), t, dir)
	# 				}
	# 			}
	# 			#Play animation
	# 			for (t in input$start:input$stop) {
	# 				output$plot <- renderImage({
	# 					list(src = paste(dir, t, ".png", sep=""), height="100%", width="100%")
	# 				}, deleteFile=FALSE)
	# 				Sys.sleep(0.1)
	# 			}
	# 		}
	# 		playing <<- !(playing)
	# 	})
	# }
	# observeEvent(input$back, {
	# 	#Did the display picker launch?
	# 	if (length(plugins$compatDisplays) == 1) {
	# 		launchUI("plotPicker()")
	# 	} else {
	# 		launchUI("displayPicker()")
	# 	}
	# })
	return
}

#TODO: Add real-time animation view back (rolling buffer)
#Introduction screen module

#Introduction screen UI
introUI <- function(id) {
	#Create namespace function from id
	ns <- NS(id)
	#Enclose UI contents in a tagList
	tagList(
		fixedPanel(class="mainwindow_inactive",
			fixedPanel(class="popup",

				fluidRow(
			  		column(8, offset=2,
			  			h1("Welcome to Power Viewer!", class="windowtitle")
			  		)
			  	),
			  	fluidRow(
					column(12,
			  			h3("Power Viewer is a tool for viewing power grid data with a library of plotting methods.
			  				The next two screens will allow you to choose from the available data sets and plot styles.
			  				Ready to begin?", class="instructions")
			  		)
				),
				actionButton(ns("forward"), "Start", class="next")
			)
		)
	)
}

#Server logic
intro <- function(input, output, session) {
	observeEvent(input$forward, {
		print("button pressed")
	})
	return
}

testIntro <- function() {
	print("Intro has definately been imported at this point")
}
#Plot selection activity 
plotPicker <- function() {
	fixedPanel(class="mainwindow_inactive",
		fixedPanel(class="popup",
			fluidRow(
				column(2,
					actionLink("back", "", icon=icon("arrow-left", "fa-2x"), class="icon")
				),
				column(8,
					h1("Plot Type", class="windowtitle")
				)
			),
			fluidRow(
				column(8, offset=2,
					h3("Select a plot type:"),
					br(),
					selectInput("plot", "", modules$compatPlots),
					p("Please be patient, plot will take approximately 5 seconds to load after clicking 'Select Plot' button.")
				)
			),
			actionButton("forward", "Select Plot", class="next")
		)
	)
}
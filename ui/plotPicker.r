#Plot selection activity 
plotSelection <- function() {
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
					selectInput("plottype", "Plot Type", files$plotNames)
				)
			),
			actionButton("selectPlot", "Select Plot", class="next")
		)
	)
}
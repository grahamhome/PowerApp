#Display selection activity 
displayPicker <- function() {
	fixedPanel(class="mainwindow_inactive",
		fixedPanel(class="popup",
			fluidRow(
				column(2,
					actionLink("back", "", icon=icon("arrow-left", "fa-2x"), class="icon")
				),
				column(8,
					h1("Display", class="windowtitle")
				)
			),
			fluidRow(
				column(8, offset=2,
					h3("Select a plot type:"),
					br(),
					selectInput("display", "", plugins$compatDisplays)
				)
			),
			actionButton("forward", "Select Display", class="next")
		)
	)
}
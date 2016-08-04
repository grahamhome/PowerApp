#Display selection activity. Designed to be run from app.r,
#not as a standalone Shiny application.

#Author: Graham Home <grahamhome333@gmail.com>

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
					h3("Select a display:"),
					br(),
					selectInput("display", "", plugins$compatDisplays)
				)
			),
			actionButton("forward", "Select Display", class="next")
		)
	)
}
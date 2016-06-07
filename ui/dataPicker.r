#Data selection activity
dataPicker <- function() {
	fixedPanel(class="mainwindow_inactive",
		fixedPanel(class="popup",
			fluidRow(
				column(2,
					actionLink("back", "", icon=icon("arrow-left", "fa-2x"), class="icon")
				),
				column(8,
					h1("Data Set", class="windowtitle")
				)
			),
			fluidRow(
				column(8, offset=2,
					h3("Select a data set:"),
					br(),
					selectInput("data", "", plugins$data),
					br(),
					p("Please be patient, data will take approximately 5 seconds to load after clicking 'Import Data'.", style="font-weight:bold")
				)
			),
			actionButton("forward", "Import Data", class="next")
		)
	)
}
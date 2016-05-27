#Data selection activity
dataSelection <- function() {
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
					selectInput("dataset", "", names(files$imports))
				)
			),
			actionButton("selectData", "Import Data", class="next")
		)
	)
}
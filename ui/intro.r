#Introduction screen
intro <- function() {
	print("intro is being called")
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
		  				Ready to begin?", style="line-height:150%; text-align:center")
		  		)
			),
			actionButton("forward", "Start", class="next")
		)
	)
}
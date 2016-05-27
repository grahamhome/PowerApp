#Window for displaying time series plots
timeSeriesDisplay <- function() {
	fixedPanel(class="mainwindow",
		fluidRow(
			column(2,
				actionLink("back", "", icon=icon("arrow-left", "fa-2x"), class="icon")
			),
			column(8,
				h1("Plot", class="windowtitle") #TODO: Replace with a reactive title based on plot type & data set
			)
		),
		fluidRow(
			column(1,
				p("Function"),
				p("List"),
				p("Here")
			),
			column(11,
				#do.call(tabsetPanel, makeFunctionTabs()), #This breaks my back button...but why?!?!?!
				plotOutput("plot", height="400px", width="100%"),
				sliderInput("time", "Time range to examine",  min = 1, max = 100, value = 1, width = "100%"), #TODO: set max/min reactively
				br(),
				column(4, offset=4,
					div(style="text-align:center", actionLink("play", "", icon=icon("play", "fa-2x"), class="icon"))
				)
			)
		)
	)
}

#TODO: Add real-time animation view back (rolling buffer)
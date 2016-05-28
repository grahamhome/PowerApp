#Proper Name
name <- function() {
	"Time Series Display"
}

#Compatible plot modules
use_plots <- function() {
	list("'linear.R','map.R','heatmap.R','correlation.R'")
}

#Window for displaying time series plots
timeSeriesDisplay <- function(input, output, session) {
	fixedPanel(class="mainwindow",
		fluidRow(

			column(2,
				actionLink("back", "", icon=icon("arrow-left", "fa-2x"), class="icon")
			)
		),
		fluidRow(
			column(12,

				#plotOutput("plot", height="400px", width="100%"), #TODO: Size reactively based on window size
				output$plot <- renderPlot(eval(parse(text=paste(input$activeMethod, "(", input$time, ")", sep="")))),
				radioButtons("activeMethod", "Function:", fnames()[c(2, length(fnames()))], inline=TRUE),
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
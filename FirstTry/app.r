 #A Shiny app which uses ggvis to display real-time statistical analysis of PMU data from a power grid.
library(shiny)
library(shinythemes)

source(file="read_data.r")

#UI (webpage) is defined here
ui <- fluidPage(
	theme = shinytheme("spacelab"),
	titlePanel("Change in Bus Frequency over Time"),
	plotOutput("map"),
	hr(),
	fluidRow(
		column(12,
			sliderInput(inputId="time", label="Choose a time value to examine", value=25, min=1, max=3606)
		)
	)
)

#R functionality is defined here
server <- function(input, output) {
	output$map <- renderPlot({
		plotmapfreq(input$time)
		})

}

#Start the app
app = shinyApp(ui=ui, server=server)
runApp(app, port=5678)


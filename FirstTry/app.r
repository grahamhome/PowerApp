 #A Shiny app which uses ggvis to display real-time statistical analysis of PMU data from a power grid.
library(shiny)

source(file="read_data.r")

#UI (webpage) is defined here
ui <- fluidPage(
	sliderInput(inputId="num", label="Choose a time value to examine", value=25, min=1, max=1000),
	#sliderInput(inputId="num", label="Choose a number of samples", value=25, min=1, max=100),
	plotOutput("plot")
	)

#R functionality is defined here
server <- function(input, output) {
	output$plot <- renderPlot({
		plotmapvoltsingle(input$num)
		#hist(rnorm(input$num))
		})

}

#Start the app
shinyApp(ui=ui, server=server)


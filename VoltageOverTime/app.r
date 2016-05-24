 #A Shiny app which uses ggvis to display real-time statistical analysis of PMU data from a power grid.
library(shiny)
library(plotly)

source(file="read_data.r")

#UI (webpage) is defined here - inputs and outputs
ui <- fluidPage(

  titlePanel("Changes in Bus Voltage Over Time"),

  plotlyOutput("plot"),

  hr(),

  fluidRow(
  	column(12,
  		sliderInput("time", "Time range to examine",  min = 1, max = 845, value = c(500, 750)),
  		radioButtons("speed", "Animation speed", c("Half speed"=0.5, "Normal speed"=1, "Double speed"=2), inline=TRUE),
  		actionButton("start", "Go!")
  	)
  )
)

#R functionality is defined here
server <- function(input, output) {
	output$plot <- renderPlotly({
		get_plotlyfreqmap(input$time[[1]])
		})

}

#Start the app
app <- shinyApp(ui=ui, server=server)
runApp(app, port=5678)


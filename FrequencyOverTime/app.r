 #A Shiny app which uses Plotly to display real-time statistical analysis of PMU data from a power grid.
library(shiny)
library(plotly)

source(file="read_data.r")

#UI (webpage) is defined here - inputs and outputs
ui <- fluidPage(

  titlePanel("Changes in Bus Frequency Over Time"),

  plotlyOutput("plot"),

  hr(),

  fluidRow(
  	column(12,
  		sliderInput("time", "Time range to examine",  min = 1, max = 845, value = c(500, 750)),
  		radioButtons("speed", "Animation speed", c("Half speed"=0.5, "Normal speed"=1, "Double speed"=2), selected=1, inline=TRUE),
  		actionButton("go", "Go!")
  	)
  )
)

#R functionality is defined here
server <- function(input, output) {

  #Create a new list of maps whenever the Go button is clicked
  maps <- eventReactive(input$go, {
    mapList <- list()
    for (t in input$time[[1]]:input$time[[2]]) {
      mapList[[(t+1)-input$time[[1]]]] <- get_plotlyfreqmap(t)
    }
    mapList
  })

  #Index for map list
  i <- 0

  #Render a plot by selecting a map from the list, updating the index every 10 ms.
	output$plot <- renderPlotly({
    invalidateLater(10)
    i <- (i+1)
		maps()[[i]]
		})

}

#Start the app
app <- shinyApp(ui=ui, server=server)
runApp(app, port=5678)


 #A Shiny app which uses Plotly to display real-time statistical analysis of PMU data from a power grid.
library(shiny)
library(plotly)

source(file="read_data.r")

#UI (webpage) is defined here - inputs and outputs
ui <- fluidPage(

  titlePanel("Correlation of Bus Voltages"),

  imageOutput("plot", height="600px", width="1500px"),

  hr(),

  fluidRow(
  	column(12,
  		sliderInput("time", "Time range to examine",  min = 1, max = 845, value = c(500, 750), width = "75%"),
  		radioButtons("speed", "Animation speed", c("Very slow"=10, "Normal speed"=1, "Very fast"=0.01), selected=1, inline=TRUE),
  		actionButton("go", "Go!")
  	)
  )
)

#R functionality is defined here
server <- function(input, output) {

	counter <- 0


  #Create new PNGs when the 'go' button is pressed
  observeEvent(input$go, {
  	start <- input$time[[1]]
  	end <- input$time[[2]]
    for (t in start:end) {
    	if (!file.exists(paste("img/", t, ".png", sep=""))) {
    		plotpng(get_corrplotvolt(t), t)
      		#png_plotmapfreq(t)
      	}
    }
    #Reset the counter
    counter <<- 0
  })

   #Update PNG start index when the 'go' button is pressed
   start <- eventReactive(input$go, {
   	input$time[[1]]
   	})

   #Update PNG stop index when the 'go' button is pressed
   stop <- eventReactive(input$go, {
   	input$time[[2]]
   	})

   #Update PNG animation speed value when the 'go' button is pressed
   speed <- eventReactive(input$go, {
   	as.numeric(input$speed)
   	})

  #Render a plot by selecting a map from the list, updating the index every 10 ms.
  output$plot <- renderImage({
  	speed <- speed()
  	start <- start()
  	stop <- stop()
  	invalidateLater(100*speed)
    if ((start+counter) >= stop) {
    	counter <<- 0 # this will restart the animation, or I could turn off the scheduled invalidation to end it
    } else {
    	counter <<- counter + 1
    }
    #print(start+counter)
 	list(src = paste("img/", start+counter, ".png", sep=""), height="100%", width="100%")
  }, deleteFile=FALSE)

}

#Start the app
app <- shinyApp(ui=ui, server=server)
runApp(app, port=5678)


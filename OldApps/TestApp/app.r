library(shiny)
ui <- fluidPage(

  titlePanel("Changes in Bus Voltage Over Time"),

  sidebarLayout(

    sidebarPanel(
      sliderInput("num", "Time value to examine",  
                  min = 1, max = 1000, value = 500)
    ),

    mainPanel(
      plotOutput("plot")
    )
  )
)

#R functionality is defined here
server <- function(input, output) {
  output$plot <- renderPlot({
    ggplotly(plotmapvoltsingle(input$num))
    #hist(rnorm(input$num))
    })

}

shinyApp(ui=ui, server=server)
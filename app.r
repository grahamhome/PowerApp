library(shiny)
ui <- fluidPage(

  titlePanel("Welcome to Power Viewer!"),
  #TODO: Add intro text and OK button to launch choose_data.r
)

#R functionality is defined here
server <- function(input, output) {
}

shinyApp(ui=ui, server=server)
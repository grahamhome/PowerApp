library(shiny)
ui <- fluidPage(

  titlePanel("Choose Plot Method"),
  #TODO: Add thumbnail grid for different compatible plot types, each tn imports the plot method & call display method.
)

#R functionality is defined here
server <- function(input, output) {
}

shinyApp(ui=ui, server=server)
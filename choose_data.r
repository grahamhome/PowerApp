library(shiny)
ui <- fluidPage(

  titlePanel("Choose Data Set"),
  #TODO: Add dropdown list populated by scanning data/ and Next button to run data importer & then choose_plot.r
)

#R functionality is defined here
server <- function(input, output) {
}

shinyApp(ui=ui, server=server)
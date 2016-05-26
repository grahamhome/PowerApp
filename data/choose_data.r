#Module which allows user to import a data set
#Author: Graham Home <grahamhome333@gmail.com>

#Dependencies
library(shiny)

choose_data <- function() {
	print("Totally importing data right now")
}

choose_data_plots <- function() {
	c("map.r", "linear.r", "read_data.r")
}

#Module UI is defined here - inputs and outputs
dataChooserUI <- function(id, label="Data Picker") {

	#First create namespace function
	ns <- NS(id)
}

#Module's R functionality is defined here
dataChooser <- function(input, output, session) {
	dataUI <- renderUI({
		tagList(ns("content"),
  			h3(ns("Choose a data set")),
  			selectInput(ns("dataset"), "Data Set", c("Solar Flare"="sf", "Line Down"="ld")), #TODO: generate this list reactively after scanning data/
  			actionButton(ns("select"), "Select")
		)
	})
	#TODO: scan data/
	#TODO: import selected data on button click

	return()
}


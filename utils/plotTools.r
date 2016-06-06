#Utility functions for displaying the results of plotting plugins.

#Given a plot, a time value, and a directory, creates an appropriately-sized PNG of the plot named with the time value.
plotpng <- function(g, t, dir) {
  ggsave(file=paste(dir, t, ".png", sep=""), plot=g, width=10, height=4, units="in")
}

#Given a function call (i.e. function name with arguments in parentheses) and a filename, saves the result of the function call with the specified filename.
plot2png <- function(functionCall, fileName) {
	png(filename=fileName, width=1000, height=400, units="px")
	print(eval(parse(text=functionCall)))
	dev.off()
}

#Given a function call (i.e. function name with arguments in parentheses) and a filename, saves the result of the function call with the specified filename.
#Specialized for correlation plots which are of a different type than ggplots.
plotCorr2png <- function(functionCall, fileName) {
	png(filename=fileName, width=1000, height=400, units="px")
	eval(parse(text=functionCall))
	dev.off()
}
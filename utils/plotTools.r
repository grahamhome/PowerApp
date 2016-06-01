#Utility functions for displaying the results of plotting plugins.

#Given a plot, a time value, and a directory, creates an appropriately-sized PNG of the plot named with the time value.
plotpng <- function(g, t, dir) {
  ggsave(file=paste(dir, t, ".png", sep=""), plot=g, width=10, height=4, units="in")
}
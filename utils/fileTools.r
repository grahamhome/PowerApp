#Tools for creating files.

#Sequentially creates a set of plot images for the given method in the given directory over the given range.
#Uses a proxy method that saves the resulting plot objects to PNG files.
makeFiles <- function(start, stop, method) {
	path <- paste("plots/img/", method, "/", name(), "/", sep="")
	#Create directory for image files if it does not exist
	dir.create(file.path("plots/", "img"), showWarnings=FALSE)
	dir.create(file.path("plots/img/", method), showWarnings=FALSE)
	dir.create(file.path(paste("plots/img/", method, sep=""), name()), showWarnings=FALSE)
	#Create any image files that do not yet exist
	for (t in start:stop) {
		if (!(file.exists(paste(path, t, ".png", sep="")))) {
			plot2png(paste(method, "(", t, ")", sep=""), paste(path, t, ".png", sep=""))
		}
	}
}
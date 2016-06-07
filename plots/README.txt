This is the directory for plot generation plugins.
Each R file in this directory must contain the function "fnames()".
fnames() must return a list of function names mapped to their proper names.
The first element in the list returned by fnames() must be the name of the file
itself named with the plugin's proper name.
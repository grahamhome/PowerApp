This is the directory for plot generation functions.
Each R file in this directory must contain a function that
shares its name with the file itself. This function must return
a plot object.

linker.txt contains import and display links for plot generation methods.
Links specify which import method and which display method to use
with a given plot generation method.
Links must be written one per line in the following format:
plot_file_name:input_file_name_1,input_file_name_2:display_file_name
There must be a blank line at the end of the file (hit Enter after entering last link).

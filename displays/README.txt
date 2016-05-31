This is the directory for display modules.
Display modules provide a user interface for a 
specified graphing function running on a
specified data set.
All display modules must have the function "name()" which returns 
the display module's proper name.
All display modules must also have the function "use_plots" which 
returns a list of filenames of plotting plugins which the display 
module is compatible with.
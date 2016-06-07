This is the directory for data import plugins.
An import plugin translates one or more raw data files 
from 'rawdata/' into a collection of R environment variables 
which are expected by a plotting plugin.
All import plugins must include the function "name()", which returns 
the import plugin's proper name.
All import plugins must also include the function "use_plots()", which returns a 
list of filenames of plotting modules which are compatible with the import plugin.
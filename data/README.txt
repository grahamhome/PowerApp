This is the directory for import scripts.
An import script translates one or more raw data files 
from 'rawdata/' into a collection of R environment variables 
which are expected by a plot-generation function.
All import scripts must include the function "name()", which returns 
the import script's proper name.
All import scripts must also include the function "use_plots()", which returns a 
list of filenames of plotting modules which are compatible with the import script.
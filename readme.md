Power Apps are individual Shiny apps for viewing different types of graphs.

Ultimately there will be a master app that incorporates features from multiple apps.

To run an app, simply follow these steps:

1. Install shiny with install.packages("shiny")

2. Ensure that all data files needed are in the data/ directory of the app you want to use

3. From the app's directory, run app.r (I use "Rscript app.r")

4. Go to 127.0.0.1:5678 in your browser to use the app.
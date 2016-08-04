This is the top-level directory of the Power Viewer application.
It contains the main app (app.r), which is used to run the application.
All sub-directories have their own readme files which explain 
their purpose.

To run the application, use the command "Rscript app.r" from the command line.
The application can then be viewed by opening Google Chrome and pointing it to
the address 127.0.0.1:5678. Other browsers may also work but this is not guaranteed.

Note that using the back button or the refresh button in Chrome will likely
cause the application to become unresponsive and it will have to be restarted
from the command line. Ctrl+c can be used to kill the process before re-running
the Rscript command. This is a limitation of Shiny and the back button included 
in the app itself should be used for navigation instead of the browser button.

Any questions about this application which cannot be answered through examination
of the code may be sent to Graham Home <grahamhome333@gmail.com> and will typically
be answered within one weekday. 
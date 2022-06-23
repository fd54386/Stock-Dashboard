# Stock-Dashboard
Quick Stock Dashboard for R to facilitate Custom Indicators and Screeners using R functions.
This project is an all-in-one single file shiny app that was the starting point for the SQL repository.
This design is ultimately ineffective for live logging of quotes due to the single-threaded nature of R Shiny.  
Calculations on 500 tickers takes ~ 5seconds per refresh, locking out the UI during that time.

This single .R file architecture could be implemented with lower overhead than the SQL design when sourcing historical tick data from .csv files or APIs, however I do not intend to pursue it further at this time.

To use -- load app.R into your favorite IDE. 
1. Point the filepath at line 190 towards a file on your local PC (project pathing had some issues with shiny, in practice).  Note that a sample csv with high volume tickers is included in the repository.
2. Run App and let the charting begin.  
3. Charting is only active while app is open, and no logs are currently implemented.  functions to step through .csv files are in the file but need quick massaging to use.

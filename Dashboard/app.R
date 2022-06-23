#Developed by Fred Davey
#Custom Stock Data Browser
#This project is intended to be used for custom indicator definitions and exploration of stocks utilizing statistical methods not available in common charting software.
#Note-- there is no warranty provided with this software.  Any user who actively trades with this software is encouraged to do their own validations.

#This file is for an all-in-one R solution that both pulls data and charts it on the same thread.  it is slow without asynchronous design.
#This single-threaded structure is better suited for run once calculations such as end of day reviews of tiingo / API data
#A much cleaner version that utilizes a 2nd R instance and a SQL database can be found in the Stocks_Live_Server_SQL-UI repository.

#This needs manual massaging to run.
#1. StockTickers global variable needs to point to a local filepath (shiny was not finding the project directory on its own)

#2. If hooking up to a local static  dataset instead of a live one, fnPopulateParentTempDataFrame and the tempData definition below it can be uncommented.

# For a local dataset, two more calls in the server function need to be changed.  Historical Comment #1/2 and #2/2  -- defining our initial working dataset and our ongoing reactive data updates



#Imports
library(shiny)
library(plotly)
library(tidyverse)
library(tidyquant)
library(quantreg)
library(broom)

#Locally Defined Functions
#####

fnPullQuoteData_singleQuote<- function(aTickerList,aMasterTable, ...){
  

  #Free way to get sub-minute stock data off Yahoo Finance.  Intended to be used by regularly pulling quotes on a timer.  Alternative free but rate limited 1 min interval options are available off Tiingo API (paid is $10). Yahoo solution is better able to view the whole market (and let's be honest tick 'close' values are equivalent to regular quotes)
  #Inputs -- list of interesting symbols, Table with all previous data so we can calculate indicators dependent on lagged values
  #Actions -- Function will query Yahoo Finance API for Last trade & daily aggregate stats. Note -- Takes 2-3 seconds for querying 500 tickers.
  #Outputs a tibble with: Symbol, trade time, lasttrade, $change, %change, Daily OHL, Vol, indicators
  
    #We will build query table throughout the subloop.  This frontruns Yahoo's maximum query size
    fQueryTibble = NULL
    p1 <- Sys.time()
    #print(paste0('PreYahooTime is: ',p1))
    for (j in 0:(ceiling(length(aTickerList) / 200) - 1))
    {
      fTickerShort <- aTickerList[(j*200+1):min(length(aTickerList),((1+j)*200)) ]
      recentQuote<- getQuote(fTickerShort)
      recentQuote<- rownames_to_column(recentQuote, var = 'Symbol')
      fQueryTibble <- bind_rows(fQueryTibble, recentQuote)
    }
    
    fQueryTibble<- fQueryTibble %>% mutate(appendTime = Sys.time())
    
    #p2 <- Sys.time()
    #print(paste0('YahooTime is: ',difftime(p2, p1, units = 'secs')))
    
    fQueryTibble<- fnAddIntradayIndicatorCols(fQueryTibble, aMasterTable, aPeriod_sec = 10 )
    
    #p3 <- Sys.time()
    #print(paste0('PostIndicatorTime is: ', difftime(p3, p2, units = 'secs')))

  return(fQueryTibble)
}

fnStepThroughHistorical <- function(aDataFrame, aIndex){
  #Psuedo live querying -- read .csv files row by row to simulate active trade day on weekends.  Could also be used to review trades / practice.
  #Input -- Dataframe imported as a global variable from previous yahoo queries -- need an extra `Index` field based on query timestamp.  2nd argument is an index to hook into the extra field.
  #Actions -- Takes Modulus of index vs maximum file index.  Takes file at filepath and subsets out the requested index
  #Output -- One Set of quote data
  fMaxIndex = max(aDataFrame$Index)
  
  return(aDataFrame[aDataFrame$Index == aIndex%%fMaxIndex,])
  
} 

fnAddIntradayIndicatorCols <- function(aRecentDatapoints, aMasterDataFrame, aPeriodCount = 12, aPeriod_sec){
  #1.A
  #For ongoing data collection, Add indicators in a reasonable way, without calculating across all collected periods, just most recent
  #Input -- Most recent datapull entries.  Master Dataframe with all trade data.  We will filter against the $appendTime using the periodCount specified for indicators
  #Actions -- Subset Master, send recent datapull + old subset into indicator function
  #Output -- Returns most recent single pull with requested indicator columns appended on.
  #Note -- extra indicators currently need static function calls within the bind_cols function.  Not a dynamic solution.
  
  #We are building the output tib row by row to avoid excessive joining logic & column management
  fOutputTib = NULL
  
  
  #Subset Out Most Recent number of points based on indicator requested aPeriodCount and datacollection aPeriod_sec
  if(!is.null(aMasterDataFrame)){
  #print('trying to arrange old rows')
    
  fOldRows <- aMasterDataFrame[aMasterDataFrame$appendTime > (Sys.time()- ((aPeriodCount+.9) * aPeriod_sec)), ]
  fOldRows <- fOldRows %>% arrange(Symbol, appendTime)
  
  }
  
  #Loop through the calculations for all tickers
  for (i in 1:nrow(aRecentDatapoints)){
    #Generate tibble with only the points we need on this iteration
    if(!is.null(aMasterDataFrame)){
    fCalcTib = bind_rows(aRecentDatapoints[i,], fOldRows[fOldRows$Symbol == aRecentDatapoints$Symbol[i],])
    }
    else{
      fCalcTib = aRecentDatapoints[i,]
    }
    #Bind existing row data with calculated data.
    #fIndicatorRowResult would be a good variable to insert into a SQL database.
    fIndicatorRowResult <- bind_cols(aRecentDatapoints[i, ], fnLiveIndicators(fCalcTib))
    
    #This is our in-app data solution
    fOutputTib <- bind_rows(fOutputTib, fIndicatorRowResult)
  }
  return(fOutputTib)
}

fnAddIntradayIndicatorCols_Index <- function(aRecentDatapoints, aMasterDataFrame, aPeriodCount = 12, aCurrentIndex){
  #Similar to the non-index function above, however we will key off of index instead of time for our lookbacks.
  
  #We are building the output tib row by row to avoid excessive joining logic & column management
  fOutputTib = NULL
  
  #Subset Out Most Recent number of points based on indicator requested aPeriodCount and datacollection aPeriod_sec
  fOldRows <- aMasterDataFrame[aMasterDataFrame$Index > aCurrentIndex - aPeriodCount, ]
  fOldRows <- fOldRows %>% arrange(Symbol, `Trade Time`)
  
  #Loop through the calculations for all tickers
  for (i in 1:nrow(aRecentDatapoints)){
    #Generate tibble with only the points we need on this iteration
    fCalcTib = bind_rows(aRecentDatapoints[i,], fOldRows[fOldRows$Symbol == aRecentDatapoints$Symbol[i],])
    
    #Bind existing row data with calculated data.
    #fIndicatorRowResult would be a good variable to insert into a SQL database.
   # if(fCalcTib$Symbol[1] == 'AMD'){print(fnLiveIndicators(fCalcTib))}
    
    fIndicatorRowResult <- bind_cols(aRecentDatapoints[i,], fnLiveIndicators(fCalcTib))
    
    #This is our in-app data solution
    fOutputTib <- bind_rows(fOutputTib, fIndicatorRowResult)
  }
  return(fOutputTib)
}

fnLiveIndicators<- function(aDataFrame){
  #1/2 B  Calculate the indicators for our intraday logging
  #Input -- Dataframe for the most recent datapoint and intended period to calculate over
  #Actions -- Run the math
  #Output -- Single row tibble with only the indicator output columns
  #Note -- currently only adding one set of indicator variables.  Would potentially modify fn 1A to run a list of indicator functions in a loop, each would have a different 1.B equivalent
  
  #Note -- 12 points seems to result in stable regression solutions.
  if(nrow(aDataFrame) <12){
    return(tibble(Slope = NA, SloLoCI = NA, SloUpCI = NA))
  }
  
  #For a demo, we are going to run a robust regresion (IE -- has less weight on outliers than LeastSquares Regression) and report all parameters and CIs
  #rq function from the quantreg package -- LAD regression
  #times %%86400 -> times go from absolute dates to hours since midnight.  This helps with precision & helps the function solve.  With full date, the regression was computationally singular.
  
  #Note, we also have issues when there isn't a trade executed over the timeframe specified
  #we'll run against append time for now (flat is useful!), may go for tryCatch if more shenanigans pop up.

  #Non-unique solution warnings -- we're running enough fits that noisy solutions are ok.
  suppressWarnings(fTrendFit<- rq(formula = Last~(as.numeric(appendTime)%% 10000), data = aDataFrame))
  fParams <- tidy(fTrendFit)

  return(tibble(Slope = as.numeric(fParams[2,2]), SloLoCI = as.numeric(fParams[2,3]), SloUpCI = as.numeric(fParams[2,4])))

}

fnAddIndicatorColumns_FinishedSet <- function(aIndicatorFunction, aDataFrame, aNRowsFit = 12, ...){
  #Unused -- Potentially steps through entire finished dataset, but requires different dataset sorting logic -- would fit concurrent data from multiple symbols as-is.
  #For finished datasets, apply indicator functions. Has more flexibility than a mutate() & lag() design. Also more efficient than recalculating a master table
  #Inputs -- The name of the function that generates your indicator (can we change this into a list long term?), and the tibble used for calculations.  Also accepts pass through variables for the indicator function.
  #Actions -- Creates a subset of appropriate period length and sends it through the indicator function
  #Output -- The input tibble with new columns appended on
  fIndicatorCols = NULL
  
  for(i in aNRowsFit:nrow(aDataFrame)){
    fIndicatorCols<- bind_rows(fIndicatorCols, aIndicatorFunction(aDataFrame[(i-aNRowsFit):i,], ...))
  }
  
  return(fIndicatorCols)
}

#####
#Global Variables
#1. 
#Tickers that have passed trading criteria for size (limits pump & dump) & volume (expecting better liquidity)
#Recommend paring down ticker list from here:
#https://stackoverflow.com/questions/5246843/how-to-get-a-complete-list-of-ticker-symbols-from-yahoo-finance
#http://www.nasdaq.com/screening/companies-by-name.aspx?letter=0&exchange=all&render=download 
StockTickers<- read_csv("C:/Users/Fred/Documents/GitHub/Stock Dashboard/Stock-Dashboard/HighMKTCapHighVolTickers_20220610.csv")$value

##2. Local Dataset with SPY and one other ticker for evaluation
# fnPopulateParentTempDataFrame<- function(){
# fTempData <- read_csv("C:/Users/Fred/Documents/R/Sample Data/HighValue_4secQuotes_1hr.csv")
# fTempDF = numeric()
# for(i in 1:(nrow(fTempData) / 496)){
#   
#   fTempDF = c(fTempDF, replicate(496, i))
# }
# fTempData <- bind_cols(fTempData, as_tibble(fTempDF))
# fTempData <- fTempData%>% rename(Index = value)
# 
# return(fTempData)
# }
# tempData <- fnPopulateParentTempDataFrame()


#Shiny App Functions -- UI Layout and Server Logic
# Define UI for application for convenient real time stock feedback
#####
ui <- fluidPage(
  
  # Application title
  titlePanel("Custom Chart Indicators"),
  
  # Sidebar 
  sidebarLayout(
    sidebarPanel(
      #Text Input Box to Pull a Ticker
      textInput('TickerSearch', label = 'Specify Ticker', value = "AMD")
      
      #List of everything pulled currently
      #dataTableOutput("IndicatorList"),
      
      #Filters toggles for RS / RW / Volume
      
    ),
    
    # Plots
    mainPanel(
      plotOutput("Chart"),
      # Not available with historical setup at the moment (requires a lag reference)
      #plotOutput("VolumeProfile"), 
      plotOutput("IndicatorVsTime")

    )
  )
)

# Define server logic required to generate plots
#Server
#####
server <- function(input, output) {
  #Initialize our datasets
  #Note -- can't just define our convolutedWOrk variable directly due to the complexity of expressions allowed in the reactiveValues fn (looks for comma after first expression)
  WorkingDataset = NULL
  #Historical Comment #1/2
  #WorkingDataset = bind_rows(WorkingDataset, fnStepThroughHistorical(tempData, 1))
  
  #Live Yahoo
  WorkingDataset<- fnPullQuoteData_singleQuote(StockTickers,WorkingDataset)

  #Need to put dataset into a reactive container so that the changes in observe calls stick.
  #Update -- not completely sure this is true now that I've worked with reactive calls more, but it works and I'm moving on to other things.  May be some room for cleaner code here.
  
  reactiveData <- reactiveValues( convolutedWork = WorkingDataset,  i = 2 )
  #Any code chunks that call auto invalidate will refresh after the specified duration (milliseconds)
  autoInvalidate <- reactiveTimer(10000)

  observe({
    autoInvalidate()
    #Historical Comment #2 / 2
    # isolate({reactiveData$convolutedWork = bind_rows(reactiveData$convolutedWork, fnAddIntradayIndicatorCols_Index(
    #                                                         aRecentDatapoints = fnStepThroughHistorical(tempData, reactiveData$i), 
    #                                                         aMasterDataFrame = reactiveData$convolutedWork,
    #                                                         aPeriodCount = 12, aCurrentIndex = reactiveData$i)
    #                                                  )})
    
    #This is the live updating of data.  If using the historical query above, comment this out
     isolate({reactiveData$convolutedWork = bind_rows(reactiveData$convolutedWork, fnPullQuoteData_singleQuote(
                                                             aTickerList = StockTickers, 
                                                             aMasterTable = reactiveData$convolutedWork))
     })
    
    isolate({reactiveData$i = reactiveData$i+1})
 
  })
  
  #Ticker performance vs time
  output$Chart <- renderPlot({
    autoInvalidate()
    
    isolate({
      suppressWarnings(
      ggplot(data = reactiveData$convolutedWork[reactiveData$convolutedWork$Symbol == input$TickerSearch,], aes(x = `Trade Time`, y= Last)) + 
      geom_line() + geom_point() + labs(title = input$TickerSearch) 
      )
    })
  })

    #Plot the indicator with confidence intervals shaded
  output$IndicatorVsTime <- renderPlot({
    autoInvalidate()
    isolate({
      suppressWarnings(
        ggplot(data = reactiveData$convolutedWork[reactiveData$convolutedWork$Symbol == input$TickerSearch,], aes(x = `Trade Time`, y= Slope)) + 
        geom_line(col = 'red') + geom_point() + geom_ribbon(aes(ymin = SloLoCI, ymax = SloUpCI), alpha = .1) + labs(title = 'Robust Regression Slope vs Time') + 
        geom_hline(aes(yintercept = 0))
        )
    })
    
  })
  }

# Run the application 
shinyApp(ui = ui, server = server)

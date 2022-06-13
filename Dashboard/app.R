#Developed by Fred Davey
#Custom Stock Data Browser
#Intended to be used for custom indicator definitions and exploration of stocks utilizing statistical methods not available in common charting software.
#Note-- there is no warranty provided with this software.  Any user who actively trades with this software is encouraged to do their own validations.

#This software is still heavily under development.  Needs lots of massaging to get the appropriate data hooked up.  Also likely has major memory leaks.
#There is a significant amount of computation going on.  This will likely need to be converted into 2 instances + SQL server to make UI usable

#Imports
library(shiny)
library(plotly)
library(tidyverse)
library(tidyquant)
library(quantreg)
library(broom)

#Locally Defined Functions
#####
#1. 
#Convoluted free way to get sub-minute stock data off Yahoo Finance.  Some free but rate limited 1 min interval options are available off Tiingo API (paid is $10). Yahoo solution is better able to view the whole market (and let's be honest tick closes are just as arbitrary)
#Inputs -- list of interesting symbols, number of hours to loop the query, polling period in seconds
#Actions -- Function will then query Yahoo Finance API for Last trade & daily aggregate stats for duration specified, 1 query every default 5 secs. Note -- locks out R session, need to update for live feedback.  Also note -- Takes 2-3 seconds for querying 500 tickers, possibly more as tibble size grows
#Outputs a tibble with: Symbol, trade time, lasttrade, $change, %change, Daily OHL, Vol,Corresponding SPY, RelChange vs last query
fnPullQuoteDataHours_SplitYahoo<- function(aTickerList, aHours, aPeriod_sec = 5, ...){
  fPoints = (60/aPeriod_sec)*60*aHours
  
  #Do I need to predefine my tibble length?  R is notoriously slow with dynamic resizing
  #Seemingly favored tidyverse method:
  #bind_rows(tibbleList)
  #rough split says ~ 100ms / 1000 loop iterations.  We have ~3 secs of idle as-is, seems acceptable for ~4000 iterations / day
  
  #DayTibble is our parent table that is getting populated across the 6.5 hour trading day
  DayTibble <-  NULL
  pointCollectionPeriod = aPeriod_sec
  
  for(i in 1:fPoints){
    #We will build query table throughout the subloop.  This frontruns Yahoo's maximum query size
    fQueryTibble = NULL
    #Also need to save the last datapull so that we don't need to mess with sorting for relative change calculations
    fPrevQuote = NULL
    print(paste0(as.character(i), 'of ', as.character(fPoints), ' attempts'))
    p1 <- Sys.time()
    print(paste0('PreYahooTime is: ',p1))
    for (j in 0:(ceiling(length(aTickerList) / 200) - 1))
    {
      fTickerShort <- aTickerList[(j*200+1):((1+j)*200) ]
      recentQuote<- getQuote(fTickerShort)
      recentQuote<- rownames_to_column(recentQuote, var = 'Symbol')

      fQueryTibble <- bind_rows(fQueryTibble, recentQuote)
    }
    
    if(!is.null(fPrevQuote)){
      fQueryTibble <- inner_join(fQueryTibble, fPrevQuote, by = Symbol)%>% 
        mutate(intrVol = Volume - LastVolume )%>% Select(Symbol, `Trade Time`, Last, Change, `% Change`, Open, High, Low, Volume, SPY, relSpy, relTick, intrVol)
    }
    
    fPrevQuote <- fQueryTibble %>% select(Symbol, Volume) %>% rename(LastVolume = Volume)
    
    fQueryTibble %>% mutate(appendTime = Sys.time())
    p2 <- Sys.time()
    
    print(paste0('YahooTime is: ',difftime(p2, p1, units = 'secs')))
    
    fQueryTibble<- fnAddIntradayIndicatorCols(fQueryTibble, DayTibble, aPeriod_sec = pointCollectionPeriod )
    
    p3 <- Sys.time()
    print(paste0('PostIndicatorTime is: ', difftime(p3, p2, units = 'secs')))
    
    DayTibble<- bind_rows(DayTibble, fQueryTibble)
    
    
    p4 <-Sys.time()
    print(paste0('BindTime is: ', difftime(p4, p3, units = 'secs')))
    
    theDelay <- pointCollectionPeriod - as.numeric(difftime(Sys.time(),p1,unit="secs"))
    Sys.sleep(max(0, theDelay))
  }
  
  return(DayTibble)
}
#1 for Shiny
#Pull Yahoo Quotes, 
fnPullQuoteData_singleQuote<- function(aTickerList,aMasterTable, ...){
    #We will build query table throughout the subloop.  This frontruns Yahoo's maximum query size
    fQueryTibble = NULL
    p1 <- Sys.time()
    print(paste0('PreYahooTime is: ',p1))
    for (j in 0:(ceiling(length(aTickerList) / 200) - 1))
    {
      fTickerShort <- aTickerList[(j*200+1):min(length(aTickerList),((1+j)*200)) ]
      recentQuote<- getQuote(fTickerShort)
      recentQuote<- rownames_to_column(recentQuote, var = 'Symbol')
      fQueryTibble <- bind_rows(fQueryTibble, recentQuote)
      #write_csv(fQueryTibble, paste0('C:/Users/Fred/Documents/GitHub/Stock Dashboard/Stock-Dashboard/phantomrow',j,'.csv'))
    }
    
    fQueryTibble<- fQueryTibble %>% mutate(appendTime = Sys.time())
    p2 <- Sys.time()
    
    print(paste0('YahooTime is: ',difftime(p2, p1, units = 'secs')))
    #print(fQueryTibble)
    #print(aMasterTable)
    fQueryTibble<- fnAddIntradayIndicatorCols(fQueryTibble, aMasterTable, aPeriod_sec = 10 )
    
    p3 <- Sys.time()
    print(paste0('PostIndicatorTime is: ', difftime(p3, p2, units = 'secs')))
    #write_csv(fQueryTibble, 'C:/Users/Fred/Documents/GitHub/Stock Dashboard/Stock-Dashboard/phantomrow.csv')
    #print(fQueryTibble$Symbol[497,])
    
  return(fQueryTibble)
}



#2. Psuedo live querying -- read .csv files row by row to simulate active trade day on weekends.  Could also be used to review trades / practice.
#Input -- Dataframe imported as a global variable from previous yahoo queries -- need an extra `Index` field based on query timestamp.  2nd argument is an index to hook into the extra field.
#Actions -- Takes Modulus of index vs maximum file index.  Takes file at filepath and subsets out the requested index
#Output -- One Set of Quotes
fnStepThroughHistorical <- function(aDataFrame, aIndex){
  fMaxIndex = max(aDataFrame$Index)
  
  return(aDataFrame[aDataFrame$Index == aIndex%%fMaxIndex,])
  
} 

#1.A
#For ongoing data collection, Add indicators in a reasonable way, without calculating across all collected periods, just most recent
#Input -- Most recent datapull entries.  Master Dataframe with all trade data.  We will filter against the $appendTime using the periodCount specified for indicators
#Actions -- Subset Master, send recent datapull + old subset into indicator function
#Output -- Returns most recent datapoints with requested indicators appended on.
#Note -- this version is insufficient for good indicator scalability.  Works with one hardcoded indicator function.  Will ideally want a 2nd nested loop through a list of functions

fnAddIntradayIndicatorCols <- function(aRecentDatapoints, aMasterDataFrame, aPeriodCount = 12, aPeriod_sec){
  #We are building the output tib row by row to avoid excessive joining logic & column management
  fOutputTib = NULL
  
  
  #Subset Out Most Recent number of points based on indicator requested aPeriodCount and datacollection aPeriod_sec
  if(!is.null(aMasterDataFrame)){
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

#2.A 
#Similar to 1.A however we will key off of index instead of time for our lookbacks.
fnAddIntradayIndicatorCols_Index <- function(aRecentDatapoints, aMasterDataFrame, aPeriodCount = 12, aCurrentIndex){
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

#1/2 B  Calculate the indicators for our intraday logging
#Input -- Dataframe for the most recent datapoint and intended period to calculate over
#Actions -- Run the math
#Output -- Single row tibble with only the indicator output columns
#Note -- currently only adding one set of indicator variables.  Would potentially modify fn 1A to run a list of indicator functions in a loop, each would have a different 1.B equivalent
fnLiveIndicators<- function(aDataFrame){
  if(nrow(aDataFrame) <12){
    return(tibble(Slope = NA, SloLoCI = NA, SloUpCI = NA))
  }
  
  #For a demo, we are going to run a robust regresion (IE -- has less weight on outliers than LeastSquares Regression) and report all parameters and CIs
  #rq function from the quantreg package -- LAD regression
  #times %%86400 -> times go from absolute dates to hours since midnight.  This helps with precision & helps the function solve.  With full date, the regression was computationally singular.
  
  #Note, we also have issues when there isn't a trade executed over the timeframe specified
  #we'll run against query time for now (flat is useful!), may go for tryCatch if more shenanigans pop up.

  #Non-unique solution warnings -- we're running enough fits that noisy solutions are ok.
  suppressWarnings(fTrendFit<- rq(formula = Last~(as.numeric(appendTime)%% 10000), data = aDataFrame))
  fParams <- tidy(fTrendFit)

  return(tibble(Slope = as.numeric(fParams[2,2]), SloLoCI = as.numeric(fParams[2,3]), SloUpCI = as.numeric(fParams[2,4])))
  
  # tryCatch(
  #   expr = {
  #     fTrendFit<- rq(formula = Last~(as.numeric(queryTime)%% 5000), data = aDataFrame)
  #     fParams <- tidy(fTrendFit)
  #     return(tibble(Slope = as.numeric(fParams[2,1]), SloLoCI = as.numeric(fParams[2,2]), SloUpCI = as.numeric(fParams[2,3])))
  #   },
  #   error = function(e){
  #     message('LAD indicator Error')
  #     print(e)
  #     return(tibble(Slope = NA, SloLoCI = NA, SloUpCI = NA))
  #   },
  #   warning = function(w){
  #     message('LAD indicator warning')
  #     print(w)
  #     return(tibble(Slope = NA, SloLoCI = NA, SloUpCI = NA))
  #     
  #   }
  #   
  # )
  # 
}

#Unused -- Potentially steps through entire finished dataset, but requires different dataset sorting logic -- would fit concurrent data from multiple symbols as-is.
#For finished datasets, apply indicator functions. Has more flexibility than a mutate() & lag() design. Also more efficient than recalculating a master table
#Inputs -- The name of the function that generates your indicator (can we change this into a list long term?), and the tibble used for calculations.  Also accepts pass through variables for the indicator function.
#Actions -- Creates a subset of appropriate period length and sends it through the indicator function
#Output -- The input tibble with new columns appended on
fnAddIndicatorColumns_FinishedSet <- function(aIndicatorFunction, aDataFrame, aNRowsFit = 12, ...){
  fIndicatorCols = NULL
  
  for(i in aNRowsFit:nrow(aDataFrame)){
    fIndicatorCols<- bind_rows(fIndicatorCols, aIndicatorFunction(aDataFrame[(i-aNRowsFit):i,], ...))
  }
  
  return(fIndicatorCols)
}

#Unused -- works with finished set indicator columns.  Calculate LAD regression parameters for trends vs time.  Include Confidence Intervals.  Note -- no predictive power observed from preliminary testing, even when comparing against CI bounds
fnCalcLocalSlope <- function(aDataFrame){
  fTrendFit<- rq(formula = Last~(as.numeric(`Trade Time`)%% 86400), data = aDataFrame)
  fParams <- tidy(fTrendFit)
  return(tibble(Slope = as.numeric(fParams[2,1]), SloLoCI = as.numeric(fParams[2,2]), SloUpCI = as.numeric(fParams[2,3])))
}
#Unused -- Manual calculation of SLR parameters to help with smaller datasets
#LSE and error estimates from linear models with R, 2E by Faraway
fnSlowCalcSlope <- function(aDataFrame, aAlpha = .99) {
  #Need sizing to populate the matrices
  fPoints <- nrow(aDataFrame)
  
  #define our x and y matrices
  #Note we aren't using the timestamps as precision interferes with the ability of software to solve.
  matX <- as.matrix(cbind(replicate(fPoints,1), 1:fPoints))
  y <- as.matrix(aDataFrame$close)
  
  #define intermediate matrices and terms needed to solve for Betas with linear algebra.
  XtX <- t(matX)%*%matX
  invXtX <- solve(XtX)
  hatMatrix<- matX%*%invXtX%*%t(matX)
  RSS <- t(y)%*%(diag(fPoints)-hatMatrix)%*%y
  
  #Useful terms.  Beta for slope and intercept.  
  bMat <- invXtX%*%t(matX)%*%y
  #note, p = 2 since we have slope and intercept
  seSlope <- sqrt(invXtX[2,2])*sqrt(RSS/(fPoints-2))
  
  return(tibble(estimate = bMat[2,1], std.error = seSlope, lowerCI = bMat[2,1] + seSlope *qnorm((1-aAlpha)/2), upperCI = estimate - std.error *qnorm((1-aAlpha)/2)))
}

#####
#Global Variables
#1. 
#Tickers that have passed trading criteria for size (limits pump & dump) & volume (expecting better liquidity)
#Recommend paring down ticker list from here:
#https://stackoverflow.com/questions/5246843/how-to-get-a-complete-list-of-ticker-symbols-from-yahoo-finance
#http://www.nasdaq.com/screening/companies-by-name.aspx?letter=0&exchange=all&render=download 
StockTickers<- read_csv("C:/Users/Fred/Documents/GitHub/Stock Dashboard/Stock-Dashboard/HighMKTCapHighVolTickers_20220610.csv")$value

#2. Local Dataset with SPY and one other ticker for evaluation
#Hide this in a function that's easy to comment out when unnecessary.
#Long term, should probably have live vs dataset feeds controllable from UI
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
  titlePanel("Stock Scanner and Custom Chart Indicators"),
  
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
  #Historical csv comment 1/2
  #WorkingDataset = bind_rows(WorkingDataset, fnStepThroughHistorical(tempData, 1))
  #Live Yahoo
  WorkingDataset<- fnPullQuoteData_singleQuote(StockTickers,WorkingDataset)
  #options(warn = -1)
  #options(warn = 0)
  #Need to put dataset into a reactive container so that the changes in observe calls stick.
  reactiveData <- reactiveValues( convolutedWork = WorkingDataset,  i = 2 )
  #Any code chunks that call auto invalidate will refresh after the specified duration (milliseconds)
  autoInvalidate <- reactiveTimer(10000)
  #wrap this code to be a reactive expression)
  observe({
    autoInvalidate()
    #Historical Comment #2 / 2
    # isolate({reactiveData$convolutedWork = bind_rows(reactiveData$convolutedWork, fnAddIntradayIndicatorCols_Index(
    #                                                         aRecentDatapoints = fnStepThroughHistorical(tempData, reactiveData$i), 
    #                                                         aMasterDataFrame = reactiveData$convolutedWork,
    #                                                         aPeriodCount = 12, aCurrentIndex = reactiveData$i)
    #                                                  )})
    
     isolate({reactiveData$convolutedWork = bind_rows(reactiveData$convolutedWork, fnPullQuoteData_singleQuote(
                                                             aTickerList = StockTickers, 
                                                             aMasterTable = reactiveData$convolutedWork)
                                                      )
     })
    
    isolate({reactiveData$i = reactiveData$i+1})
   
    #   isolate({
    #    print(reactiveData$convolutedWork[reactiveData$convolutedWork$Index > reactiveData$i - 12 &
    #                                              reactiveData$convolutedWork$Symbol == input$TickerSearch,]) 
    #   
    # })
  })
  
  output$Chart <- renderPlot({
    autoInvalidate()
    
    isolate({
      suppressWarnings(
      ggplot(data = reactiveData$convolutedWork[reactiveData$convolutedWork$Symbol == input$TickerSearch,], aes(x = `Trade Time`, y= Last)) + 
      geom_line() + geom_point() + labs(title = input$TickerSearch) 
      )
    })
  })

   
  # output$VolumeProfile <- renderPlot({
  #    autoInvalidate()
  #    
  #    isolate({
  #    ggplot(data = reactiveData$convolutedWork, aes(x = `Trade Time`, y= Last))+
  #        geom_violin(aes(weight = intervalVolume, color = Symbol, fill = Symbol,  alpha = 255)) 
  #    })
  #  })
  
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
  
  #Probably want a different timer for this so that we can actually explore
  #Also probably don't want to set a default arrangement within this invalidate call (loses all progress)
  # output$IndicatorList <- renderDataTable({
  #   autoInvalidate()
  #   isolate({
  #   arrange(reactiveData$convolutedWork[reactiveData$convolutedWork$Index == reactiveData$i-1,c('Symbol', 'Slope')], Slope, Symbol)
  #   })
  # })

  #onStop(write_csv(isolate({reactiveData$convolutedWork}), paste0('sessionDataset_', date(Sys.time()), '.csv')))
  }

# Run the application 
shinyApp(ui = ui, server = server)

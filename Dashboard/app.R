#Developed by Fred Davey
#Custom Stock Data Browser
#Intended to be used for custom indicator definitions and exploration of stocks utilizing statistical methods not available in common charting software.

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
    for (j in 0:(ceiling(aTickerList / 200) - 1))
    {
      fTickerShort <- aTickerList[(j*200+1):((1+j)*200) ]
      recentQuote<- getQuote(fTickerShort)
      recentQuote<- rownames_to_column(recentQuote, var = 'Symbol')
      
      recentQuote <- cbind(recentQuote, spyQuote)
      
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
  fOldRows <- aMasterDataFrame[aMasterDataFrame$appendTime > (Sys.time()- ((aPeriodCount+.9) * aPeriod_sec)), ]
  fOldRows <- fOldRows %>% arrange(Symbol, `TradeTime`)
  
  #Loop through the calculations for all tickers
  for (i in 1:nrow(aRecentDatapoints)){
    #Generate tibble with only the points we need on this iteration
    fCalcTib = bind_rows(aRecentDatapoints[i,], fOldRows[fOldRows$Symbol == aRecentDatapoints$Symbol[i],])
    
    #Bind existing row data with calculated data.
    #fIndicatorRowResult would be a good variable to insert into a SQL database.
    fIndicatorRowResult <- bind_col(aRecentDatapoints[i], fnLiveIndicators(fCalcTib))
    
    #This is our in-app data solution
    fOutputTib <- bind_rows(fOutputTib, fIndicatorRowResult)
  }
  return(fOutputTib)
}
#1.B  Calculate the indicators for our intraday logging
#Input -- Dataframe for the most recent datapoint and intended period to calculate over
#Actions -- Run the math
#Output -- Single row tibble with only the indicator output columns
#Note -- currently only adding one set of indicator variables.  Would potentially modify fn 1A to run a list of indicator functions in a loop, each would have a different 1.B equivalent
fnLiveIndicators<- function(aDataFrame){
  #For a demo, we are going to run a robust regresion (IE -- has less weight on outliers than LeastSquares Regression) and report all parameters and CIs
  #rq function from the quantreg package -- LAD regression
  #times %%86400 -> times go from absolute dates to hours since midnight.  This helps with precision & helps the function solve.  With full date, the regression was computationally singular.
  fTrendFit<- rq(formula = Last~(as.numeric(`Trade Time`)%% 86400), data = aDataFrame)
  fParams <- tidy(fTrendFit)
  return(tibble(Slope = as.numeric(fParams[2,1]), SloLoCI = as.numeric(fParams[2,2]), SloUpCI = as.numeric(fParams[2,3])))
}

#1.C Psuedo live querying -- read .csv files row by row to simulate active trade day on weekends.  Could also be used to review trades / practice.
fnStepThroughHistorical <- function(){
  
} 

#2. For finished datasets, apply indicator functions. Has more flexibility than a mutate() & lag() design. Also more efficient than recalculating a master table
#Inputs -- The name of the function that generates your indicator (can we change this into a list long term?), and the tibble used for calculations.  Also accepts pass through variables for the indicator function.
#Actions -- Creates a subset of appropriate period length and sends it through the indicator function
#Output -- The input tibble with new columns appended on
fnAddIndicatorColumns_FinishedSet <- function(aIndicatorFunction, aDataFrame, ...){
  fIndicatorCols = NULL
  
  for(i in aNRowsFit:nrow(aDataFrame)){
    fIndicatorCols<- row_bind(fIndicatorCols, aIndicatorFunction(aDataFrame[(i-aNRowsFit):i,], ...))
  }
  
  return(fIndicatorCols)
}

#3 Calculate Linear regression parameters for trends vs time.  Include Confidence Intervals.  Note -- no predictive power observed from preliminary testing, even when comparing against CI bounds
fnCalcLocalSlope <- function(aDataFrame, aAlpha = .99){
  fTrendFit = lm(close ~ date, data = aDataFrame)
  fParams = tidy(fTrendFit)[2,]
  fParams = fParams %>% mutate(lowerCI = estimate + std.error *qnorm((1-aAlpha)/2), upperCI = estimate - std.error *qnorm((1-aAlpha)/2) )
  return(fParams%>% select(estimate, std.error, lowerCI, upperCI))
}
#4 Manual calculation of SLR parameters to help with smaller datasets
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

#5 Calculate regression parameters vs SPY

#6 Perform a mean offset on SPY data for single-axis plotting against Ticker (since Hadley Wickham is strongly opposed to multiple Y axes)
#Input - dataframe with `% Change` columns.  Expected to have 'SPY' and one other ticker of interest.
#Actions -- Creates a column where the SPY % change vs open has an offset added to it so it will plot centered on the relative changes of a different ticker.
fnOffsetSpy <- function(aDataFrame){
  offset = mean(tempData$`% Change`[tempData$Symbol == 'AMD']) - mean(tempData$`% Change`[tempData$Symbol == 'SPY'])
  aDataFrame <- aDataFrame %>% mutate (plotLast = case_when(Symbol == 'SPY' ~ `% Change` + offset, TRUE ~ `% Change`))
  return(aDataFrame)
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
tempData <- read_csv("C:/Users/Fred/Documents/R/Sample Data/AMDSPY_4secQuotes.csv")

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
      textInput('TickerSearch', label = 'Specify Ticker'),
      submitButton('Submit New Ticker'),          
      
      #List of everything pulled currently
      dataTableOutput("IndicatorList"),
      #Toggles to parse down for RS / RW / Volume
      
    ),
    
    # Plots
    mainPanel(
      plotOutput("ChartVsSpy"),
      plotOutput("VolumeProfile"), 
      #plotOutput("IndicatorVsTime"),
      #plotOutput("IndicatorBivariate")
    )
  )
)

# Define server logic required to generate plots
#Server
#####
server <- function(input, output) {
  #Three things need to be going on with data.
  #1 - Querying yahoo for new quotes
  #2 - Updating all tickers for required metrics
  #3 - Cleaning data for SPY & selected ticker ( & sector?) to plot
  
  tempData <- tempData %>% arrange(Symbol, `Trade Time`)
  tempData <- tempData %>% mutate (intervalVolume = case_when(Symbol == lag(Symbol)~Volume - lag(Volume)))
  
  tempData <- fnOffsetSpy(tempData)
  
  print('frequencytest')
  
  output$ChartVsSpy <- renderPlot({
    ggplot(data = tempData, aes(x = `Trade Time`, y= plotLast)) + geom_line(aes(group = Symbol)) + geom_point(aes(color = Symbol)) 
  })
  
  output$VolumeProfile <- renderPlot({
    ggplot(data = tempData, aes(x = `Trade Time`, y= plotLast))+ geom_violin(aes(weight = intervalVolume, group = Symbol, color = Symbol, fill = Symbol,  alpha = 255)) 
  })
  
  #output$IndicatorVsTime <- renderPlot({})
  #output$IndicatorBivariate <- renderPlot({})
}

# Run the application 
shinyApp(ui = ui, server = server)


########################
################################################################################
############ MAIN SCRIPT
####


# define dates
today        <- Sys.Date()
endLastWeek  <- floor_date(today, "week")  - 2
endLastMonth <- floor_date(today ,"month") - 1

### create date range to use
startDt <- as.Date("2013-03-01")
weekDts <- seq.Date(startDt , endLastWeek, by= "1 week")
weekDts <- seq.Date(startDt , as.Date("2018-04-30"), by= "1 week")
#mthDts <- seq.Date(startDt , today, by= "1 month") - 1
#allDts <- unique(c(weekDts, mthDts))


# check if new dates indicators requested
missingDts <- weekDts[!weekDts %in% unique(indicators$Date)]

if (length(missingDts) > 0) {

    tic     <- unique(indicators$Ticker)
    
    newData <- getBdhData(tic, indicatorFields, missingDts)
    
    indicators <- rbind(indicators, newData)
    
    save(indicators, file= "TidyData/indicators.RData")
    
}


# check if new fields are  requested
newFields <- newFields[!newFields %in% indicatorFields]

if (length(newFields) > 0) {

    tic <- unique(indicators$Ticker)
    dts <- unique(indicators$Date)
    
    newData <- getBdhData(tic, newFfield, dts)

    indicators <- rbind(indicators, newData)
    
    save(indicators, file= "TidyData/indicators.RData")
    
    #empty newFields.csv
    cat(NULL, file="RawData/newFields.csv")
    
}


# check if newTickers missing in Securities Database
missingTic <- newTic[!Ticker %in% securities$Ticker, Ticker]
missingTic <- as.character(missingTic)

if (length(missingTic) > 0) {
    
    # complete securities datas
    newData <- getBdpData(missingTic, tickerFields)

    # merge and save  newData with exisiting database
    securities <- rbind(securities, newData)
    
    save(securities, file="TidyData/securities.RData")

}


# check if newTickers missing in Indicators database
missingTic <- newTic[!Ticker %in% indicators$Ticker, Ticker]
missingTic <- as.character(missingTic)

if (length(missingTic) > 0) {
    
    # complere indicators database
    newData <- getBdhData(missingTic,indicatorFields, weekDts)
    
    # merge and save indicators          
    indicators <- rbind(indicators, newData)
    
    save(indicators, file="TidyData/indicators.RData")

}

#empty newTickers.csv
cat(NULL, file="RawData/newTickers.csv")



####
############ END OF SCRIPT
################################################################################
########################




# check bloomberg data usage (max 250k/day)
rowSums(newData[, lapply(.SD, function(x) length(unique(x)))][,-1:-2])



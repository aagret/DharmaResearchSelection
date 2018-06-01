
########################
################################################################################
############ MAIN SCRIPT
####

# fix Bloomberg daily limit
blpMax = 200000

# define dates
dts <- getDates()

# check if new dates indicators requested ####
missingDts <- dts$weekDts[!dts$weekDts %in% unique(indicators$Date)]

if (length(missingDts)) {

    tic  <- as.character(unique(indicators$Ticker))
    flds <- colnames(indicators[, -c("Ticker","Date","T12_TOTAL_DISTRIBUTION_YIELD")])
    
    if (isBlpOK(tic, flds, missingDts)) {
        
        newData <- getBdhData(tic, flds, missingDts)
        
        indicators <- unique(rbind(indicators, newData, fill= TRUE))
        
        save(indicators, file= "TidyData/indicators.RData")
        
    }
    
}



# check if new fields are  requested ####
#newFields <- newFields[!newFields %in% indicatorFields]
newFields <- indicatorFields$Field[!indicatorFields$Field %in% colnames(indicators)]

if (length(newFields) > 0) {

    tic  <- as.character(unique(indicators$Ticker))
    flds <- as.character(newFields)
    dts  <- sort(unique(indicators$Date))
    
    if (isBlpOK(tic, newFields)) newData <- getBdhData(tic, flds, dts)
    
    setkey(newData, Ticker, Date)
    setkey(indicators, Ticker, Date)
    
    indicators <- newData[indicators]
    
    save(indicators, file= "TidyData/indicators.RData")

}


# check if newTickers missing in Securities Database ####
missingTic <- newTic[!Ticker %in% securities$Ticker, Ticker]
missingTic <- c(missingTic, 
                securities$Ticker[!securities$Ticker %in% indicators$Ticker])
missingTic <- as.character(missingTic)

if (length(missingTic) > 0) {
    
    flds <- colnames(securities)[-1]
    
    # complete securities datas
    newData <- getBdpData(missingTic, flds)

    # merge and save  newData with exisiting database
    securities <- unique(rbind(securities, newData, fill= TRUE))

    save(securities, file="TidyData/securities.RData")

    # complere indicators database
    
    flds <- colnames(indicators)[-c(1,2,27)]
    dts  <- sort(unique(indicators$Date))
    
    newData <- getBdhData(missingTic,flds, dts)
    
    # merge and save indicators          
    indicators <- unique(rbind(indicators, newData, fill= TRUE))

    save(indicators, file="TidyData/indicators.RData")

}

#empty newTickers.csv
#cat(NULL, file="RawData/newTickers.csv")

# carry over last observation

    # merge all 
    setkey(indicators, Ticker, Date)
    setkey(securities, Ticker)
    database <- securities[indicators]

    # replace NA by previous value
    database <- database[, lapply(.SD, function(x) na.locf(x, na.rm= FALSE)), by= Ticker]

    # add calculated fields
    database <- calculatedFields(database)
    
    save(database, file="TidyData/database.RData")

####
############ END OF SCRIPT
################################################################################
########################




# check bloomberg data usage (max 250k/day)
# rowSums(newData[, lapply(.SD, function(x) length(unique(x)))][,-1:-2])



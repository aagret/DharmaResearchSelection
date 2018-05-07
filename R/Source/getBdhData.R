
# get blooberg historical datas

getBdhData <- function(tic= missingTic, flds= fields, dts= weekDts)
    
    if (length(tic) > 0 ) {
        
        con <- blpConnect()
        
        opt = c("periodicityAdjustment"="CALENDAR",
                "periodicitySelection"="WEEKLY",
                "nonTradingDayFillMethod"="PREVIOUS_VALUE")

#       opt = c("periodicitySelection"="WEEKLY")
        
        db <- bdh(tic, 
                  flds, 
                  start.date= dts[1],
                  end.date= last(dts),
                 # include.non.trading.days = TRUE,
                  options= opt,
                  verbose= TRUE)
        
        
        
        blpDisconnect(con)
        
        # set to data.frame
        if (length(tic) > 1) db <- ldply(db, data.frame) else db <- cbind(missingTic, db)
        
        colnames(db)[1:2] <- c("Ticker", "Date")
        
        # add calculated fields
        db <- calculatedFields(db)
        
        setDT(db, key= c("Ticker", "Date"))
        
        db <- db[, lapply(.SD, function(x) na.locf(x, na.rm= FALSE)), by= Ticker]
        
    }



# get blooberg historical datas

getBdhData <- function(tic= missingTic, flds= fields, dts= weekDts)
    
    dFields <- flds[Type== "D",]
    fFields <- flds[Type== "F",]
    
    BDH(dFields, opt=Daily)
    BDH(fFields, opt= Securities[Ticker==fFields, Freq,])
    
    
    
    
    
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
                  include.non.trading.days = FALSE,
                  options= opt,
                  verbose= TRUE)
        
        
        
        blpDisconnect(con)
        
        # set to data.frame
        if (length(tic) > 1) db <- ldply(db, data.frame) else db <- cbind(tic, db)
        
        colnames(db)[1:2] <- c("Ticker", "Date")
        
        # add calculated fields
        if(length(db$T12M_NET_CAPITAL_STOCK) > 0) db <- calculatedFields(db)
        
        setDT(db, key= c("Ticker", "Date"))
        
        db <- db[, lapply(.SD, function(x) na.locf(x, na.rm= FALSE)), by= Ticker]
        
    }


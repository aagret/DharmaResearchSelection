
# get blooberg historical datas

getBdhData <- function(tic= missingTic, flds= fields, dts= weekDts)

    if (length(tic) > 0 ) {

        flds <- indicatorFields[indicatorFields$Field %in% flds,]
        flds <- split(flds$Field, f= factor(flds$Period))

        con <- blpConnect()

        # get daily datas   
        optBase = c("periodicityAdjustment"="CALENDAR",
                    "nonTradingDayFillOption"="ACTIVE_DAYS_ONLY",
                    "nonTradingDayFillMethod"="PREVIOUS_VALUE")
        
        
        if (length(as.character(flds$D)) > 0) {
            
            opt <- c(optBase, "periodicitySelection"="WEEKLY")
            
            db_d <- bdh(tic, 
                        as.character(flds$D), 
                        start.date= dts[1],
                        end.date= last(dts),
                        include.non.trading.days = TRUE,
                        options= opt,
                        verbose= TRUE)
            
            # set to data.frame
            if (is.data.frame(db_d)) db_d <- cbind(.id=tic, db_d) else db_d <- ldply(db_d, data.frame)
            
            blpMax <<- blpMax - length(which(!is.na(db_d[-1])))
        }
            
        # get periodicity datas
        tic <- cbind(tic, 
                     substring(securities[Ticker %in% tic, Primary_Periodicity], 1, 1))
        
        tic <- split(tic[,1], f= tic[,2], drop= TRUE)
        
        opt <- c(optBase, "periodicitySelection"="QUARTERLY")
        
        
        # get periodicity datas
        if (length(as.character(flds$F)) > 0) {
            
            db_f <- lapply(tic, function(x) bdh(x, 
                                                as.character(flds$F), 
                                                start.date= dts[1],
                                                end.date= last(dts),
                                                include.non.trading.days = TRUE,
                                                options= opt,
                                                verbose= TRUE))
            
            db_f <- ldply(db_f, function(x) ldply(x, data.frame))
            
            
            blpMax <<- blpMax - length(which(!is.na(db_f[-1])))
            
        }
        
        blpDisconnect(con)
        
        if (exists("db_d")) db <- merge(db, db_f, by= c(".id", "date"), all=TRUE) else db <- db_f
        
        db$X..i.. <- NULL
        
        colnames(db)[1:2] <- c("Ticker", "Date")
        
        setDT(db, key= c("Ticker", "Date"))
        
        
    }



getStyle <- function(db= database) {
    
    
    setkey(db, Date)
    
    valFlds <- as.character(indicatorFields[indicatorFields$Class == "Value", 2])
    groFlds <- as.character(indicatorFields[indicatorFields$Class == "Growth",2])
    
    db[, Momentum:= rollmeanr(PX_LAST, 5, fill= NA) / rollmeanr(PX_LAST, 20, fill= NA), 
       by= Ticker]
    
    val <- db[, lapply(.SD, function(x) rank(x, na.last= "keep") /
                           sum(!is.na(x))),
              .SDcols= valFlds]
    
    gro <- db[, lapply(.SD, function(x) rank(x, na.last= "keep") /
                           sum(!is.na(x))),
              .SDcols= groFlds]
    
    db[, ":=" (Value=  rowMeans(val, na.rm= TRUE),
               Growth= rowMeans(gro, na.rm= TRUE))]
    
    db <- db[, c("Date", "Ticker", "Growth", "Value", "Momentum"), with= FALSE]
    
    }



checkMissingIndicators <- (db= database) {
    
    m_db <- db[, lapply(.SD, function(x) all(is.na(x))), by= .(Ticker, Date)]
    m_db <- melt(m_db, id=1)
    m_db <- m_db[value == TRUE]
    m_db <- split(m_db$Ticker, f= m_db$variable, drop = TRUE)
    
    dts <- unique(db$Date)
    
    res <- lapply(m_db, function(x) getBdhData(unlist(x), names(x), dts))

}



### load new Ticker List

loadNewTickers <- function(file= filename) {
    
    file   <- paste0("RawData/", file)
    
    newTic <- data.table() # needed if file is empty
    newTic <- fread(file, sep=",", header=FALSE)

    colnames(newTic) <- "Ticker"

    # add Bloomberg "Equity" key to tickers
    newTic[, Ticker:= factor(paste0(Ticker, " Equity"))]
    
    setkey(newTic, Ticker)
    
    return(newTic)
    
}

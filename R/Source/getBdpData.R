
# function to download from bloomberg missing datas

getBdpData <- function(tic= missingTic, flds= fields)

if (length(tic) > 0 ) {
    
    con <- blpConnect()
    db  <- bdp(tic, flds)
    
    db$Ticker <- rownames(db)
    
    blpDisconnect(con)
    
    setDT(db, key= "Ticker")
    
}
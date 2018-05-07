
## calc rating
calcScoring <- function(db= database, crit= criteria) {
    
    flds <- as.character(crit$Field)
    
    db[, c(flds):= lapply(.SD,
                          function(x) frank(x, na.last= "keep") / 
                              sum(!is.na(x))),
       by= Date,
       .SDcols= flds]
    
    
    changeNAtoZero(db)
    
    db[, Scoring:= rowMeans(.SD, na.rm= TRUE), .SDcols= flds]
    
    db[, Ranking:= frank(Scoring, na.last= "keep") / sum(!is.na(Scoring)), 
       by= Date]
    
    db[, SectorRank:= frank(Scoring, na.last= "keep") / sum(!is.na(Scoring)), 
       by= .(GICS_Sector_Name, Date)]
    
    db[, GroupRank:= frank(Scoring, na.last= "keep") / sum(!is.na(Scoring)), 
       by= .(GICS_Industry_Group_Name, Date)]
    
    db[, IndustryRank:= frank(Scoring, na.last= "keep") / sum(!is.na(Scoring)), 
       by= .(GICS_Industry_Name, Date)]
    
    flds <- c("Ticker", "Date", "Scoring", "Ranking", "SectorRank", "GroupRank",
              "IndustryRank", as.character(crit$Field))
    
    db <- db[, flds, with= FALSE]
    
    setkey(db, Ticker, Date)
    
}


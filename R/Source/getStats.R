
# get universe statistics

getStats <- function(db= database) {
    
    colNum <- colnames(db)[-1:-2]
    
    res <- db[, lapply(.SD, summary), .SDcols=colNum, by= Date]

    res <- cbind("Name"=c("Min.", "1st Qu.", "Median", "Mean",
                          "3rd Qu.", "Max.", "NA's"),
                 res)
    
    res <- res[Name != "NA's",]
    
}
    

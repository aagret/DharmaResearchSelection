

# cleanData Function ------------------------------------------------------
# a function to remove NA's and format Dates

cleanData <- function(db= database) {

    ## remove NA's and set Date format
    db      <- db[!is.na(db$Rating),]
    db$Date <- as.Date(db$Date)
    
    ## remove old incomplete datas
    db <- db[db$Date >= as.Date("2013-05-31"),] # TO BE REMOVED ?
    
}

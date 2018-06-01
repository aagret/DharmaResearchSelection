
# function to define dates to use in script
getDates <- function(){
    
    dts <- list()
    
    dts$startDt      <- as.Date("2013-03-01")
    dts$today        <- Sys.Date()
    
    dts$endLastWeek  <- floor_date(dts$today, "week")  - 2
    dts$endLastMonth <- floor_date(dts$today ,"month") - 1
    
    #dts$weekDts      <- seq.Date(dts$startDt, as.Date("2018-04-30"), by= "1 week")
    dts$weekDts      <- seq.Date(dts$startDt , dts$endLastWeek, by= "1 week")
    dts$mthDts       <- seq.Date(dts$startDt, dts$today, by= "1 month") - 1
    dts$allDts       <- unique(c(dts$weekDts, dts$mthDts))
    
    return(dts)
    
}
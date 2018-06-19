
#### Statistiques de la Database ####

getSummary <- function(db= database) {
    
    db <- db[, .SD, .SDcols=which(sapply(db,is.numeric))]
    
    stats <- matrix(, nrow = 7, ncol = ncol(db))
    stats <- rbind(Min.=     sapply(db, min,            na.rm=TRUE),
                   '1st Qu.'=sapply(db, quantile, 0.25, na.rm=TRUE),
                   Median=   sapply(db, median,         na.rm=TRUE),
                   Mean=     sapply(db, mean,           na.rm=TRUE),
                   '3rd Qu.'=sapply(db, quantile, 0.75, na.rm=TRUE),
                   MAx.=     sapply(db, max,            na.rm=TRUE),
                   '!NA'=    sapply(db, function(x) sum(!is.na(x))),
                   'NA'=     sapply(db, function(x) sum(is.na(x))))
    
   round(stats, 2)

}

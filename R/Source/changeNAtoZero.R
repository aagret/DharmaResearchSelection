
changeNAtoZero <- function(db= database) {
    
    lapply(names(db),
           function(x) set(db, 
                           which(is.na(db[[x]])), 
                           j= x, 
                           value= 0)
    )
    
}


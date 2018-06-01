
## calc rating
calcRating <- function(db= database, crit= criteria) {

    fix <- db[,1:11]
    db  <- db[,-1:-11]
    
    for (fld in colnames(db)){
        
        if (fld %in% crit$Field) {
            
            data <- db[, fld, with= FALSE]
            
            nr <- ifelse(crit[Field == fld, Select] == "<" &
                             data < crit[Field == fld,Target],
                         crit[Field == fld, Weight],
                         ifelse(crit[Field == fld, Select] == ">" &
                                    data > crit[Field == fld, Target],
                                crit[Field == fld, Weight],
                                0))
            
            if(!exists("res")) res <- nr else res <- cbind(res, nr)
        }
        
    }
    
    res <- cbind(fix, "Rating"=rowSums(res, na.rm= TRUE), res)
    
    res[, Rating:= max(Rating) + 1 - Rating]
    
}


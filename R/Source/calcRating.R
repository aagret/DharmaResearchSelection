
## calc rating
calcRating <- function(db= database, crit= criteria) {
    
    
    fix <- db[,1:2]
    db  <- db[,-1:-2]
    
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
            
            if(!exists("rating")) rating <- nr else rating <- cbind(rating, nr)
        }
        
    }
    
    rating <- cbind(fix, "Rating"=rowSums(rating, na.rm= TRUE), rating)
    
    rating[, Rating:= max(Rating) + 1 - Rating]
    
}


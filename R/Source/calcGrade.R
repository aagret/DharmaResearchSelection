
## calc rating
calcGrade <- function(db= database, crit= criteria, stats= stats) {
    
    
    fix <- db[,1:2]
    db  <- db[,-1:-2]

    for (fld in colnames(db)){
        
        if (fld %in% crit$Field) {
            
            data <- db[, fld, with= FALSE]
            
            stMed  <- as.numeric(stats[Name == "Median", fld, with= FALSE])
            stMax  <- as.numeric(stats[Name == "Max.",   fld, with= FALSE])
            stMin  <- as.numeric(stats[Name == "Min.",   fld, with= FALSE])
            weight <- as.numeric(crit[Field == fld, Weight]) 
            
            
            nr <- pmax(-1,
                       (data - 
                            stMed) /
                           
                           ifelse(crit[Field == fld, Select] == "<",
                                  stMin,
                                  stMax),
                       na.rm=TRUE) *
                
                weight
            
            

            if(!exists("grade")) grade <- nr else grade <- cbind(grade, nr)
            
        }
    
    }
    
    colnames(grade) <- crit$Field
    
    grade <- cbind(fix, "Grade"=rowSums(grade, na.rm= TRUE), grade)
    
}



isBlpOK <- function(tic = tic, flds= fields, dts= dates) {

    request <- length(tic) * 
        (length(flds) - 1) * 
        (as.integer((max(dts) - min(dts)) / 91) + 1)  

    if (request < blpMax) return(TRUE) else return(FALSE)

}

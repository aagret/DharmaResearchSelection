
### add calculated fields

calculatedFields <- function(db= database) {
    
    # T12 Total Distribution
    db$T12_TOTAL_DISTRIBUTION_YIELD <-
        - db$T12M_NET_CAPITAL_STOCK / 
        (db$CUR_MKT_CAP / 100)

    db[!is.na(db$T12_TOTAL_DISTRIBUTION_YIELD), "T12_TOTAL_DISTRIBUTION_YIELD"] <-
        db[!is.na(db$T12_TOTAL_DISTRIBUTION_YIELD),"T12_TOTAL_DISTRIBUTION_YIELD"] +
        db[!is.na(db$T12_TOTAL_DISTRIBUTION_YIELD),"EQY_DVD_YLD_12M"]
    
    db[is.na(db$T12_TOTAL_DISTRIBUTION_YIELD), "T12_TOTAL_DISTRIBUTION_YIELD"] <-
        db[is.na(db$T12_TOTAL_DISTRIBUTION_YIELD),"EQY_DVD_YLD_12M"]


               



    # # T12 Total Distribution
    # db[, T12_TOTAL_DISTRIBUTION_YIELD:= T12M_NET_CAPITAL_STOCK / CUR_MKT_CAP]
    # 
    # db[ is.na(T12_TOTAL_DISTRIBUTION_YIELD),
    #     T12_TOTAL_DISTRIBUTION_YIELD:= EQY_DVD_YLD_12M]
    # 
    # db[!is.na(T12_TOTAL_DISTRIBUTION_YIELD),
    #    T12_TOTAL_DISTRIBUTION_YIELD:= T12_TOTAL_DISTRIBUTION_YIELD + EQY_DVD_YLD_12M]
    # 
    # 
    
    
    return(db)

}

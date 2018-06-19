
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


     # Price to Book value
    db$BOOK_TO_PRICE <- db$BOOK_VAL_PER_SH / db$PX_LAST
    
    # FCF Yield
    db$FCF_YIELD <- db$TRAIL_12M_FREE_CASH_FLOW_PER_SH / db$PX_LAST
    
    # Sales to Price
    db$SALES_TO_PRICE <- db$TRAIL_12M_SALES_PER_SH / db$PX_LAST
    
    # Ebitda to Price
    db$EBITDA_TO_PRICE <- db$TRAIL_12M_EBITDA / db$PX_LAST
    



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

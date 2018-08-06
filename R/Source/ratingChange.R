


# 13/4 weeks change

ratingChange <- function(db= rating, n= period, t= tolerance) {
    
    r <- db[, .(Date, Short_Name, Ticker, Rating,
                    Chg= c(rep(NA, times= n), diff(Rating, n))), by= Ticker]
    
    r <- r[, .SD[c(.N - n, .N)], by= Ticker]
    
    r[, ':=' (OldDt= Date[1],
              NewDt= Date[2],
              OldRat=Rating[1],
              NewRat=Rating[2],
              Chg= Rating[1] - Rating[2]),
      by= Ticker]
    
    
    r <- r[abs(Chg) >= t, lapply(.SD, last), by= Ticker]
    
   # r <- list(r[grepl(" US ", Ticker),], r[!grepl(" US ", Ticker),])
    
    list(
        split(r[grepl(" US ", Ticker),  .(Chg, Ticker, Short_Name, OldDt, OldRat, NewRat)], 
          by= "Chg"),
        split(r[!grepl(" US ", Ticker), .(Chg, Ticker, Short_Name, OldDt, OldRat, NewRat)], 
              by= "Chg"))
    
}
 


ratingChange(rating, 4, 1.5)  
hist(r$Rating)

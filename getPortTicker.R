
# get Dharma positions

getPortTicker <- function() {
    
    con <- blpConnect()
    
    port <- getPortfolio("U16534512-70 Client", "Portfolio_Mposition")
    
    blpDisconnect(con)
    
    setDT(port)
    
    port <- port[!grepl("\\.", Security) & grepl("Equity", Security), Security]
    gsub("GY Equity", "GR Equity", port)
}
    

portTicker <- getPortTicker()

r <- rating[portTicker][!Industry_Sector =="Funds",]
r <- r[, lapply(.SD, last), by= Ticker]
hist(r$Rating)
density(r$Rating)
plot(r$Rating)
abline(lm(Rating ~ Rating, data=r))


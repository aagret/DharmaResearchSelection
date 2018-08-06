# check tickers

library(Rblpapi)

# download index members
con <- blpConnect()

spx <- bds("SPX Index", "Indx_Members")

# format exchange codes and save ticker list
spx <- sub(" UN", " US", spx[,1])
spx <- sub(" UW", " US", spx)

# download current be500 tickers from Bloomberg
be500 <- bds("BE500 Index", "Indx_Members")[,1]


# check existing ticker status
status <- bdp(as.character(securities$Ticker), c("Market_Status", "Eqy_Fund_Ticker", "Ult_Parent_Ticker_Exchange"))
status$Ticker <- rownames(status)
status <- status[status$Market_Status != "ACTV",]

blpDisconnect(con)

# merge all
new <- c(spx, be500, status$Ult_Parent_Ticker_Exchange)
new <- paste0(new, " Equity")

# check if any new ticker and save in .csv file
new <- new[!new %in% securities$Ticker]
new <- gsub(" Equity", "", new)

write.table(new, file= "RawData/newTickers.csv",
          row.names = FALSE,
          col.names = FALSE,
          quote= FALSE,
          append = TRUE)








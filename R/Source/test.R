
library(data.table)
library(TTR)

hist <- unique(database)
hist <- database[,.(Ticker, Date, PX_LAST)]
setkey(hist, Ticker, Date)


hist[,PX_LAST:= ROC(PX_LAST), by= Ticker]

hist[is.na(PX_LAST), PX_LAST:=0]


h2 <- dcast(hist, Date ~Ticker)

library(fAssets)
hclust <- assetsSelect(h2[,2:50], "hclust")
plot(hclust)


plot(exp(cumsum(h2[,"AAPL US Equity"])), type="l")
plot(h2[,"AAPL US Equity"], type="l")

# Check if double datas in database ---------------------------------------
findDouble <- function(h2) {
  double <- apply(h2, 2, function(r) any(r == 2))
  double <- names(double[double==TRUE])
}

lapply(findDouble(h2), function(x) {
    h <- database[Ticker == x,][duplicated(Date),]
    database[h, on= c("Ticker", "Date"), which=TRUE]
    })
       
database <- database[c(-17663, -54196),]
save(database, file="TidyData/database.RData")




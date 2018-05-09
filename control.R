
# check with past publications

old <- read.xlsx("M:/Alexandre/Cell D/BBG working files/03 - Artha Rating History.xlsm", 
                 sheetName = "Histo",
                 startRow = 2, 
                 endRow = 172,
                 header = TRUE)

d  <- gsub("X", "", colnames(old)[c(-1:-3, -226)])
dt <- as.Date(as.numeric(d) - 3, origin="1899-12-30")
colnames(old)[c(-1:-3, -226)] <- as.character(as.Date(dt, "%Y-%m-%d"))
colnames(old)[1] <- "Ticker"
#old$Ticker <- as.character(old$Ticker)
old <- old[,-2:-4]
setorder(old, Ticker)

selTic <- as.character(old[,1])

db <- rating[Ticker %in% selTic][, 1:3]
setorder(db, -Date)

db <- dcast(db, Ticker ~Date)
setcolorder(db, c("Ticker", rev(colnames(db)[-1])))

db2 <- db[, 1:50]
old2 <- old[colnames(old) %in% colnames(db2)]
db2 <- db2[,colnames(old2), with=FALSE]
old2 <- old2[old2$Ticker %in% db2$Ticker,]

setorder(db2, Ticker)
setorder(old2,Ticker)

db2 <- as.data.frame(db2)


graphDiff <- function(x= tickerNum){
    
    x1 <- xts(as.numeric(db2[x,-1]), order.by = as.Date(colnames(db2[x,-1])))
    x2 <- xts(as.numeric(old2[x,-1]), order.by = as.Date(colnames(old2[x,-1])))
    
    ymin <- min(x1, x2, x1-x2, na.rm= TRUE)
    ymax <- max(x1, x2, x1-x2, na.rm= TRUE)
    
    plot(x1, type="l", ylim=c(ymin,ymax),col="red")
    lines(x2, col="green", type="l")
    lines(x1-x2, type="l", col="blue")
    
}

graphDiff(32)


plot(a)

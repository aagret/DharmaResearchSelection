

## load Libraries
source("R/Source/calcScoring.R")
source("R/Source/changeNAtoZero.R")

## load database
load("TidyData/database.RData")
setDT(database, key= c("Ticker", "Date"))

## get old criterias
crit <- read.xlsx("RawData/criteriaOrigin.xlsx", sheetIndex = 1)
setDT(crit)
crit <- crit[complete.cases(crit)] 

## get old scoring
oldScoring <- calcScoring(database, crit)

##compute returns
oldReturn <- database[, .(Ticker, Date, PX_LAST)][oldScoring] 
oldReturn[, Return:= shift(ROC(PX_LAST), type="lead"), by= Ticker]

## carry forward NA's or to zero
oldReturn <- oldReturn[, lapply(.SD,
                                function(x) na.locf(x, na.rm= FALSE)),
                       by= .(Ticker)]

oldReturn[is.na(Return), Return:= 0]

setkey(oldReturn, Ticker, Date)

oldReturn[, Group:= cut(IndustryRank,
                        breaks= 10,
                        labels= FALSE)]

mean <- oldReturn[, mean(Return), by= .(Date, Group)]
setkey(mean, Group, Date)


mean[is.infinite(V1), V1:= 0]

mean[, V1:=c(1, exp(cumsum(V1[-1]))), by= Group]


g <- ggplot(data= mean) + 
    aes(x= Date, group= Group) + facet_wrap(~Group, scales = "free_x") +
    geom_line(aes(y= V1, color="old"))


+ 
    geom_line(aes(y= NewReturn, color="new")) +
    geom_line(aes(y= IdxReturn, color="idx")) +
    facet_wrap(~Group, scales = "free_y")  

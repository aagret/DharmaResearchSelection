
# Load Libraries ----------------------------------------------------------
library(lubridate)
library(TTR)
library(ggplot2)
library(data.table)
library(xlsx)
library(xts)


# Load Functions ----------------------------------------------------------
### load Functions ###
source("R/Source/cleanData.R")


# Load Databse ------------------------------------------------------------
load("TidyData/database.RData")
setDT(database, key= c("Ticker", "Date"))


# Load old published datas ------------------------------------------------
old <- read.xlsx("M:/Alexandre/Cell D/BBG working files/03 - Artha Rating History.xlsm", 
                 sheetName= "Histo",
                 startRow=  2, 
                 endRow=    172,
                 header=    TRUE,
                 stringsAsFactors= FALSE)


# Format old publish datas ------------------------------------------------
## remove useless columns
old <-old[, c(-2, -3, -ncol(old))]

## change colnames for dates in proper format
dt <- gsub("X", "", colnames(old)[-1])
dt <- as.Date(as.numeric(dt) , origin="1899-12-30")
dt <- as.Date(ifelse(wday(dt) != 6, dt - wday(dt + 1), dt), origin="1970-01-01")

colnames(old) <- c("Ticker", as.character(as.Date(dt, "%Y-%m-%d")))

## put it in tabular long form 
old <- melt(old, id.vars = 1)
colnames(old) <- c("Ticker","Date","Rating")

## clean Datas
old <- cleanData(old)

## set to DT
setDT(old, key= c("Ticker", "Date"))


# Get and Format New datas ------------------------------------------------   
## get format Rating datas
setkey(rating, Ticker, Date)

selTic <- unique(old[,1])
new    <- rating[selTic][ ,c(1,11,12)]

## clean Datas
new <- cleanData(new)

# set to DT
setDT(new, key= c("Ticker", "Date"))


# Load and Format Index datas ---------------------------------------------
idx <- read.xlsx("RawData/indexData.xlsx", sheetIndex = 1)
setDT(idx, key= "Date")


# Merge all datas ---------------------------------------------------------
## merge all common datas
allData <- database[, .(Ticker, Date, PX_LAST)][new][old]

setkey(allData, Date)
allData <- idx[allData]
setkey(allData, Ticker, Date)

colnames(allData) <- c("Date", "Idx_Px", "Ticker", "Px", "NewRating", "OldRating")


# Calc Returns ------------------------------------------------------------
## calc by Ticker/Day
allData[, `:=` (Idx_Ret= shift(ROC(Idx_Px), type= "lead"),
                Ret=     shift(ROC(Px),     type= "lead")),
        by= Ticker]

# carry over if NA's of 0 if not possible
allData <- allData[, lapply(.SD, 
                            function(x) na.locf(x, na.rm= FALSE)), 
                   by= .(Ticker, Date)]
# or to zero
allData[is.na(Idx_Ret), Idx_Ret:= 0]
allData[is.na(Ret), Ret:= 0]


# Calc Return Indexes -----------------------------------------------------
## calc Old returns
setkey(allData, OldRating, Date)
oldRet <- allData[, mean(Ret, na.rm= TRUE), by= .(OldRating, Date)]
oldRet[, V1:= c(1, exp(cumsum(V1[-1]))), by= OldRating] 
colnames(oldRet) <- c("Group", "Date", "OldReturn")

## cale New returns
### group returns by Rating Group
newRet <- allData

newRet[, Group:= cut(NewRating,
                                  breaks= c(-Inf, 1.99, 2.99, 3.99, 4.99, 5.99, 6.99, 7.99, Inf),
                                  labels= FALSE)] #c("1", "2", "3", "4", "5", "6", "7")))]

newRet <- newRet[, mean(Ret, na.rm= TRUE), by= .(Group, Date)]
setkey(newRet, Group, Date)

newRet[, V1:= c(1, exp(cumsum(V1[-1]))), by= Group] 
colnames(newRet) <- c("Group", "Date", "NewReturn")

idxRet <- allData[, mean(Idx_Ret), by= Date]
idxRet[, V1:= c(1, exp(cumsum(V1[-1])))]
colnames(idxRet)[2] <- "IdxReturn"


# Merge all returns indexes -----------------------------------------------
## merga datas amd add Idx
db <- merge(newRet, oldRet, all= TRUE)
setkey(db, Date)
db <- db[idxRet]
setkey(db, Group, Date)

# carry over previous value in NA's
db <- db[, lapply(.SD,
                  function(x) na.locf(x, na.rm= FALSE)), 
         by= .(Group)]


# PLOT & STATS-------------------------------------------------------------
##PLOT
plot(db[Group==1, IdxReturn], type="l", col="red")
lines(db[Group==1, OldReturn], col="blue")
lines(db[Group==1, NewReturn], col="green")

g <- ggplot(data= db) + 
    aes(x= Date, group= Group) +
    geom_line(aes(y= OldReturn, color="old")) + 
    geom_line(aes(y= NewReturn, color="new")) +
    geom_line(aes(y= IdxReturn, color="idx")) +
    facet_wrap(~Group, scales = "free_y")  

## STATS
library(PerformanceAnalytics)

setkey(db, Group, Date)
setcolorder(db, c("Date", "Group", "NewReturn", "OldReturn", "IdxReturn"))

s <- lapply(split(db[!is.na(Group),], by="Group"), as.xts)
s <- lapply(s, function(x) table.AnnualizedReturns(ROC(x[,-1])))
       
x <- setorder(allData[ , length(unique(Ticker)), by= Group], "Group")
s
x
g

       
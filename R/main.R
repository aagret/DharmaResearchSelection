
#### load securities infos

## Init ###
# load Libraries
library(data.table)
library(lubridate)
library(plyr)
library(Rblpapi)
library(xts)
library(xlsx)

# load functions
source("R/Source/loadNewTickers.R")
source("R/Source/getBdpData.R")
source("R/Source/getBdhData.R")
source("R/Source/calculatedFields.R")
source("R/Source/calcRating.R")
source("R/Source/getStats.R")
source("R/Source/calcGrade.R")
source("R/Source/calcScoring.R")

# define dates
today <-Sys.Date()
endLastWeek  <- floor_date(today, "week")  - 2
endLastMonth <- floor_date(today ,"month") - 1


# load new Tickers
newTic <- loadNewTickers("newTickers.csv")

# load fields
fields <- fread("RawData/tickersFields.csv", sep=",")
fields <- fields$Field

# load exisiting security database
load("TidyData/securities.RData")
setkey(securities, Ticker)


# check if newTickers are not in Database
missingTic <- newTic[!Ticker %in% securities$Ticker, Ticker]
missingTic <- as.character(missingTic)



# complete securities datas
newData <- getBdpData(missingTic, fields)

# merge and save  newData with exisiting database
securities <- rbind(securities, newData)
save(securities, file="TidyData/securities.RData")



### create date range
startDt = as.Date("2013-03-01")
weekDts <- seq.Date(startDt , endLastWeek, by= "1 week")
weekDts <- seq.Date(startDt , as.Date("2018-04-30"), by= "1 week")
#mthDts  <- seq.Date(startDt , today, by= "1 month") - 1
#allDts <- unique(c(weekDts, mthDts))




### load indicators database
fields <- fread("RawData/indicatorsFields.csv", sep=",")
fields <- fields$Field

load("TidyData/indicators.RData")
#setkey(indicators, Ticker)


missingTic <- newTic[!Ticker %in% indicators$Ticker, Ticker]
missingTic <- as.character(missingTic)

newData <- getBdhData(missingTic,fields, weekDts)


          
indicators <- rbind(indicators, newData)
save(indicators, file="TidyData/indicators.RData")


#empty newTickers.csv
cat(NULL, file="RawData/newTickers.csv")




rowSums(indicators[, lapply(.SD, function(x) length(unique(x)))][,-1:-2])


#### get Rating criterias
crit <- read.xlsx("RawData/criteriaLast.xlsx", sheetIndex = 1)
setDT(crit)
crit <- crit[complete.cases(crit)]

## calc indicators
rating <- calcRating(indicators, crit)

stats <- getStats(indicators) # rolling stats !!

grade <- calcGrade(indicators, crit, stats) # adjust to avoid rolling stats OR?

score <- calcScoring(indicators[securities], crit)

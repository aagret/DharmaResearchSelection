## R Script used to retrieve financial indicators on a selection of securities and 
## compute various metrics, grade, rating etc.. to help build a portfolio and publish
## research ie DharmaResearch Equity


########################
########  Init  ########
########################


#### initiate requested libraries ####
library(data.table)
library(lubridate)
library(plyr)
library(Rblpapi)
library(xts)
library(xlsx)


#### set working directory ####
# codeDir <- "/home/artha/R-Projects/DharmaResearchSelection/"
# 
# workDir   <- "/home/artha/R-Projects/DharmaResearchSelection/"
# setwd(workDir)



#############################
########  Functions  ########
#############################

### load Functions ###
source("R/Source/loadNewTickers.R")
source("R/Source/getBdpData.R")
source("R/Source/getBdhData.R")
source("R/Source/calculatedFields.R")
source("R/Source/calcRating.R")
source("R/Source/getStats.R")
source("R/Source/calcGrade.R")
source("R/Source/calcScoring.R")
source("R/Source/changeNAtoZero.R")


#########################
########  Datas  ########
#########################

### load datas

# load indicators database
load("TidyData/indicators.RData")
setkey(indicators, Ticker)

# load exisiting security database
load("TidyData/securities.RData")
setkey(securities, Ticker)

# load new Tickers
newTic <- loadNewTickers("newTickers.csv")

# load fields
indicatorFields    <- fread("RawData/indicatorsFields.csv", sep=",")
indicatorFields    <- indicatorFields$Field

newFields <- fread("RawData/newFields.csv", sep=",")
newFields <- newFields$Field

tickerFields <- fread("RawData/tickersFields.csv", sep=",")
tickerFields <- tickerFields$Field


###############################
########  MAin Script  ########
###############################

### start Main process ###

# update Database
source(paste0(codeDir, "R/updateDatabase.R"))

# compute ratings
source(paste0(codeDir, "R/computeRatings.R"))


#######################
########  End  ########
#######################

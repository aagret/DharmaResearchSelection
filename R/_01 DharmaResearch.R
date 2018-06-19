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
codeDir <- "m:/Alexandre/R-Projects/DharmaResearchSelection/"
#codeDir <- "/home/artha/R-Projects/DharmaResearchSelection/"
# 
# workDir   <- "/home/artha/R-Projects/DharmaResearchSelection/"
# setwd(workDir)


#############################
########  Functions  ########
#############################

### load Functions ###
source("R/Source/getDates.R")
source("R/Source/isBlpOK.R")
source("R/Source/loadNewTickers.R")
source("R/Source/getBdpData.R")
source("R/Source/getBdhData.R")
source("R/Source/calculatedFields.R")
source("R/Source/calcRating.R")
source("R/Source/getStats.R")
source("R/Source/calcGrade.R")
source("R/Source/calcScoring.R")
source("R/Source/changeNAtoZero.R")
source("R/Source/getSummary.R")


#########################
########  Datas  ########
#########################

### load datas

# load indicators database
load("TidyData/indicators.RData")
setDT(indicators, key= c("Ticker", "Date"))

# load exisiting security database
load("TidyData/securities.RData")
setDT(securities, key= "Ticker")

database <- securities[indicators]


# load new Tickers
newTic <- loadNewTickers("newTickers.csv")

#load fields datas
indicatorFields <- read.csv("RawData/indicatorsFields.csv")

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

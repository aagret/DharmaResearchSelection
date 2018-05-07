
########################
################################################################################
############ MAIN SCRIPT
####


#### get Rating criterias
crit <- read.xlsx("RawData/criteriaLast.xlsx", sheetIndex = 1)
setDT(crit)
crit <- crit[complete.cases(crit)]

## calc indicators
rating <- calcRating(indicators, crit)

stats <- getStats(indicators) # rolling stats !!

grade <- calcGrade(indicators, crit, stats) # adjust to avoid rolling stats OR?

score <- calcScoring(indicators[securities], crit)


####
############ END OF SCRIPT
################################################################################
########################



########################
################################################################################
############ MAIN SCRIPT
####


#### get Rating criterias
crit <- read.xlsx("RawData/criteriaLast.xlsx", sheetIndex = 1)
setDT(crit)
crit <- crit[complete.cases(crit)]

## calc indicators
rating <- calcRating(database, crit)
setDT(rating, key=c("Ticker", "Date"))

# stats <- getStats(indicators) # rolling stats !!
# grade <- calcGrade(indicators, crit, stats) # adjust to avoid rolling stats OR?

scoring <- calcScoring(database, crit)

# get style
style <- getStyle(database)


####
############ END OF SCRIPT
################################################################################
########################



# ### TEST
# 
# db <- melt(style, id.vars = c("Date","Ticker"), measure.vars = c("Growth","Value","Momentum"))
# 
# g <- ggplot(data= db[Ticker== "AAPL US Equity" & variable %in% c("Growth", "Value"),]) + 
#     aes(x= Date, y= value, color= variable) +
#     geom_line()
# 
# 
# 
# 
# + 
#     
#     
#     geom_line(aes(y= NewReturn, color="new")) +
#     geom_line(aes(y= IdxReturn, color="idx")) +
#     facet_wrap(~Group, scales = "free_y")

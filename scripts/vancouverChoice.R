# VancouverChoice.R
# R to analyze discrete choices of developers, maybe aggregate
# Tom Davidoff
# Feb 5, 2026

library(data.table)
library(ggplot2)
CUTDISTANCE <- 20 # laneay

# Choices data
dtChoice <- readRDS("~/OneDrive - UBC/dataProcessed/vancouverPermitLotsTracts.rds")
print(head(dtChoice))
dtChoice[,year:=year(permitnumbercreateddate)]
dtChoice <- dtChoice[!is.na(landWidth)]
dtPriceCT <- fread("~/OneDrive - UBC/dataProcessed/bca19_mean_ppsf_slope_by_tract.csv",colClasses=c(CTUID="character"))
setnames(dtPriceCT,c("CTUID","priceCT","nobsCT","elasticityCT"))
dtChoice <- merge(dtChoice,dtPriceCT,by="CTUID")
dtChoice[,landWidth:=as.numeric(landWidth)]
dtPriceND <- fread("~/OneDrive - UBC/dataProcessed/bca19_mean_ppsf_slope_by_neighbourhood.csv")
setnames(dtPriceND,c("neighbourhoodDescription","priceND","nobsND","elasticityND"))
a <- unique(dtChoice[,neighbourhoodDescription])
dtChoice <- merge(dtChoice,dtPriceND,by="neighbourhoodDescription")
b <- unique(dtChoice[,neighbourhoodDescription])
print(setdiff(a,b))

# Income by census tract
dtIncome <- fread("~/OneDrive - UBC/dataRaw/9810005801_databaseLoadingData.csv",
  select = c("GEO", "DGUID", "Household income statistics (6)", "VALUE")
)
dtIncome <- dtIncome[grepl("Vancouver", GEO)]
setnames(dtIncome, c("DGUID", "Household income statistics (6)", "VALUE"), c("DGUID", "incomeStat", "medianIncome"))
dtIncome <- dtIncome[incomeStat == "Median household total income (2020) (2020 constant dollars)"]
dtIncome[, tract := substr(DGUID, 13, 19)]

dtChoice[,tract:=substring(CTUID,4,10)]
a <- nrow(dtChoice)
dtChoice <- merge(dtChoice,dtIncome,by="tract")
print(c("LOSS OF OBS?",a - nrow(dtChoice)))
dtChoice <- dtChoice[use!="laneway"] # not laneway only
dtChoice <- dtChoice[!is.na(elasticityCT) & !is.na(elasticityND)]
dtSingle <-dtChoice[use=="single"]
dtSingle[,laneway:=distToLaneway<CUTDISTANCE]
print(cor(dtSingle[,.(laneway,medianIncome,elasticityCT,elasticityND,priceCT,priceND)]))
print(dtSingle[order(landWidth),mean(laneway),by=floor(landWidth)]) # not a ton of action
rr <- feols(laneway ~ medianIncome + priceCT + elasticityND + landWidth| year,data=dtSingle)
print(summary(rr)) # just price and width
rr <- lm(laneway ~ priceCT + landWidth + factor(year) ,data=dtSingle)
keep <- complete.cases(model.frame(rr))
dtSingle <- dtSingle[keep]
dtSingle[,eps:=resid(rr)]
dtLanewayResidual <- dtSingle[,resid:=mean(resid,na.rm=TRUE),by=rr]

dtDuplex

dfd
dfdfd

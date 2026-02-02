# wtpPrivate.R
# Willingness to pay for privacy by census tract based on laneway prevalence
# Tom Davidoff 
# 02/02/26

# model: mean willingness to pay for .25 or .16 of laneway at mean PPSF exceeds privacy willingness to pay. Pick mean and common standard deviation of normal distribution that best fits shares

library(data.table)
library(ggplot2)

# permit and price data
dtPrice <- fread("~/OneDrive - UBC/dataProcessed/bca19_mean_ppsf_slope_by_tract.csv",colClasses=list(character=c("CTUID")))
dtChoice <- readRDS("~/OneDrive - UBC/dataProcessed/vancouverPermitLotsTracts.rds")
print(head(dtChoice))

dtMerge <- merge(dtChoice,dtPrice,by="CTUID")
dtMerge[,tract:=substr(CTUID,4,10)]
print(table(dtMerge[,.(maxSingle,maxLaneway)]))

dtMerge[,noLaneway:=ifelse(use!="laneway",1,0)]
print(table(dtMerge[,.(maxSingle,noLaneway)]))
dtMerge <- dtMerge[use=="laneway" | maxSingle==1]
print(table(dtMerge[,.(maxSingle,noLaneway)]))
print(head(dtMerge))
print(summary(dtMerge))



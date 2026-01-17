# main.vancouver_permits_
# main analysis script
# Tom Davidoff
# 01/16/26

library(data.table)
library(ggplot2)

# Vancouver analysis
# get slopes and mean values from BCA sales analysis by census tract merge
nameSlope <- "tables/bca19_mean_ppsf_slope_by_tract.csv"
if (!file.exists(nameSlope)) {
  source("scripts/bcaSales.R")
}
# specify column CTUID is a number
dtSlope <- fread(nameSlope,colClasses=list(character="CTUID"))

# Now get projects merged with BCA and census tracts
nameSpatial <- "~/OneDrive - UBC/dataProcessed/vancouverPermitLotsTracts.rds"
if (!file.exists(nameSpatial)) {
  source("scripts/mergePermitsSpatial.R")
}
dtSpatial <- readRDS(nameSpatial)

print(head(dtSpatial))
print(head(dtSlope))

dtMerge <- merge(dtSpatial,dtSlope,by="CTUID")
print(head(dtMerge))
print(summary(dtMerge))
print(table(dtMerge[,use]))

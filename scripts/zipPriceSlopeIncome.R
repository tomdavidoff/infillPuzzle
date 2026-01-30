# zipPriceSlopeIncome.R
# R to merge income stats by whatever with per square foot price and slope 
# Tom Davidoff
# 01/29/26

# US Cities
library(data.table)
# 2 header rows, first row is variable names, second descriptions
dtCensus <- fread("~/OneDrive - UBC/dataRaw/ACSDT5Y2024/ACSDT5Y2024.B19013-Data.csv", skip=1,select = c("Estimate!!Median household income in the past 12 months (in 2024 inflation-adjusted dollars)","Geographic Area Name"),header=TRUE)
print(head(dtCensus))
setnames(dtCensus, old=c("Estimate!!Median household income in the past 12 months (in 2024 inflation-adjusted dollars)"), new=c("medianIncome"))
dtCensus[, medianIncome := log(as.numeric(medianIncome))]

dtPM <- readRDS("~/OneDrive - UBC/dataProcessed/minneapolis_attom_slopes_by_zip.rds")
dtPP <- readRDS("~/OneDrive - UBC/dataProcessed/portland_attom_slopes_by_zip.rds")

dtCensus[, zip := substr(`Geographic Area Name`, nchar(`Geographic Area Name`)-4, nchar(`Geographic Area Name`))]
print(head(dtPM))
print(head(dtPP))

dtMergeM <- merge(dtPM, dtCensus, by="zip")
print(head(dtMergeM))
dtMergeP <- merge(dtPP, dtCensus, by="zip")
print(head(dtMergeP))
print(cor(dtMergeM[, .(slope, mean_ppsf,medianIncome)], use="complete.obs"))
print(cor(dtMergeP[, .(slope, mean_ppsf,medianIncome)], use="complete.obs"))


# Vancouver
NEEDVAN <- 0
if (NEEDVAN==1) {
	library(cancensus)
	set_cancensus_api_key("davidoff")
	vancouver <- get_census(
	  dataset = "CA21",
	  region = "CMACA/59933",   # Vancouver CMA
	  vectors = c(
	    "v_CA21_906",   # Median total income of households
	    "v_CA21_2466"   # Median value of dwellings
	  ),
	  level = "DA",
	  geo_format = "sf",
	  use_cache = TRUE,
	  api_key = Sys.getenv("CM_API_KEY")
	)
	print(head(vancouver))
}
dtCan <- fread("~/OneDrive - UBC/dataRaw/9810005801_databaseLoadingData.csv",select=c("Household income statistics (6)","GEO","VALUE"))

setnames(dtCan,c("Household income statistics (6)","GEO","VALUE"),c("namer","CT","medianIncome"))
dtCan <- dtCan[ namer == "Median household total income (2020) (2020 constant dollars)"]
dtCan[,tract:=substr(CT,1,6)]
dtCan <- dtCan[grepl("Vancouver",CT)]
print(head(dtCan))
dtCan[,nTract:=as.numeric(tract)]

dtVanSlope <- fread("tables/bca19_mean_ppsf_slope_by_tract.csv",colClasses = c("CTUID"="character"))
dtVanSlope[, tract := substr(CTUID, 4, 10)]
print(head(dtVanSlope))
dtVanSlope[,nTract:=as.numeric(tract)]

dtMergeV <- merge(dtVanSlope, dtCan, by="nTract")
print(head(dtMergeV))
print(cor(dtMergeV[, .(slope, meanPPSF,medianIncome)], use="complete.obs"))

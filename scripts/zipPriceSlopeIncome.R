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

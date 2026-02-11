# bcaSales.R
# R to use 2019 sales to get census-tract level coefficients on duplex vs single family and also square footage coefficients on ppsf
# Tom Davidoff
# 01/14/26

# strategy:
# Get 2026 roll centroids, merge on roll/1000 (for subdivisions), confirm coverage for 2019 sales data (through 2018)
# merge with census tract centroids
# Regress coefficients, etc.

library(data.table)
library(sf)
library(RSQLite)
library(fixest)
library(ggplot2)
library(scales)
library(stringr)
library(readxl)
library(geosphere)
library(nnet)

# per google
latDowntown <- 49.2827
lonDowntown <- -123.1207
WIDTHTOL <- .10
MAXAGE <- 25
MINOBS <- 20

centroidsFile <- "~/OneDrive - UBC/dataProcessed/bca25Centroids.rds"
print(centroidsFile)
print(file.exists(centroidsFile))
if (!file.exists(centroidsFile)) {
  source("scripts/getBCAVancouverCentroids.R")
}
dtCentroids <- readRDS(centroidsFile)
# add to data.table

dtInventory <- fread("~/OneDrive\ -\ UBC/Documents/data/bca/Residential_inventory_202501/20250101_A09_Residential_Inventory_Extract.txt",select=c("Roll_Number","MB_Total_Finished_Area","Zoning","Land_Width_Width","Land_Depth_Depth","MB_Effective_Year"),colClasses=c(Roll_Number="character",MB_Total_Finished_Area="numeric",Zoning="character",Land_Width_Width="numeric",Land_Depth_Depth="numeric"))
print(head(dtInventory))
dtSales <- fread("~/OneDrive\ -\ UBC/Documents/data/bca/data_advice_REVD25_20250331/bca_folio_sales_20250331_REVD25.csv",select=c("ROLL_NUMBER","FOLIO_ID","CONVEYANCE_DATE","CONVEYANCE_PRICE","CONVEYANCE_TYPE_DESCRIPTION"))
dtSales <- dtSales[CONVEYANCE_TYPE_DESCRIPTION=="Improved Single Property Transaction"]
dtSales[,saleYear:=as.numeric(substring(CONVEYANCE_DATE,1,4))]
print(head(dtSales))
MINYEAR <- 2012
MAXYEAR <- 2018
dtSales <- dtSales[saleYear>=MINYEAR & saleYear<=MAXYEAR]
dtDescription <- fread("~/OneDrive\ -\ UBC/Documents/data/bca/data_advice_REVD25_20250331/bca_folio_descriptions_20250331_REVD25.csv",select=c("FOLIO_ID","ROLL_NUMBER","ACTUAL_USE_DESCRIPTION","NEIGHBOURHOOD","JURISDICTION_CODE"))
dtDescription <- dtDescription[JURISDICTION_CODE==200]
dtMerge <- merge(dtDescription,dtCentroids,by="ROLL_NUMBER")
dtMerge <- merge(dtMerge,dtInventory,by.x="ROLL_NUMBER",by.y="Roll_Number")
print(nrow(dtMerge))
dtMerge <- merge(dtMerge,dtSales,by="ROLL_NUMBER")
# order by frequency
print(table(dtMerge[,ACTUAL_USE_DESCRIPTION])[order(table(dtMerge[,ACTUAL_USE_DESCRIPTION]),decreasing=TRUE)])
dtMerge <- dtMerge[ACTUAL_USE_DESCRIPTION %in% c("Strata-Lot Residence (Condominium)", "Residential Dwelling with Suite", "Single Family Dwelling", "Row Housing (Single Unit Ownership)") | grepl("Duplex",ACTUAL_USE_DESCRIPTION)]
# first question: among single family homes are neighbourhoods relevant?
dtMerge[,age:=saleYear-MB_Effective_Year]
# citywide optimal square feet?

dtMerge[,distDowntown:=distGeo(cbind(lon,lat),c(lonDowntown,latDowntown))]
regValue1 <- feols(log(CONVEYANCE_PRICE) ~ log(MB_Total_Finished_Area) + log(age+1) +lon*distDowntown|saleYear^ACTUAL_USE_DESCRIPTION,data=dtMerge )
regValue2 <- feols(log(CONVEYANCE_PRICE) ~ log(MB_Total_Finished_Area) + log(age+1) +lon*distDowntown + i(NEIGHBOURHOOD)|saleYear^ACTUAL_USE_DESCRIPTION,data=dtMerge )
# add census tract merge from Lon/Lat
ct_file <- "~/OneDrive - UBC/dataRaw/lct_000b21a_e/lct_000b21a_e.shp"
sCT <- st_read(ct_file, quiet = TRUE)
sCT <- st_make_valid(sCT)

# turn lon/lat into a spatial point, then merge with census tract geometry as a spatial merge
# plain vanilla crs lat lon
sMerge <- st_as_sf(dtMerge, coords = c("lon", "lat"),crs=4326)
sMerge <- st_transform(sMerge, crs = st_crs(sCT))
sMerge <- st_join(sMerge, sCT, left = TRUE)
# reconvert to better crs and get lon/lat familiarly
sMerge <- st_transform(sMerge, crs = 4326)
sMerge$lon <- st_coordinates(sMerge)[,1]
sMerge$lat <- st_coordinates(sMerge)[,2]
df <- as.data.table(sMerge)
print(nrow(df))
print(nrow(df))
regValue3 <- feols(log(CONVEYANCE_PRICE) ~ log(MB_Total_Finished_Area) + log(age+1) +lon*distDowntown |saleYear^ACTUAL_USE_DESCRIPTION + CTNAME,data=df )
regValue4 <- feols(log(CONVEYANCE_PRICE) ~ log(MB_Total_Finished_Area) + log(age+1) |saleYear^ACTUAL_USE_DESCRIPTION + CTNAME,data=df )
# print etable with adjusted r2
print(etable(regValue1,regValue2,regValue3,regValue4,fitstat="ar2"))
df[,propertyType:=ifelse(ACTUAL_USE_DESCRIPTION %chin% c("Single Family Dwelling","Residential Dwelling with Suite"),"single",ifelse(grepl("Duplex,",ACTUAL_USE_DESCRIPTION),"duplex",ifelse(ACTUAL_USE_DESCRIPTION=="Row Housing (Single Unit Ownership)","TH","condo")))]
df <- df[propertyType!="single" | Zoning=="R1-1"]
df[,single:=propertyType=="single"]
df[,ppsf:=CONVEYANCE_PRICE/MB_Total_Finished_Area]
df <- df[!is.na(ppsf)]
EXTREME_VAL <- .005
minVal <- quantile(df[,ppsf], EXTREME_VAL)
maxVal <- quantile(df[,ppsf], 1-EXTREME_VAL)
df <- df[ppsf %between% c(minVal,maxVal)]
print(summary(df))
# turn the below into a regression
df[,roundSQFT:=round(MB_Total_Finished_Area/200)]
MINSQFT <- .25*.8*33*122
MAXSQFT <- 1.5*.8*33*122
MINWIDTH <- 30
MAXWIDTH <- 35 # over half of properties
df <- df[Land_Width_Width %between% c(MINWIDTH,MAXWIDTH)]
df <- df[MB_Total_Finished_Area %between% c(MINSQFT,MAXSQFT)]
df[,sfk:=MB_Total_Finished_Area/1000]
df <- df[propertyType %chin% c("condo","TH")==FALSE] # apples to apples!
ONTARIOLON <- -123.105 # google
df[,east:=lon>ONTARIOLON]
# functional form
# Automate some of this code
library(data.table)
library(fixest)

setDT(df)
df[, single := propertyType == "single"]


setDT(df)

# precompute raw polynomial terms as plain numeric columns
df[, `:=`(
  sfk1 = sfk,
  sfk2 = sfk^2,
  sfk3 = sfk^3,
  single = propertyType == "single"
)]

gvars <- c("east", "NEIGHBOURHOOD", "CTNAME")
fes   <- "saleYear + age + CTNAME"

run_triplet <- function(dt, y, x, gvars, fes) {
  sapply(gvars, function(g) {
    fml <- as.formula(sprintf("%s ~ (%s)*i(%s) | %s", y, x, g, fes))
    fitstat(feols(fml, data = dt), "war2")
  })
}

specs <- list(
  cubic  = list(dt = df[age < MAXAGE], y = "ppsf",      x = "sfk1 + sfk2 + sfk3"),
  loglog = list(dt = df[age < MAXAGE], y = "log(ppsf)", x = "log(sfk)"),
  linear = list(dt = df[age < MAXAGE], y = "ppsf",      x = "sfk"),
  single = list(dt = df[age < 20],     y = "ppsf",      x = "single")
)

war2_mat <- do.call(rbind, lapply(specs, function(s)
  run_triplet(s$dt, s$y, s$x, gvars, fes)
))
df[,lsqft := log(MB_Total_Finished_Area)]
regT <- feols(log(ppsf) ~ 0+ lsqft*i(CTNAME) | saleYear + age +CTNAME, data=df[age<MAXAGE])
regN <- feols(log(CONVEYANCE_PRICE) ~ 0+ lsqft*i(NEIGHBOURHOOD) | saleYear + age +NEIGHBOURHOOD, data=df[age<MAXAGE])
regL <- feols(log(CONVEYANCE_PRICE) ~ 0 + lsqft*lon | saleYear + age,data=df[age<MAXAGE])
print(summary(regT))
print(summary(regN))
print(summary(regL))
#lsqft:NEIGHBOURHOOD::Renfrew Heights  
# turn coefficients of this form into a data.table
df <- df[NEIGHBOURHOOD!="Shaughnessy"]
dtInteractionsNeighbourhood <- data.table(neighbourhood=character(),interactionLSF=numeric(),medianPPSF=numeric())
interceptLSF <- coef(regN)["lsqft"]
for (n in unique(df[,NEIGHBOURHOOD])) {
	medianPPSF <- median(df[NEIGHBOURHOOD==n & propertyType=="single" & age<MAXAGE,ppsf])
	x <- data.table(neighbourhood=n,interactionLSF=coef(regN)[paste0("lsqft:NEIGHBOURHOOD::",n)][1]+interceptLSF,medianPPSF=medianPPSF)
	dtInteractionsNeighbourhood <- rbind(dtInteractionsNeighbourhood, x)
}
fwrite(dtInteractionsNeighbourhood,file="~/OneDrive - UBC/dataProcessed/neighbourhoodInteractionsVancouver.csv")
q("no")

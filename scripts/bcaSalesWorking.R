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
print(head(dtDescription))
print(nrow(dtDescription))
print(head(dtCentroids))
dtMerge <- merge(dtDescription,dtCentroids,by="ROLL_NUMBER")
print(nrow(dtMerge))
dtMerge <- merge(dtMerge,dtInventory,by.x="ROLL_NUMBER",by.y="Roll_Number")
print("postInventory")
print(nrow(dtMerge))
dtMerge <- merge(dtMerge,dtSales,by="ROLL_NUMBER")
print(nrow(dtSales))
print(nrow(dtMerge))
# order by frequency
print(table(dtMerge[,ACTUAL_USE_DESCRIPTION])[order(table(dtMerge[,ACTUAL_USE_DESCRIPTION]),decreasing=TRUE)])
dtMerge <- dtMerge[ACTUAL_USE_DESCRIPTION %in% c("Strata-Lot Residence (Condominium)", "Residential Dwelling with Suite", "Single Family Dwelling", "Row Housing (Single Unit Ownership)") | grepl("Duplex",ACTUAL_USE_DESCRIPTION)]
# first question: among single family homes are neighbourhoods relevant?
dtMerge[,age:=saleYear-MB_Effective_Year]

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

# conclusion: ct
df[,ppsf:=CONVEYANCE_PRICE/MB_Total_Finished_Area]
df <- df[!is.na(ppsf)]
EXTREME_VAL <- .01
minVal <- quantile(df[,ppsf], EXTREME_VAL)
maxVal <- quantile(df[,ppsf], 1-EXTREME_VAL)
df <- df[ppsf %between% c(minVal,maxVal)]
df[,multi:=propertyType %chin% c("TH","condo")]

# get single premium and mean ppsf
dfMean <- df[multi==0,.(meanPPSF=mean(ppsf),medianPPSF=median(ppsf)),by=CTNAME]
df[,localWidth:=median(Land_Width_Width,na.rm=TRUE),by=CTNAME]
df <- df[,abs(Land_Width_Width-localWidth)<(WIDTHTOL*localWidth)]
df[,meanPPSF:=mean(ppsf),by=CTNAME]
df[,lppsf:=log(ppsf)]
df[,lsqft:=log(MB_Total_Finished_Area)]
minSqft <- quantile(df[,MB_Total_Finished_Area], EXTREME_VAL)
maxSqft <- quantile(df[,MB_Total_Finished_Area], 1-EXTREME_VAL)
df <- df[MB_Total_Finished_Area %between% c(minSqft,maxSqft)]
df <- df[age<MAXAGE]

regSingle <- feols(lppsf ~ 0 + i(NEIGHBOURHOOD,single) + log(age+1) + log(Land_Width_Width)|saleYear ,data=df[multi==0])
regSqft <- feols(lppsf ~ 0 + i(NEIGHBOURHOOD,lsqft) + log(age+1) + log(Land_Width_Width)|saleYear ,data=df[multi==0])
regUnited <- feols(lppsf ~ 0 + i(NEIGHBOURHOOD,single) + i(NEIGHBOURHOOD,lsqft) + log(age+1) + log(Land_Width_Width)|saleYear ,data=df[multi==0])
# create a data table that has the interactions on single and on lsqft by neighbourhood from regOut coefficients, can't use df column names
dfInteractions <- data.table(NEIGHBOURHOOD=character(),interactionSingle=numeric(),interactionlSqft=numeric())
for (n in unique(df[,NEIGHBOURHOOD])) {
  x <- data.table(NEIGHBOURHOOD=n, interactionSingle=coef(regSingle)[paste0("NEIGHBOURHOOD::",n,":single")], interactionlSqft=coef(regSqft)[paste0("NEIGHBOURHOOD::",n,":lsqft")])
  dfInteractions <- rbind(dfInteractions, x)
}
# interactionlSqft most sensible
print(dfInteractions)
# Note more sensible results from separate regressions?
fwrite(dfInteractions,file="~/OneDrive - UBC/dataProcessed/neighbourhoodInteractionsVancouver.csv")
fwrite(dfMean, file="~/OneDrive - UBC/dataProcessed/tractMeansVancouver.csv")

regSqftTract <- feols(lppsf ~ 0 + i(CTNAME,lsqft) + log(age+1) + log(Land_Width_Width)|saleYear ,data=df[multi==0])
dfInteractionsTract <- data.table(CTNAME=character(),interactionSqft=numeric(),nobs=numeric())
for (n in unique(df[,CTNAME])) {
  x <- data.table(CTNAME=n, interactionSqft=coef(regSqftTract)[paste0("CTNAME::",n,":lsqft")], nobs=nrow(df[CTNAME==n & multi==0]))
  dfInteractionsTract <- rbind(dfInteractionsTract, x)
}
print(dfInteractionsTract)
fwrite(dfInteractionsTract,file="~/OneDrive - UBC/dataProcessed/tractInteractionsVancouver.csv")
print((df[,median(Land_Width_Width,na.rm=TRUE),by=propertyType]))

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
regEast <- feols(ppsf ~ i(roundSQFT) | saleYear + age + CTNAME, data=df[east==1 & age<MAXAGE])
regEastPoly <- feols(ppsf ~ sfk + I(sfk^2) + I(sfk^3) | saleYear + age + CTNAME, data=df[east==0& age<MAXAGE])
regWest <- feols(ppsf ~ i(roundSQFT) | saleYear + age + CTNAME, data=df[east==0 & age<MAXAGE])
regWestPoly <- feols(ppsf ~ sfk + I(sfk^2) + I(sfk^3) | saleYear + age + CTNAME, data=df[east==0& age<MAXAGE])
etable(regEast,regEastPoly)
etable(regWest,regWestPoly)
# functional form
regEW <- feols(ppsf ~ (sfk + I(sfk^2) + I(sfk^3))*i(east) | saleYear + age +CTNAME, data=df[age<MAXAGE])
regN <- feols(ppsf ~ (sfk + I(sfk^2) + I(sfk^3))*i(NEIGHBOURHOOD) | saleYear + age +CTNAME, data=df[age<MAXAGE])
regT <- feols(ppsf ~ (sfk + I(sfk^2) + I(sfk^3))*i(CTNAME) | saleYear + age +CTNAME, data=df[age<MAXAGE])
print(c(fitstat(regEW,"war2"),fitstat(regN,"war2"),fitstat(regT,"war2")))
regEW <- feols(ppsf ~ (sfk + I(sfk^2))*i(east) | saleYear + age +CTNAME, data=df[age<MAXAGE])
regN <- feols(ppsf ~ (sfk + I(sfk^2) )*i(NEIGHBOURHOOD) | saleYear + age +CTNAME, data=df[age<MAXAGE])
regT <- feols(ppsf ~ (sfk + I(sfk^2) )*i(CTNAME) | saleYear + age +CTNAME, data=df[age<MAXAGE])
print(c(fitstat(regEW,"war2"),fitstat(regN,"war2"),fitstat(regT,"war2")))
regEW <- feols(log(ppsf) ~ log(sfk)*i(east) | saleYear + age +CTNAME, data=df[age<MAXAGE])
regN <- feols(log(ppsf) ~ log(sfk )*i(NEIGHBOURHOOD) | saleYear + age +CTNAME, data=df[age<MAXAGE])
regT <- feols(log(ppsf) ~ log(sfk)*i(CTNAME) | saleYear + age +CTNAME, data=df[age<MAXAGE])
print(c(fitstat(regEW,"war2"),fitstat(regN,"war2"),fitstat(regT,"war2")))
regEW <- feols(ppsf ~ (sfk)*i(east) | saleYear + age +CTNAME, data=df[age<MAXAGE])
regN <- feols(ppsf ~ (sfk )*i(NEIGHBOURHOOD) | saleYear + age +CTNAME, data=df[age<MAXAGE])
regT <- feols(ppsf ~ (sfk)*i(CTNAME) | saleYear + age +CTNAME, data=df[age<MAXAGE])
print(c(fitstat(regEW,"war2"),fitstat(regN,"war2"),fitstat(regT,"war2")))
df[,single:=propertyType=="single"]
regEW <- feols(ppsf ~ single*i(east) | saleYear + age +CTNAME, data=df[age<20])
regN <- feols(ppsf ~ single*i(NEIGHBOURHOOD) | saleYear + age +CTNAME, data=df[age<20])
regT <- feols(ppsf ~ single*i(CTNAME) | saleYear + age +CTNAME, data=df[age<20])
print(c(fitstat(regEW,"war2"),fitstat(regN,"war2"),fitstat(regT,"war2")))
nBy <- df[age<20 & propertyType %in% c("single","duplex"),.(ner=.N),by=CTNAME]
df <- merge(df,nBy,by="CTNAME")
df <- df[ner>MINOBS] # only keep census tracts with more than 10 single/duplex sales
print(table(nBy$ner))
# winner is 3rd order polynomial by census tract
# now for each CT, find a fitted value for age 0 to 3, and sqft 1/4*33*122, 1/2*.7*33*122, 1*.7*33*122, 1/3*1*33*122
# (find modal age between 0 and 5)
winAGE <- -1
winN <- 0
for (a in seq(0,5)) {
  nAge <- nrow(df[age==a ])
  if (nAge>winN) {
	  winAGE <- a
	  winN <- nAge
  }
}
regT <- feols(ppsf ~ (sfk + I(sfk^2) + I(sfk^3))*i(CTNAME) | saleYear + age +CTNAME, data=df[age<MAXAGE])
dtFit <- data.table(CTNAME=character(),sqft=numeric(),fittedPPSF=numeric())
print(coef(regT))
for (ct in unique(df$CTNAME)) {
	# confirm ct is in regression
	  if (is.na(coef(regT)[paste0("I(I(sfk^2)):CTNAME::",ct)])) {
	    next
	  }
	for (sqft in c(.25,.35,.7)) {
		newDt <- data.table(sfk=sqft*33*122/1000,age=winAGE,saleYear=2018,CTNAME=ct)
		fittedPPSF <- predict(regT,newdata=newDt)
		dtFit <- rbind(dtFit,data.table(CTNAME=ct,sqft=sqft,fittedPPSF=fittedPPSF))
	}
}
print(dtFit)
print(summary(dtFit)) # note absurd
regT <- feols(ppsf ~ (sfk + I(sfk^2))*i(CTNAME) | saleYear + age +CTNAME, data=df[age<MAXAGE])
dtFit <- data.table(CTNAME=character(),sqft=numeric(),fittedPPSF=numeric())
print(coef(regT))
for (ct in unique(df$CTNAME)) {
	# confirm ct is in regression
	  if (is.na(coef(regT)[paste0("I(I(sfk^2)):CTNAME::",ct)])) {
	    next
	  }
	for (sqft in c(.25,.35,.7)) {
		newDt <- data.table(sfk=sqft*33*122/1000,age=winAGE,saleYear=2018,CTNAME=ct)
		fittedPPSF <- predict(regT,newdata=newDt)
		dtFit <- rbind(dtFit,data.table(CTNAME=ct,sqft=sqft,fittedPPSF=fittedPPSF))
	}
}
print(dtFit)
print(summary(dtFit)) # note absurd
regT <- feols(log(ppsf) ~ log(MB_Total_Finished_Area)*i(CTNAME) | saleYear + age +CTNAME, data=df[age<MAXAGE])
dtFit <- data.table(CTNAME=character(),sqft=numeric(),fittedPPSF=numeric(),elasticity=numeric(),medianPPSF=numeric())
print(coef(regT))
for (ct in unique(df$CTNAME)) {
	# confirm ct is in regression
	  if (is.na(coef(regT)[paste0("log(MB_Total_Finished_Area):CTNAME::",ct)])) {
	    next
	  }
	for (sqft in c(.25,.35,.7)) {
		newDt <- data.table(MB_Total_Finished_Area=sqft*33*122,age=winAGE,saleYear=2018,CTNAME=ct)
		fittedPPSF <- predict(regT,newdata=newDt)
		medianPPSF <- df[CTNAME==ct & age<MAXAGE & propertyType=="single",mean(ppsf,na.rm=TRUE)]
		elasticity <- coef(regT)[paste0("log(MB_Total_Finished_Area):CTNAME::",ct)]
		dtFit <- rbind(dtFit,data.table(CTNAME=ct,sqft=sqft,fittedPPSF=fittedPPSF,elasticity=elasticity,medianPPSF=medianPPSF))
	}
}
print(dtFit)
print(summary(dtFit)) # note absurd
print(summary(df[age<MAXAGE & propertyType=="single",ppsf]))
print(winAGE)
fwrite(dtFit,file="~/OneDrive - UBC/dataProcessed/tractInteractionsVancouver.csv")

q("no")
for (y in seq(2012,2018)) {
	print(c(y,mean(df[saleYear==y,mean(ppsf,na.rm=TRUE)])))
}
for (k in seq(400,4000,100)) {
	print(c(k,mean(df[saleYear>2016 & round(MB_Total_Finished_Area/100)==k/100,mean(ppsf,na.rm=TRUE)])))
}
print(mean(df[saleYear>2016,mean(ppsf,na.rm=TRUE),by=ACTUAL_USE_DESCRIPTION]))
q("no")

# conclusion: ct
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
print(summary(regSqftTract))
regPriceTract <- feols(log(CONVEYANCE_PRICE) ~ 0 + i(CTNAME,log(MB_Total_Finished_Area)) + log(age+1) + log(Land_Width_Width)|saleYear ,data=df[multi==0])
dfInteractionsTract <- data.table(CTNAME=character(),interactionSqft=numeric(),nobs=numeric())
for (n in unique(df[,CTNAME])) {
  x <- data.table(CTNAME=n, interactionSqft=coef(regSqftTract)[paste0("CTNAME::",n,":lsqft")], nobs=nrow(df[CTNAME==n & multi==0]))
  dfInteractionsTract <- rbind(dfInteractionsTract, x)
}
print(dfInteractionsTract)
fwrite(dfInteractionsTract,file="~/OneDrive - UBC/dataProcessed/tractInteractionsVancouver.csv")
print((df[,median(Land_Width_Width,na.rm=TRUE),by=propertyType]))

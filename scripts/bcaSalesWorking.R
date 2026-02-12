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
library(xtable)

# per google
latDowntown <- 49.2827
lonDowntown <- -123.1207
ONTARIOLON <- -123.105 # google
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
MINSQFT <- 400
df[,propertyType:=ifelse(ACTUAL_USE_DESCRIPTION %chin% c("Single Family Dwelling","Residential Dwelling with Suite"),"single",ifelse(grepl("Duplex,",ACTUAL_USE_DESCRIPTION),"duplex",ifelse(ACTUAL_USE_DESCRIPTION=="Row Housing (Single Unit Ownership)","TH","condo")))]
df[,lsqft := log(MB_Total_Finished_Area)]
df <- df[!is.na(lsqft) & !is.na(CONVEYANCE_PRICE) & !is.na(age) & !is.na(lon) & !is.na(lat) & !is.na(saleYear) & !is.na(CTNAME) & CONVEYANCE_PRICE>0 & age>=0 & MB_Total_Finished_Area>MINSQFT]
MINWIDTH <- 30
MAXWIDTH <- 35 # over half of properties
df <- df[Land_Width_Width %between% c(MINWIDTH,MAXWIDTH)]
df[,east:=lon>ONTARIOLON]
df[,single:=propertyType=="single"]

ancientYears <- c(2002,2004)
oldYears <- c(2012,2014)
modYears <- c(2017,2018)
newYears <- c(2022,2024)
residPlot <- function(yearList) {
	dmap <- df[propertyType %in% c("single","duplex") & age<MAXAGE & saleYear %in% yearList]
	rr <- feols(log(CONVEYANCE_PRICE) ~ lsqft | saleYear + age ,  data=dmap)
	dmap[,rrResidual := resid(rr)]
	ggplot(dmap,aes(x=lon,y=lat,color=rrResidual)) + geom_point() + scale_color_gradient2(low="grey80",high="grey10",limits = c(-1,1),
    oob = scales::squish) + theme_minimal() + labs(color="Residual") + geom_vline(xintercept=ONTARIOLON, linetype="dashed", color="black") + theme(legend.position = "bottom") 
	ggsave(paste0("text/residualsMap",yearList[1],"to",yearList[2],".png"))
}
lapply(list(ancientYears,oldYears,modYears,newYears), residPlot)

print(etable(feols(log(CONVEYANCE_PRICE) ~ lsqft + i(propertyType)*east | saleYear + age,data=df[age<MAXAGE])))

df <- df[saleYear>2016 & saleYear<2020,]

r0 <- feols(log(CONVEYANCE_PRICE) ~ lsqft |saleYear+age,data=df[age<MAXAGE])
rD <- feols(log(CONVEYANCE_PRICE) ~ lsqft + lon*east |saleYear+age+east,data=df[age<MAXAGE])
rL <- feols(log(CONVEYANCE_PRICE) ~ lsqft + lon|saleYear+age,data=df[age<MAXAGE])
rLN <- feols(log(CONVEYANCE_PRICE) ~ lsqft |saleYear+age+NEIGHBOURHOOD,data=df[age<MAXAGE])
rLT <- feols(log(CONVEYANCE_PRICE) ~ lsqft |saleYear+age+CTNAME,data=df[age<MAXAGE])
# create a latex table of adjusted r2 for these regressions with brief descriptive names
baseRegTable <- data.table(
  Specification = c("Baseline", "East Dummy", "Longitude ", "Neighbourhood FE", "Census Tract FE"),
  Adjusted_R2 = sapply(list(r0,rD, rL, rLN, rLT), function(reg) round(as.numeric(fitstat(reg, "ar2")),2))
)
print(xtable(baseRegTable),file="text/vancouverRegressionComparisonTable.tex",include.rownames=FALSE, floating=FALSE)

r0 <- feols(log(CONVEYANCE_PRICE) ~ lsqft |saleYear+age,data=df[age<MAXAGE])
rD <- feols(log(CONVEYANCE_PRICE) ~ lsqft*east|saleYear+age ,data=df[age<MAXAGE])
rL <- feols(log(CONVEYANCE_PRICE) ~ lsqft*lon|saleYear+age ,data=df[age<MAXAGE])
rLN <- feols(log(CONVEYANCE_PRICE) ~ lsqft*NEIGHBOURHOOD |saleYear+age,data=df[age<MAXAGE])
rLT <- feols(log(CONVEYANCE_PRICE) ~ lsqft*CTNAME |saleYear+age,data=df[age<MAXAGE])
interRegTable <- data.table(
  Specification = c("Baseline", "East Dummy Interaction", "Longitude Interaction", "Neighbourhood Interaction", "Census Tract Interaction"),
  Adjusted_R2 = sapply(list(r0,rD, rL, rLN, rLT), function(reg) round(as.numeric(fitstat(reg, "ar2")),2))
)
print(xtable(interRegTable),file="text/vancouverInteractionRegressionComparisonTable.tex",include.rownames=FALSE, floating=FALSE)

rD0 <- feols(log(CONVEYANCE_PRICE) ~ lsqft + east|saleYear+age,data=df[age<MAXAGE])
rL0 <- feols(log(CONVEYANCE_PRICE) ~ lsqft + lon|saleYear+age,data=df[age<MAXAGE])

rD <- feols(log(CONVEYANCE_PRICE) ~ lsqft*east|saleYear+age,data=df[age<MAXAGE])
rDR <- feols(log(CONVEYANCE_PRICE) ~ lsqft+ single*east|saleYear+age,data=df[age<MAXAGE])
print(etable(rD0,rL0,rD,rL,rDR,tex=TRUE))

df[,ppsf:=CONVEYANCE_PRICE/MB_Total_Finished_Area]
rDL <- feols(log(ppsf) ~ lsqft + east|saleYear+age,data=df[age<MAXAGE])
rD <- feols(log(ppsf) ~ lsqft*east|saleYear+age,data=df[age<MAXAGE])
print(etable(rDL,rD,tex=TRUE,file="text/vancouverEastPPSF.tex"))
WIDTH <- 33
DEPTH <- 110
Laneway <- WIDTH*DEPTH*.16
Duplex <- WIDTH*DEPTH*.7/2
Single <- WIDTH*DEPTH*.7
plex4 <- WIDTH*DEPTH*1/4
sqft <- c(Laneway,Single,Duplex,plex4)
print(summary(df[,.(lsqft,CONVEYANCE_PRICE,age,saleYear,east)]))
playData <- data.table(lsqft =rep(log(sqft),2),east=c(rep("FALSE",length(sqft)),rep("TRUE",length(sqft))),age=rep(0,2*length(sqft)),saleYear=rep(2019,2*length(sqft)))
playData[,pred := predict(rD, newdata=playData)]
playData[,fittedPPSF := round(exp(pred),2)]
playData[,type:=rep(c("laneway","single","duplex","quadplex"),2)]
playData[,sqft:=rep(sqft,2)]
playData[,units:=rep(c(1,1,2,4),2)]
playData[,totalPrice:=round(fittedPPSF*sqft*units,2)]
print(xtable(playData[,.(type,east,sqft,fittedPPSF,totalPrice)]), file="text/vancouverFittedValues.tex",include.rownames=FALSE,floating=FALSE)


q("no")

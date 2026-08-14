# edmontonMatch.R
# Match building permits to assessment
# Tom Davidoff
# 07/16/26

library(data.table)
library(fixest)
library(sf)
library(duckdb)
library(ggplot2)

assessorHistoricalPath <- "~/DropboxExternal/dataRaw/edmonton/Property_Assessment_Data_(Historical)_20260611.csv"
assessorCurrentPath <- "~/DropboxExternal/dataRaw/edmonton/Property_Information_(Current_Calendar_Year)_20260612.csv"

dtAH <- fread(assessorHistoricalPath,select=c("Lot Size","Account Number","Zoning","Legal Description","Latitude","Longitude","Assessment Year","Assessed Value"))
dtAC <- fread(assessorCurrentPath,select = c("lot_size","zoning","legal_description","Latitude","Longitude","Account Number","year_built","Total Gross Area"))
dtAC <- dtAC[zoning=="RS"]
setkey(dtAH, "Account Number")
setkey(dtAC, "Account Number")
dtAM <- dtAH[dtAC]

# permit match
permitPath <- "~/DropboxExternal/dataRaw/edmonton/edmontonBuildingPermits.csv"
con <- dbConnect(duckdb())
q <- sprintf(R"(
  SELECT
    "PERMIT_DATE", "PERMIT_NUMBER", "YEAR", "BUILDING_TYPE", "WORK_TYPE",
    "CONSTRUCTION_VALUE", "FLOOR_AREA", "UNITS_ADDED", "ADDRESS",
    "LEGAL_DESCRIPTION", "ZONING", "NEIGHBOURHOOD", "LATITUDE", "LONGITUDE"
  FROM read_csv_auto('%s')
  WHERE ZONING = 'RS'
)", permitPath)
dfP <- dbGetQuery(con,q)
dbDisconnect(con, shutdown = TRUE)
dtP <- as.data.table(dfP)
dtP[,isRowHouse := grepl("Row House",BUILDING_TYPE)]
dtP[,isDetached := grepl("Single Detached",BUILDING_TYPE)]

# only new construction or construction value above critical percentile of new building types
QCRIT <- .25
qNew <- dtP[isRowHouse | isDetached,quantile(CONSTRUCTION_VALUE/FLOOR_AREA,probs=c(QCRIT),na.rm=TRUE)]
dtP <- dtP[isRowHouse | isDetached | (CONSTRUCTION_VALUE/FLOOR_AREA)>=qNew]

dtAM[,LEGAL_DESCRIPTION := gsub("\\s+", " ", `Legal Description`)]
dtAM[,LEGAL_DESCRIPTION := gsub("Plan:", "Plan", LEGAL_DESCRIPTION)]
dtAM[,LEGAL_DESCRIPTION := gsub("Block:", "Blk", LEGAL_DESCRIPTION)]
dtAM[,LEGAL_DESCRIPTION := gsub("Lot:", "Lot", LEGAL_DESCRIPTION)]
dtAM[,maxYear:=max(`Assessment Year`),by=`Account Number`]
dtAM <- dtAM[`Assessment Year`==maxYear] # lot size current==historical for ~all accounts. FORMALIZE THIS

dtPM <- merge(dtP, dtAM, by="LEGAL_DESCRIPTION",all.x=TRUE, all.y=FALSE)
dtPM <- dtPM[isRowHouse | isDetached]

# Now merge with census tracts to get shares of different projects by tract, elasticities, and mean price/rent
dfT <- st_read("~/DropboxExternal/dataRaw/lct_000b21a_e/lct_000b21a_e.shp")
dfT <- st_transform(dfT, crs = 4326)
dfT <- st_make_valid(dfT)

# use LATITUDE and LONGITUDE from permit data to get census tract for each permit by making a sf object and doing a spatial join
dfP <- st_as_sf(dtP[!is.na(LONGITUDE)], coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
dfP <- st_make_valid(dfP)
dfP <- st_join(dfP, dfT, join = st_within)
dtP <- as.data.table(dfP)
# get fraction that is detached vs row house by tract
dtShare <- dtP[YEAR>2023,.(N=.N, NDetached=sum(isDetached), NRowHouse=sum(isRowHouse)), by=.(CTNAME)]

# same tract merge for the current-year assessor data
dfAM <- st_as_sf(dtAM[!is.na(Longitude)], coords = c("Longitude", "Latitude"), crs = 4326)
dfAM <- st_make_valid(dfAM)
dfAM <- st_join(dfAM, dfT, join = st_within)
dtAM <- as.data.table(dfAM)
dtAM[,assessed:=gsub("\\$","",`Assessed Value`)]
dtAM[,assessed:=as.numeric(gsub(",","",assessed))]

regHedonic <- feols(log(as.numeric(assessed)) ~ log(as.numeric(lot_size)) + log(`Total Gross Area`) | CTNAME, data=dtAM)
dtHedonic <- as.data.table(fixef(regHedonic), keep.rownames = TRUE)
names(dtHedonic) <- c("CTNAME","FE")

# elasticity by tract: cov(log(`Total Gross Area`), log(assessed)) / var(log(`Total Gross Area`))
dtAM[,lotNumeric := as.numeric(lot_size)]
dtAM <- dtAM[!is.na(lotNumeric) & !is.na(`Total Gross Area`) & !is.na(assessed) & lotNumeric>0 & `Total Gross Area`>0 & assessed>0]
dtCoef <- dtAM[,.(Elasticity=cov(log(`Total Gross Area`), log(assessed))/var(log(`Total Gross Area`))), by=.(CTNAME)]
setkey(dtCoef, "CTNAME")
setkey(dtHedonic, "CTNAME")
dtCoef <- merge(dtCoef, dtHedonic, by="CTNAME", all.x=TRUE, all.y=FALSE)
print(cor(dtCoef[,Elasticity], dtCoef[,FE], use="complete.obs"))
ggplot(dtCoef, aes(x=Elasticity, y=FE)) + geom_point() + geom_smooth(method="lm") + theme_bw() + labs(title="Elasticity vs Fixed Effect by Census Tract", x="Elasticity", y="Fixed Effect")
ggsave("text/edmontonElasticityFE.png")

setkey(dtShare, "CTNAME")
dtCoefShare <- dtCoef[dtShare, nomatch=0]
print(cor(dtCoefShare[,.(Elasticity, FE, NRowHouse/(NDetached+NRowHouse))], use="complete.obs"))
ggplot(dtCoefShare, aes(x=Elasticity, y=NRowHouse/(NDetached+NRowHouse))) + geom_point() + geom_smooth(method="lm") + theme_bw() + labs(title="Elasticity vs Share of Row Houses by Census Tract", x="Elasticity", y="Share of Row Houses")
ggsave("text/edmontonElasticityShare.png")
ggplot(dtCoefShare, aes(x=FE, y=NRowHouse/(NDetached+NRowHouse))) + geom_point() + geom_smooth(method="lm") + theme_bw() + labs(title="Fixed Effect vs Share of Row Houses by Census Tract", x="Fixed Effect", y="Share of Row Houses")
ggsave("text/edmontonFEShare.png")
q("no")

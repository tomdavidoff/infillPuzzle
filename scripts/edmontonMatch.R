# edmontonMatch.R
# Match building permits to assessment
# Strategy: do 2026 and late 2025 permits so old address still valid maybe
# Tom Davidoff
# 07/16/26

library(data.table)
library(fixest)
library(sf)
library(duckdb)
library(ggplot2)

# Compare assessor historical and current data to verify that current lot size is always historical lot size
assessorHistoricalPath <- "~/DropboxExternal/dataRaw/edmonton/Property_Assessment_Data_(Historical)_20260611.csv"
assessorCurrentPath <- "~/DropboxExternal/dataRaw/edmonton/Property_Information_(Current_Calendar_Year)_20260612.csv"

dtAH <- fread(assessorHistoricalPath,select=c("Lot Size","Account Number","Zoning","Legal Description","Latitude","Longitude","Assessment Year","Assessed Value"))
dtAC <- fread(assessorCurrentPath,select = c("lot_size","zoning","legal_description","Latitude","Longitude","Account Number","year_built","Total Gross Area"))
dtAC <- dtAC[zoning=="RS"]
setkey(dtAH, "Account Number")
setkey(dtAC, "Account Number")
dtAM <- dtAH[dtAC]
print(head(dtAM))
print(summary(dtAM[,as.numeric(lot_size)==as.numeric(`Lot Size`)]))
print(summary(dtAM[,legal_description==`Legal Description`]))
print(dtAM[legal_description!=`Legal Description`,.(legal_description,`Legal Description`)])
print(dtAM[as.numeric(lot_size)!=as.numeric(`Lot Size`),.(lot_size,`Lot Size`)])
print(dtAM[as.numeric(lot_size)!=as.numeric(`Lot Size`),summary(as.numeric(lot_size)-as.numeric(`Lot Size`))])
print(dtAM[as.numeric(lot_size)!=as.numeric(`Lot Size`),quantile(abs(as.numeric(lot_size)-as.numeric(`Lot Size`)),probs=c(.9,.95,.975,.99),na.rm=TRUE)])
print(dtAM[as.numeric(lot_size)!=as.numeric(`Lot Size`) & legal_description!=`Legal Description`,quantile(abs(as.numeric(lot_size)-as.numeric(`Lot Size`)),probs=c(.9,.95,.975,.99),na.rm=TRUE)])
print(dtAM[as.numeric(lot_size)!=as.numeric(`Lot Size`) & legal_description!=`Legal Description`,quantile((as.numeric(lot_size)/as.numeric(`Lot Size`)),probs=c(.001,.01,.05,.95,.99,.999),na.rm=TRUE)])

# now permit match quality
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
dtAM[,LEGAL_DESCRIPTION := gsub("\\s+", " ", `Legal Description`)]
dtAM[,LEGAL_DESCRIPTION := gsub("Plan:", "Plan", LEGAL_DESCRIPTION)]
dtAM[,LEGAL_DESCRIPTION := gsub("Block:", "Blk", LEGAL_DESCRIPTION)]
dtAM[,LEGAL_DESCRIPTION := gsub("Lot:", "Lot", LEGAL_DESCRIPTION)]
dtAM[,minYear:=min(`Assessment Year`),by=`Account Number`]
dtAM[,maxYear:=max(`Assessment Year`),by=`Account Number`]
print(summary(dtAM[,.(minYear,maxYear)]))
dtAM <- dtAM[`Assessment Year`==maxYear] # pretty much all good for lot size per the above analysis. FORMALIZE THIS

dtPM <- merge(dtP, dtAM, by="LEGAL_DESCRIPTION",all.x=TRUE, all.y=FALSE)
dtPM[,hasAccount := !is.na(`Account Number`) & `Account Number`!=""]
print(dtPM[,.(mean(hasAccount),.N),by=WORK_TYPE])
print(dtPM[,table(BUILDING_TYPE)])
dtPM <- dtPM[isRowHouse | isDetached]
print(dtPM[,summary(as.numeric(lot_size)),by=.(isRowHouse)])


dtR <- dtPM[grepl("Row House",BUILDING_TYPE)]

print(dtR[YEAR>=2022 & grepl("(01)",WORK_TYPE),mean(year_built>=YEAR,na.rm=TRUE),by=YEAR])
print(dtPM[YEAR>=2022 & grepl("(01)",WORK_TYPE),mean(year_built>=YEAR,na.rm=TRUE),by=YEAR])
print(head(dtR))
print(dtR[year_built>YEAR,summary(abs(as.numeric(lot_size)-as.numeric(`Lot Size`)),na.rm=TRUE)])
# reverse merge: can we find a permit for properties built starting 2023
dtAMR <- dtAM[year_built>=2023]
dtAMR <- merge(dtAMR, dtP, by="LEGAL_DESCRIPTION",all.x=TRUE, all.y=FALSE)
dtAMR[,hasPermit:=!is.na(YEAR)]
print(dtAMR[,table(hasPermit)])

# Now merge with census tracts to get shares of different projects by tract, elasticities, and mean price/rent
dfT <- st_read("~/DropboxExternal/dataRaw/lct_000b21a_e/lct_000b21a_e.shp")
print(head(dfT))

print(summary(dtPM))
# use LATITUDE and LONGITUDE from permit data to get census tract for each permit by making a sf object and doing a spatial join
dfP <- st_as_sf(dtP[!is.na(LONGITUDE)], coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
dfT <- st_transform(dfT, crs = 4326)
# Error in wk_handle.wk_wkb(wkb, s2_geography_writer(oriented = oriented,  : Loop 0 is not valid: Edge 383 has duplicate vertex with edge 841 fix this
dfP <- st_make_valid(dfP)
dfT <- st_make_valid(dfT)

dfP <- st_join(dfP, dfT, join = st_within)
dtP <- as.data.table(dfP)
# get fraction that is detached vs row house by tract
dtShare <- dtP[,.(N=.N, NDetached=sum(isDetached), NRowHouse=sum(isRowHouse)), by=.(CTNAME)]
print(dtShare)

# make a similar tract merge for the current-year assessor data, but first filter to only RS properties
dfAM <- st_as_sf(dtAM[!is.na(Longitude)], coords = c("Longitude", "Latitude"), crs = 4326)
dfAM <- st_make_valid(dfAM)
dfAM <- st_join(dfAM, dfT, join = st_within)
dtAM <- as.data.table(dfAM)
print(head(dtAM))
dtAM[,assessed:=gsub("\\$","",`Assessed Value`)]
print(head(dtAM[,assessed]))
dtAM[,assessed:=as.numeric(gsub(",","",assessed))]
print(head(dtAM[,assessed]))
regHedonic <- feols(log(as.numeric(assessed)) ~ log(as.numeric(lot_size)) + log(`Total Gross Area`) | CTNAME, data=dtAM)
# data table of fixed effects DGUID with the associated DGUID value
dtHedonic <- as.data.table(fixef(regHedonic), keep.rownames = TRUE)
print(head(dtHedonic))
names(dtHedonic) <- c("CTNAME","FE")
print(summary(regHedonic))

regElasticity <- feols(log(as.numeric(assessed)) ~ log(`Total Gross Area`)*i(CTNAME) | CTNAME, data=dtAM)
# get interaction terms
dtCoef <- as.data.table(coef(regElasticity), keep.rownames = TRUE)
print(sum(!is.na(dtCoef$CTNAME)))
print(summary(dtCoef))
# do differently, just cov(log(`Total Gross Area`), log(as.numeric(assessed))) / var(log(`Total Gross Area`)) by CTNAME
dtAM[,lotNumeric := as.numeric(lot_size)]
dtAM[,LotNumeric := as.numeric(`Lot Size`)]
dtAM <- dtAM[!is.na(lotNumeric) & !is.na(`Total Gross Area`) & !is.na(assessed) & lotNumeric>0 & `Total Gross Area`>0 & assessed>0]
dtCoef <- dtAM[,.(Elasticity=cov(log(`Total Gross Area`), log(assessed))/var(log(`Total Gross Area`))), by=.(CTNAME)]
dtCheck <- merge(dtAM,dtCoef,by="CTNAME",all.x=TRUE,all.y=TRUE)
print(dtCheck[is.na(Elasticity)])
print(summary(dtCoef))
print(summary(dtAM))
print(dtCoef)
print(dtCoef[!is.na(Elasticity),.N])
setkey(dtCoef, "CTNAME")
setkey(dtHedonic, "CTNAME")
dtCoef <- merge(dtCoef, dtHedonic, by="CTNAME", all.x=TRUE, all.y=FALSE)
print(dtCoef)
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


q <- sprintf("SELECT \"Account Number\", \"Assessed Value\", \"Zoning\", \"Legal Description\",\"Latitude\", \"Longitude\", \"Lot Size\", \"Neighbourhood\"
  FROM read_csv_auto('%s')
  QUALIFY bool_or(Zoning = 'RS') OVER (PARTITION BY \"Account Number\")", assessorPath)
dfA <- dbGetQuery(con,q)
dtA <- as.data.table(dfA)
 
# convert below to duckdb query
print(head(dtP))
print(head(dtA))
dtA[,LEGAL_DESCRIPTION := gsub("\\s+", " ", `Legal Description`)]
dtA[,LEGAL_DESCRIPTION := gsub("Plan:", "Plan", LEGAL_DESCRIPTION)]
dtA[,LEGAL_DESCRIPTION := gsub("Block:", "Blk", LEGAL_DESCRIPTION)]
dtA[,LEGAL_DESCRIPTION := gsub("Lot:", "Lot", LEGAL_DESCRIPTION)]
dtM <- merge(dtP, dtA, by="LEGAL_DESCRIPTION",all.x=TRUE, all.y=FALSE)
print(dtM[,fullMerge := ifelse(is.na(`Account Number`), ifelse(is.na(PERMIT_NUMBER), "No match", "Partial match"), "Full match")])

q("no")
#dfP <- pl$scan_csv(path.expand("~/DropboxExternal/dataRaw/edmonton/edmontonBuildingPermits.csv"))$
select("PERMIT_DATE", "PERMIT_NUMBER", "YEAR", "BUILDING_TYPE", "WORK_TYPE", "CONSTRUCTION_VALUE", "FLOOR_AREA", "UNITS_ADDED",
	"ADDRESS", "LEGAL_DESCRIPTION", "ZONING", "NEIGHBOURHOOD",
	"LATITUDE", "LONGITUDE")$
  filter(pl$col("ZONING")== "RS")
dtP  <- fread("~/DropboxExternal/dataRaw/edmonton/edmontonBuildingPermits.csv")
print(head(dtP))



q("no")
dtA <- fread("~/DropboxExternal/dataRaw/edmonton/Property_Assessment_Data_(Historical)_20260611.csv" , select=c("Account Number", "Zoning", "Assessed Value",
		 "Latitude", "Longitude", "Lot Size", "Neighbourhood","maxRS"))
# limit to current RS zones
dtA[,maxRS := max(Zoning=="RS"), by=`Account Number`]
dtA <- dtA[maxRS==1]
q("no")
print(summary(result))
# result$to_data_frame()  # if you want a base R data.frame for fixest/sf
result <- dfA$collect())   # wall tim
dfA$explain()                          # confirm the plan / pushdowns
q("no")



# convert the polars python code to R polars, let's use scan_csv instead of read to do manipulations if possible in R implementation

dfA <- pl$scan_csv("~/DropboxExternal/dataRaw/edmonton/Property_Assessment_Data_(Historical)_20260611.csv",infer_schema_length=0)
# now create maxRS and confine to current RS zones (maxRS==1)
dfA <- dfA$with_columns((pl$col("Zoning")=="RS")$any()$over("Account Number")$alias("maxRS"))
dfA <- dfA$filter(pl$col("maxRS")==1)
# now do evaluation of lazy?



print(dfA.head())
# print a table of maxRS by Zoning



#library(data.table)
#library(fixest)
#library(sf)

#import polars as pl
#import sys

# convert next 3 lines to polars
dfA = pl.read_csv("~/DropboxExternal/dataRaw/edmonton/Property_Assessment_Data_(Historical)_20260611.csv",infer_schema_length=0)
dfC = pl.read_csv("~/DropboxExternal/dataRaw/edmonton/Property_Information_(Current_Calendar_Year)_20260612.csv",infer_schema_length=0)
dfP = pl.read_csv("~/DropboxExternal/dataRaw/edmonton/edmontonBuildingPermits.csv",infer_schema_length=0)


# limit to current RS zones
dfA = dfA.with_columns((pl.col("Zoning")=="RS").any().over("Account Number").alias("maxRS"))
print(dfA.head())
# print a table of maxRS by Zoning
vs_table = dfA.pivot_count(on="maxRS", index="Zoning")
print(vs_table)
sys.exit()
dfdfd


dtA[,table(maxRS==1,Zoning)]
dtC <- fread("~/DropboxExternal/dataRaw/edmonton/Property_Information_(Current_Calendar_Year)_20260612.csv")


# header rows
#==> /Users/tdavidof/DropboxExternal/dataRaw/edmonton/edmontonBuildingPermits.csv <==
#Row ID,PERMIT_DATE,PERMIT_NUMBER,REPORT_PERMIT_DATE,YEAR,MONTH_NUMBER,JOB_CATEGORY,JOB_DESCRIPTION,BUILDING_TYPE,WORK_TYPE,CONSTRUCTION_VALUE,FLOOR_AREA,UNITS_ADDED,ADDRESS,LEGAL_DESCRIPTION,ZONING,NEIGHBOURHOOD_NUMBER,NEIGHBOURHOOD,BIA,COUNT,LATITUDE,LONGITUDE,LOCATION,Geometry Point,Occupancy Date
#1-253374239,2019-07-04,,2019-07-04,2019,7,Commercial Final,To construct an LRT Station (Davies Station) on City Road Right-of-Way,Transportation Terminals (440),(01) New,20150000,39815,0,100 - DAVIES LRT STATION NW,"","",6170,DAVIES INDUSTRIAL WEST,"",1,,,"","",
#
#==> /Users/tdavidof/DropboxExternal/dataRaw/edmonton/Property_Assessment_Data_(Historical)_20260611.csv <==
#"Account Number","Assessment Year","Suite","House Number","Street Name","Legal Description","Latitude","Longitude","Point Location","Neighbourhood","Actual Year Built","Garage","Zoning","Lot Size","Assessed Value","Assessment Class 1","Assessment Class % 1","Assessment Class 2","Assessment Class % 2","Assessment Class 3","Assessment Class % 3"
#"1477850","2013",,"10821","150 STREET NW","Plan: 704KS  Block: 68  Lot: 5","53.5533","-113.57936","POINT (-113.57936 53.5533)","HIGH PARK","1955","Y","RF1","681.1240","$269,500.00","RESIDENTIAL","100",,,,
#
#==> /Users/tdavidof/DropboxExternal/dataRaw/edmonton/Property_Information_(Current_Calendar_Year)_20260612.csv <==
#"Account Number","Suite","House Number","Street Name","legal_description","zoning","lot_size","Total Gross Area","year_built","garage","Neighbourhood ID","Neighbourhood","Ward","Latitude","Longitude","Point Location"
#"11219634",,"303","29 STREET SW","Plan: 2322574  Block: 11  Lot: 42","RSF","335.364","183.4","2024","true","6669","ALCES","Sspomitapi","53.4300755348","-113.38586836927","POINT (-113.38586836927416 53.43007553480231)"

## first question: Suites in RS 2025+ completions
#print(head(dtC))
dtCs <- dtC[zoning=="RS" & year_built>=2025 ]
print(dtCs[,.(N=.N,hasSuite=sum(Suite!=""))])
# Answer: no suites, that's not how rowhouse for sale would be identified

# edmontonMatch_skeleton.R
# Structure for the three-file linkage. NOT runnable — stubs mark where
# real logic (canon(), knn match, demo chain, area-conservation) plugs in.
# Division of labour:
#   dtP (permits)      = CLASSIFIER  -> built form, units, suites
#   dtA (historical)   = PARENT      -> pre-redev lot_size (2023 vintage)
#   dtC (current year) = CHILD       -> new post-redev parcels (for assemblies)
#
# Column provenance (what each join produces):
#   builtForm, unitsAdded, suiteFlag   <- dtP        (never from roll)
#   parentLotSize, parentValue         <- dtA        via permit->parent join
#   n_parents, parent_area (assembly)  <- dtC<->dtA  child->parent join
#   builtForm MUST ride onto both the parent-lot number AND the assembly number

library(data.table); library(sf); library(nabor)

# ---------------------------------------------------------------------------
# 0. canonical legal + shared helpers  (apply IDENTICALLY to every file)
# ---------------------------------------------------------------------------
canon <- function(x) {
  x <- toupper(x)
  x <- gsub("BLOCK:?","BLK",x); x <- gsub("LOT:?","LOT",x)
  x <- gsub(":","",x); gsub("\\s+"," ",trimws(x))
}
proj <- function(dt) st_coordinates(                      # lon/lat -> UTM12N m
  st_transform(st_as_sf(dt, coords=c("lon","lat"), crs=4326), 26912))

# ---------------------------------------------------------------------------
# 1. CLASSIFY  — everything that decides "what was built" lives on the permit
# ---------------------------------------------------------------------------
# dtP[, builtForm  := classify(building_type, JOB_DESCRIPTION)]   # row/SF/semi/...
# dtP[, unitsAdded := UNITS_ADDED]
# dtP[, suiteFlag  := unitsAdded>=2 & grepl("SUITE|SECONDARY", toupper(JOB_DESCRIPTION))]
# dtP[, legalC := canon(legalDescription)]
# -> builtForm is the label that must survive to the final table on BOTH paths below.

# ---------------------------------------------------------------------------
# 2. PARENT side — historical roll, restricted to pre-redevelopment vintage
# ---------------------------------------------------------------------------
# dtA <- dtA[assessmentYear < 2024]
# dtA[, maxYear := max(assessmentYear), by=legalC]
# dtA <- dtA[assessmentYear == maxYear]        # DECISION: latest pre-2024, not hard ==2023
# dtA[, legalC := canon(legalDescription)]
#   carries: legalC, lot_size, assessedValue, lon/lat

# ---------------------------------------------------------------------------
# 3a. JOIN A  permit -> parent   (gets lot_size for the classified project)
#     Three tiers, because new legals often don't exist in the 2023 roll:
# ---------------------------------------------------------------------------
#   tier 1: legal match      dtP$legalC %in% dtA$legalC        (unsubdivided cases)
#   tier 2: demo chain       permit -> demo permit -> parent legal   (subdivided)
#   tier 3: spatial fallback knn(proj(dtA), proj(dtP), k=5) + 20m cap + sequencing
#   result: dtP gains parentLotSize, parentValue  — builtForm already on dtP. GOOD.

# ---------------------------------------------------------------------------
# 3b. JOIN B  child -> parent(s)   (assemblies: new dtC account -> dying dtA accts)
# ---------------------------------------------------------------------------
#   childNew <- dtC[account NOT in 2023 vintage]         # post-redev parcels
#   parents  <- dtA[account dies 2023->2025]
#   link via buffer spatial join + area-conservation (childArea / sum(parentArea)
#            within 0.90–1.10); n_parents, parent_area per child.
#   >>> CRITICAL: childNew has NO builtForm yet — it's a roll record. Attach the
#       permit's builtForm to the child BEFORE attributing an assembly, else the
#       assembly gets classified off roll fields (the bug you're fixing).
#   childNew <- merge(childNew, dtP[,.(builtForm, ...key...)], by=<addr/spatial>)

# ---------------------------------------------------------------------------
# 4. ASSEMBLE final table — builtForm from permit governs on both branches
# ---------------------------------------------------------------------------
# out <- rbind(
#   projectsFromJoinA[, .(projectId, builtForm, lotSize=parentLotSize, n_parents=1L)],
#   projectsFromJoinB[, .(projectId, builtForm, lotSize=parent_area,   n_parents)]
# )  # then dedup where a project appears on both paths, preferring assembly-aware area
